"""The variable-binding model: one place that decides lexical vs. dynamic.

CLHS 3.1.2.1.1 and 11.1.2.1.2: a form that establishes a binding for a variable
which is declared or proclaimed SPECIAL binds it **dynamically** -- in the
symbol's value cell, the same cell SYMBOL-VALUE / BOUNDP / SET / PROGV read and
write -- and that binding is undone when the form exits, however it exits.
Every other variable is bound **lexically**, in the establishing form's own
environment.

That one decision used to exist twice and be missing eight times.

* `eval_let` had it.
* `eval_letstar` had a drifted copy that was also wrong: for a special variable
  it called ``global_env.add_variable``, so the binding went into the *global*
  environment and was never undone.
* DO, DO*, DOLIST, DOTIMES, LOOP, DO-SYMBOLS, DO-EXTERNAL-SYMBOLS and
  DO-ALL-SYMBOLS did not have it at all. They established their variable with
  ``Environment.set_variable``, which *walks the environment chain and mutates
  the first binding of that name it finds*; because ``Environment.__init__``
  hands a child its parent's ``variable_bindings`` list, that walk always
  reached an enclosing binding. So an iteration form assigned to an enclosing
  variable instead of binding its own::

      (let ((x 99)) (dolist (x '(1 2 3))) x)   =>  NIL, not 99

  A wrong answer in its own right, and a measurement gate as well: rt.lsp's
  failure reporter takes its output stream in a parameter named ``s``, so a
  ``(do-all-symbols (s) ...)`` or ``(loop for s = ...)`` anywhere in the suite
  overwrote RT's own stream with a symbol and aborted the rest of the run.

Everything that binds a variable now goes through `BindingFrame`, so there is
one answer to "is this variable special here", one way to establish a binding,
one way to assign to it on the next iteration, and one unwind. plan.md's note
on M2 warns specifically against fixing specials one binding form at a time,
which is why this leak was diagnosed, measured and reverted rather than fixed
locally with a third copy of the decision.
"""

import fclpy.lisptype as lisptype
from .core import car, cdr, cons, _consp_internal


def _symbol_name(x):
    """The name of `x` if it is a symbol, else None."""
    return x.name if isinstance(x, lisptype.LispSymbol) else None


def split_declarations(body):
    """Split the leading ``(DECLARE ...)`` forms off `body`.

    CLHS 3.4.11: declarations appear only at the head of a body, so the first
    form that is not a DECLARE ends them. Returns ``(declare_forms, rest)``
    with `declare_forms` a Python list and `rest` the remaining Lisp body.
    The iteration forms need `rest`: they execute their body as a TAGBODY, and
    a declaration is not a statement.
    """
    declarations = []
    rest = body
    while _consp_internal(rest):
        first = car(rest)
        if _consp_internal(first) and _symbol_name(car(first)) == 'DECLARE':
            declarations.append(first)
            rest = cdr(rest)
        else:
            break
    return declarations, rest


def declared_specials(declarations):
    """The symbols named by ``(SPECIAL ...)`` specs in `declarations`."""
    specials = []
    seen = set()
    for decl in declarations:
        specs = cdr(decl)
        while _consp_internal(specs):
            spec = car(specs)
            if _consp_internal(spec) and _symbol_name(car(spec)) == 'SPECIAL':
                names = cdr(spec)
                while _consp_internal(names):
                    var = car(names)
                    if isinstance(var, lisptype.LispSymbol) and var.name not in seen:
                        seen.add(var.name)
                        specials.append(var)
                    names = cdr(names)
            specs = cdr(specs)
    return specials


def body_specials(body):
    """``(special_symbols, body_without_declarations)`` for a binding form."""
    declarations, rest = split_declarations(body)
    return declared_specials(declarations), rest


def root_environment(env):
    """The global environment at the root of `env`'s chain."""
    root = env
    while getattr(root, 'parent', None) is not None:
        root = root.parent
    return root


def proclaim_special(var, env):
    """Record that `var` is globally special, and return it.

    The one writer of the proclamation table `is_proclaimed_special` reads.
    DEFVAR, DEFPARAMETER, DECLAIM's and PROCLAIM's ``(SPECIAL ...)`` specs and
    the bootstrap's standard variables all go through here, so there is a
    single answer to "which names are globally special" -- the table that
    decides, in `BindingFrame.bind`, whether a later binding form binds in the
    value cell or in its own environment.
    """
    name = _symbol_name(var)
    if name is None:
        raise lisptype.LispNotImplementedError(
            f"cannot proclaim {var} special: a variable must be a symbol")
    root = root_environment(env)
    specials = getattr(root, '_special_variables', None)
    if specials is None:
        specials = {}
        root._special_variables = specials
    specials[name] = True
    return var


def is_proclaimed_special(var, env):
    """Has `var` been *proclaimed* special (DEFVAR/DEFPARAMETER/PROCLAIM)?

    Only the root environment is consulted, and that is the point. A
    proclamation is global and pervasive, so it makes every later binding of
    the variable dynamic; a `declaration` is local to the form it heads, and
    must **not** make a nested binding form bind dynamically. DOTIMES.17 and
    DOTIMES.18 are precisely that distinction::

        (let ((i 0) (y nil))          (let ((i 0) (y nil))
          (declare (special i))         (declare (special i))
          (flet ((%f () i))             (flet ((%f () i))
            (dotimes (i 4)                (dotimes (i 4)
              (push (%f) y)))               (declare (special i))
          y)                                (push (%f) y)))
        ;; => (0 0 0 0)                 y)
                                      ;; => (3 2 1 0)

    Left, the DOTIMES rebinds `i` lexically and `%f` still sees the enclosing
    dynamic binding; right, the DOTIMES' own declaration makes it rebind `i`
    dynamically and `%f` sees each iteration. Walking the environment chain
    here would collapse the two, since the enclosing LET's declaration is
    recorded on the environment it was evaluated in.
    """
    name = _symbol_name(var)
    if name is None:
        return False
    specials = getattr(root_environment(env), '_special_variables', None)
    return bool(specials) and name in specials


def dynamic_value(symbol, default=None):
    """The current value of a dynamic variable, read from Python.

    Resolution follows ``evaluation_core.eval``'s own order for a variable
    reference -- a binding in the current environment chain first, then the
    symbol's value cell -- so a binding a Lisp ``LET`` made is honoured by a
    builtin that reads the variable from Python. `default` is returned when
    the variable is unbound; Python ``None`` is the value cell's "unbound"
    marker, so it can never be a value here.

    This exists because every builtin that consults a control variable was
    writing the same four lines itself, and they had already drifted: LOAD's
    copy read ``*DEFAULT-PATHNAME-DEFAULTS*`` out of ``COMMON-LISP-USER``
    while COMPILE-FILE's read it out of ``COMMON-LISP``, and since global
    lookup is by symbol *identity* those are two different variables -- so
    the two operators resolved the same relative pathname differently.
    """
    import fclpy.state as state
    env = getattr(state, 'current_environment', None)
    if env is not None and env.has_variable(symbol):
        return env.find_variable(symbol)
    value = getattr(symbol, 'value', None)
    return default if value is None else value


def set_dynamic_value(symbol, value):
    """Assign to a dynamic variable's innermost existing binding.

    This is *assignment* (SETQ), not establishment: `Environment.set_variable`
    walks the chain and writes the first binding of the name it finds, ending
    at the value cell. That is exactly right here and exactly wrong for
    establishing a binding -- see `BindingFrame`, which is what a form that
    *binds* a variable must use.
    """
    import fclpy.state as state
    env = getattr(state, 'current_environment', None)
    if env is None:
        symbol.value = value
        return value
    return env.set_variable(symbol, value)


def special_reference(var):
    """The form a *free* SPECIAL declaration makes `var` expand to.

    ``%SPECIAL-REF`` reads the symbol's dynamic value cell when one has been
    established and otherwise falls back to an ordinary lexical lookup, so
    installing it as a symbol-macro redirects references without disturbing a
    variable that is only ever bound lexically. LOCALLY already worked this
    way; a binding form's free declarations now use the same mechanism.
    """
    return cons(lisptype.LispSymbol('%SPECIAL-REF'), cons(var, lisptype.NIL))


class BindingFrame:
    """The variable bindings one form establishes, and how they are undone.

    Construct it with the form's **own** new environment, optionally the form's
    body (so the declarations governing the bindings are read in one place),
    and the variables the form is about to bind. Then call `bind` once per
    variable per value: the first call for a variable establishes its binding,
    later calls assign to that same binding. That is what an iteration form's
    stepping needs, and it is what makes successive iterations share one
    binding, which DO.15 checks.

    Use it as a context manager -- or call `unwind` from a ``finally`` -- so a
    dynamic binding is undone even when the body exits non-locally.
    """

    def __init__(self, env, body=None, bound_vars=(), defer_free_declarations=False):
        self.env = env
        self._dynamic = {}      # name -> (symbol, had_value, old_value)
        self._lexical = {}      # name -> the Binding this frame established
        self._package_bound = False
        self._old_package = None

        specials = declared_specials(split_declarations(body)[0]) if body is not None else []
        self._special_names = {var.name for var in specials}

        bound_names = {name for name in map(_symbol_name, _flatten_vars(bound_vars))
                       if name is not None}
        self._free_specials = [var for var in specials if var.name not in bound_names]
        if not defer_free_declarations:
            self.install_free_declarations()

    def install_free_declarations(self):
        """Redirect *free* references to a variable this form declares special.

        A declaration governs free references to the variable inside the form
        (CLHS 3.3.4), including those in a DO step form and a DOLIST result
        form, both evaluated in this environment. DOLIST.17 and DO.17 are that
        case: the variable is declared special in the body but bound by an
        enclosing LET, so the reference must reach the value cell rather than
        the lexical binding that shadows it.

        **Init forms are not inside that scope**, which is why the timing is a
        parameter. DO evaluates its init forms in the *enclosing* environment,
        so they are unaffected wherever this runs; DO* evaluates each one in
        this environment, so installing before them would capture them too --
        and DO*.16 requires that it not, expecting the enclosing lexical
        binding rather than the declared-special one::

            (block done
              (let ((x :bad)) (declare (special x))
                (let ((x :good))
                  (do* ((i (return-from done x) 0)) (t nil)
                    (declare (special x))))))          ; => :good
        """
        for var in self._free_specials:
            self.env.add_symbol_macro(var, special_reference(var))
        self._free_specials = []

    def is_dynamic(self, var):
        """Would `bind` put `var` in its value cell rather than the environment?"""
        name = _symbol_name(var)
        if name is None:
            return False
        if name in self._dynamic:
            return True
        if name in self._lexical:
            return False
        return self._binds_dynamically(var, name)

    def _binds_dynamically(self, var, name):
        """Where a *new* binding for `name` goes: value cell, or environment.

        The global environment has no lexical variables (CLHS 3.1.1.1), so a
        frame over it can only bind dynamically -- otherwise `bind` would ask
        `add_variable` for a `Binding` object the global environment does not
        create, and the next iteration's assignment would have nothing to
        write through.
        """
        return (name in self._special_names
                or getattr(self.env, 'is_global', False)
                or is_proclaimed_special(var, self.env))

    def bind(self, var, value):
        """Establish `var`'s binding, or assign to the one this frame holds."""
        if not isinstance(var, lisptype.LispSymbol):
            raise lisptype.LispNotImplementedError(
                f"cannot bind {var}: a variable must be a symbol")
        name = var.name

        # Already bound by this frame: assign, never establish a second one.
        if name in self._dynamic:
            var.value = value
            self._mirror_package(var, value)
            return value
        if name in self._lexical:
            binding = self._lexical[name]
            binding.value = value
            # add_variable keeps this cache in step with the binding list, so
            # writing through the binding object has to keep it in step too.
            self.env._variable_map[name] = value
            self._mirror_package(var, value)
            return value

        if self._binds_dynamically(var, name):
            had_value = getattr(var, 'value', None) is not None
            self._dynamic[name] = (var, had_value, getattr(var, 'value', None))
            var.value = value
        else:
            # add_variable *prepends* to this environment's own binding list,
            # so the per-iteration assignment above -- and any SETQ in the body
            # -- reaches this binding instead of walking out to an enclosing
            # one of the same name.
            self.env.add_variable(var, value)
            self._lexical[name] = self.env.variable_bindings
        self._mirror_package(var, value)
        return value

    def _mirror_package(self, var, value):
        """`*PACKAGE*`'s value is mirrored in ``state.current_package``.

        Anything that binds it must update both or symbol interning silently
        goes to the wrong package -- and that holds on both paths here, because
        `*PACKAGE*` is not in the root environment's proclaimed-special table,
        so binding it currently takes the lexical one.
        """
        if var.name != '*PACKAGE*' or not isinstance(value, lisptype.Package):
            return
        import fclpy.state as state
        if not self._package_bound:
            self._package_bound = True
            self._old_package = getattr(state, 'current_package', None)
        state.current_package = value

    def unwind(self):
        """Undo every dynamic binding this frame established."""
        for var, had_value, old_value in self._dynamic.values():
            var.value = old_value if had_value else None
        self._dynamic.clear()
        if self._package_bound:
            import fclpy.state as state
            # Restore unconditionally. `eval_let` guarded this with
            # `if old_package is not None`, which conflates "nothing was saved"
            # with "None *is* the saved value" -- and None is the normal state
            # of the mirror until something binds `*PACKAGE*`, since a plain
            # reference falls back to a default. So the very first
            # `(let ((*package* p)) ...)` never restored, leaving
            # `state.current_package` set to `p` for the rest of the session and
            # silently interning every later symbol into the wrong package.
            state.current_package = self._old_package
            self._package_bound = False
            self._old_package = None

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self.unwind()
        return False


def _flatten_vars(varspec):
    """Every symbol in a variable spec, so a destructuring pattern counts too."""
    if isinstance(varspec, lisptype.LispSymbol):
        return [varspec]
    if _consp_internal(varspec):
        out = []
        cursor = varspec
        while _consp_internal(cursor):
            out.extend(_flatten_vars(car(cursor)))
            cursor = cdr(cursor)
        out.extend(_flatten_vars(cursor))
        return out
    if isinstance(varspec, (list, tuple)):
        out = []
        for item in varspec:
            out.extend(_flatten_vars(item))
        return out
    return []


__all__ = [
    'BindingFrame',
    'body_specials',
    'declared_specials',
    'is_proclaimed_special',
    'proclaim_special',
    'root_environment',
    'special_reference',
    'split_declarations',
]
