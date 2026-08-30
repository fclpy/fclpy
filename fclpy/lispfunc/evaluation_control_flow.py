"""Control flow special forms: BLOCK, CATCH, THROW, UNWIND-PROTECT, TAGBODY, GO."""

import fclpy.lisptype as lisptype
from .core import car, cdr, _consp_internal
from . import registry as _registry
from .evaluation_core import ReturnFromException, ThrowException, GoException


class BlockFrame:
    """The identity of one lexically established BLOCK (CLHS 3.1.2.1.2.1).

    RETURN-FROM is lexically scoped to the enclosing BLOCK *form*, not to the
    nearest runtime frame whose name happens to match: a closure defined
    inside a BLOCK returns to *that* block even when it is later called under
    a same-named one (BLOCK.10). So the target of a return is this frame
    object, which `eval_block` matches by identity, not the block's name.
    `active` is the frame's dynamic extent: once False, the block has been
    exited and returning to it is an error (CLHS 5.2).
    """

    __slots__ = ('name_key', 'active')

    def __init__(self, name_key):
        self.name_key = name_key
        self.active = True


class TagbodyFrame:
    """The identity of one lexically established TAGBODY, with its tags.

    GO's tag is lexically scoped to the enclosing TAGBODY *form* (CLHS
    5.3.3: the tags have lexical scope), so GO carries the frame it resolved
    to and `eval_tagbody` receives the jump only when the frame is its own.
    `tags` maps each tag key to the index of the tag in the form; `active`
    is the frame's dynamic extent.
    """

    __slots__ = ('tags', 'active')

    def __init__(self, tags):
        self.tags = tags
        self.active = True


def _block_name_key(name):
    """Canonical hashable key for a block name.

    NIL has three Python spellings here (None, the NIL singleton, and a
    LispSymbol named NIL) and all three name the same block, so the key
    folds them into one string the way the old three-way tag comparisons
    did. Other block names are keyed by their symbol's name, which is what
    the old name-matching compared.
    """
    if name is None or name is lisptype.NIL or name == lisptype.NIL:
        return 'NIL'
    if isinstance(name, lisptype.LispSymbol):
        return name.name
    return str(name)


def establish_block_frame(env, name):
    """Register a fresh BLOCK frame for `name` on `env`, and return it.

    `env` must be an environment created *for this block* (eval_block, the
    implicit-block runners and PPRINT-LOGICAL-BLOCK each make one), so at
    most one frame per name lives on any one environment: an outer
    same-named block's frame sits on its own environment further out on the
    lexical chain, and `find_block_frame` takes the innermost.
    """
    frame = BlockFrame(_block_name_key(name))
    frames = env.__dict__.get('block_frames')
    if frames is None:
        frames = {}
        env.block_frames = frames
    frames[frame.name_key] = frame
    return frame


def find_block_frame(env, name):
    """The innermost lexically visible BLOCK frame named `name`, or None.

    Walks the lexical environment chain, so a closure resolves the block its
    *definition* was nested in: the frame of a block evaluated later, under
    the call, sits on an environment that is not on the closure's chain and
    is never found.
    """
    key = _block_name_key(name)
    current = env
    while current is not None:
        frames = getattr(current, 'block_frames', None)
        if frames:
            frame = frames.get(key)
            if frame is not None:
                return frame
        current = current.parent
    return None


def establish_tagbody_frame(env, tags):
    """Register a fresh TAGBODY frame holding `tags` on `env`, and return it."""
    frame = TagbodyFrame(tags)
    env.tagbody_frame = frame
    return frame


def find_tagbody_frame(env, tag_key):
    """The innermost lexically visible TAGBODY frame containing `tag_key`.

    Skips frames that do not have the tag rather than stopping at the
    innermost frame, so `(tagbody a (tagbody (go a)))` finds the outer
    frame: the inner one is visible but does not contain the tag.
    """
    current = env
    while current is not None:
        frame = getattr(current, 'tagbody_frame', None)
        if frame is not None and tag_key in frame.tags:
            return frame
        current = current.parent
    return None


def deactivate_frame(frame):
    """End a block or tagbody frame's dynamic extent (its target has exited).

    The frame stays registered on its environment rather than being removed,
    so a later RETURN-FROM/GO whose lexical target *is* this frame is an
    extent error instead of silently retargeting an outer same-named frame.
    """
    frame.active = False


def eval_block(form, env):
    """Evaluate BLOCK special form: (BLOCK name body-form*)
    
    Establishes a block with the given name. Evaluates body forms in sequence.
    Can be exited early with RETURN-FROM.

    The block's identity is a `BlockFrame` registered on a fresh child
    environment, which the body evaluates in: a closure defined in the body
    closes over the chain that carries the frame, so it returns to *this*
    block even when called under a later, same-named one (BLOCK.10), while
    direct code inside a later same-named block resolves to that one.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("BLOCK requires at least a name")
    
    block_name = car(args)
    body_forms = cdr(args)
    
    # Block names must be symbols (including NIL, which is a symbol in Common Lisp)
    if not (isinstance(block_name, lisptype.LispSymbol) or block_name is None or block_name == lisptype.NIL):
        raise lisptype.LispNotImplementedError(f"BLOCK name must be a symbol, got {block_name}")
    
    # The frame lives on a child environment so it is lexically visible to
    # exactly the body forms and the closures they define.
    block_env = lisptype.Environment(env)
    frame = establish_block_frame(block_env, block_name)

    try:
        try:
            # Evaluate body forms in sequence
            result = lisptype.NIL
            current = body_forms
            while _consp_internal(current):
                result = eval(car(current), block_env)
                current = cdr(current)
            return result
        except ReturnFromException as e:
            # Only the frame *this* BLOCK established receives the transfer;
            # a same-named block (inner or outer) re-raises it.
            if e.block_frame is frame:
                return e.value
            raise
    finally:
        deactivate_frame(frame)


def eval_return_from(form, env):
    """Evaluate RETURN-FROM special form: (RETURN-FROM name value?)
    
    Exits the named BLOCK, returning the specified value (or NIL).

    The target is resolved *lexically* -- the innermost block named `name`
    on the environment chain, which is the one the form's text is nested in.
    Returning to a block that is not visible, or whose dynamic extent has
    ended, signals a CONTROL-ERROR (CLHS 5.2). CATCH/THROW is the opposite:
    it matches by name at runtime.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("RETURN-FROM requires at least a name")
    
    block_name = car(args)
    value_forms = cdr(args)
    
    # Block names must be symbols (including NIL, which is a symbol in Common Lisp)
    if not (isinstance(block_name, lisptype.LispSymbol) or block_name is None or block_name == lisptype.NIL):
        raise lisptype.LispNotImplementedError(f"RETURN-FROM name must be a symbol, got {block_name}")
    
    frame = find_block_frame(env, block_name)
    if frame is None or not frame.active:
        from .evaluation_conditions import signal_error_object
        return signal_error_object(lisptype.ControlError(
            message=("RETURN-FROM: no block named {} is visible".format(
                _block_name_key(block_name))
                     if frame is None else
                     "RETURN-FROM: the block named {} has already been exited".format(
                         _block_name_key(block_name)))))
    
    # Evaluate the value form (default to NIL)
    if _consp_internal(value_forms):
        value = eval(car(value_forms), env)
    else:
        value = lisptype.NIL
    
    # Raise exception to exit the block: `block_frame` is what the target
    # BLOCK matches on, by identity.
    raise ReturnFromException(block_name, value, frame)


@_registry.cl_macro('RETURN', documentation='RETURN macro: exits innermost NIL block')
def return_macro_expander(*args, **kwargs):
    """RETURN macro expander: converts (RETURN [value]) to (RETURN-FROM NIL [value])

    RETURN is equivalent to RETURN-FROM with the block name NIL. The
    macro function's signature accepts the raw args of the macro call
    plus, when invoked through the macro-expander path, a trailing
    environment arg (recognised via `__expects_environment__`).

    A direct call (no environment arg) is a PROGRAM-ERROR
    (`return.error.1`/`.2`/`.3`). The fall-back path in
    `evaluation_core` re-calls with fewer args on a Python
    `TypeError`, so the condition must be raised *inside* the macro
    function, not as a bare `TypeError` reaching a Lisp value.
    """
    if not args or not isinstance(args[-1], lisptype.Environment):
        # Direct call without an environment arg is a PROGRAM-ERROR.
        raise lisptype.LispProgramError(
            message=("RETURN: macro function must be called via the "
                     "macro-expander (form &environment env)"))
    # Real macro call: pop the trailing env arg. The remaining
    # `raw_args` are the unevaluated value-form (zero or one arg).
    raw_args = args[:-1]
    if len(raw_args) == 0:
        # `(RETURN)` -- (RETURN-FROM NIL)
        return lisptype.lispCons(
            lisptype.LispSymbol('RETURN-FROM'),
            lisptype.lispCons(lisptype.NIL, lisptype.NIL))
    if len(raw_args) == 1:
        # `(RETURN value)` -- (RETURN-FROM NIL value)
        return lisptype.lispCons(
            lisptype.LispSymbol('RETURN-FROM'),
            lisptype.lispCons(
                lisptype.NIL,
                lisptype.lispCons(raw_args[0], lisptype.NIL)))
    raise lisptype.LispProgramError(
        message="RETURN: too many arguments")
return_macro_expander.__expects_environment__ = True


def eval_catch(form, env):
    """Evaluate CATCH special form: (CATCH tag body-form*)
    
    Establishes a catch point. Evaluates body forms. If THROW is called
    with matching tag, catches it and returns the thrown value.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("CATCH requires a tag and optional body forms")
    
    tag_form = car(args)
    body_forms = cdr(args)
    
    # Evaluate the tag form
    tag = eval(tag_form, env)

    # Record the catcher as outstanding for the extent of the body, so THROW
    # can tell "no catcher for this tag" (a CONTROL-ERROR) from "a catcher
    # further out" (an ordinary transfer). See state.catch_tags.
    import fclpy.state as state
    state.catch_tags.append(tag)
    try:
        # Evaluate body forms in sequence
        result = lisptype.NIL
        current = body_forms
        while _consp_internal(current):
            result = eval(car(current), env)
            current = cdr(current)
        return result
    except ThrowException as e:
        if _tags_match(e.tag, tag):
            # Caught! Return the thrown value
            return e.value
        # Not for us, re-raise for outer catch
        raise
    finally:
        # Remove *this* catcher, by identity, rather than popping blindly: a
        # non-local exit out of the body may have left inner catchers on the
        # stack (their own `finally` clauses run, but an exception raised
        # inside a `finally` elsewhere could still skip one).
        for index in range(len(state.catch_tags) - 1, -1, -1):
            if state.catch_tags[index] is tag:
                del state.catch_tags[index:]
                break


def eval_throw(form, env):
    """Evaluate THROW special form: (THROW tag value)
    
    Throws to the nearest matching CATCH.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("THROW requires a tag and a value")
    
    tag_form = car(args)
    rest = cdr(args)
    
    if not _consp_internal(rest):
        raise lisptype.LispNotImplementedError("THROW requires a value")
    
    value_form = car(rest)
    
    # Evaluate both tag and value
    tag = eval(tag_form, env)
    value = eval(value_form, env)

    # CLHS THROW: "If there is no outstanding catcher whose tag is EQ to tag,
    # no unwinding is done and an error of type CONTROL-ERROR is signaled."
    # Signalling it here, before raising, is what keeps an uncaught throw
    # inside the language: it used to leave the evaluator as a bare Python
    # `ThrowException`, which matches no handler clause and therefore aborted
    # whatever was running the code instead of failing it.
    import fclpy.state as state
    if not any(_tags_match(tag, outstanding) for outstanding in state.catch_tags):
        from .evaluation_conditions import signal_error_object
        return signal_error_object(lisptype.ControlError(
            message=f"attempt to THROW to a tag that does not exist: {tag}"))

    raise ThrowException(tag, value)


def _tags_match(thrown, established):
    """Whether a THROW of `thrown` is caught by a CATCH of `established`.

    CLHS 5.2 says the comparison is EQ. The name fallback is this
    implementation's, and it is needed while a symbol can still reach here as
    a freshly built `LispSymbol` rather than an interned one; it is the one
    place the comparison is written, so CATCH and THROW cannot disagree about
    which tags match -- and disagreeing is what makes a throw either
    uncatchable or caught by the wrong frame.
    """
    if thrown is established or thrown == established:
        return True
    return (isinstance(thrown, lisptype.LispSymbol)
            and isinstance(established, lisptype.LispSymbol)
            and thrown.name == established.name)


def eval_unwind_protect(form, env):
    """Evaluate UNWIND-PROTECT special form: (UNWIND-PROTECT protected-form cleanup-form*)
    
    Evaluates protected-form, ensuring cleanup-forms run regardless of how it exits.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("UNWIND-PROTECT requires at least a protected form")
    
    protected_form = car(args)
    cleanup_forms = cdr(args)
    
    try:
        # Evaluate the protected form
        result = eval(protected_form, env)
        return result
    finally:
        # Always run cleanup forms, regardless of how we exit (normal or exception)
        current = cleanup_forms
        while _consp_internal(current):
            eval(car(current), env)
            current = cdr(current)


def _make_tag_key(tag):
    """Create a consistent key for a TAGBODY tag (symbol, number, or other atom).
    
    In Common Lisp, tags are atoms that can be symbols or numbers.
    This function creates a hashable key that can be used in dictionaries.
    """
    if isinstance(tag, lisptype.LispSymbol):
        return ('symbol', tag.name)
    elif isinstance(tag, (int, float)):
        return ('number', tag)
    else:
        # For other atoms, use string representation
        return ('other', str(tag))


def eval_tagbody(form, env):
    """Evaluate TAGBODY special form: (TAGBODY {tag | statement}*)
    
    Establishes tags for GO to jump to. Tags are atoms (symbols or numbers);
    other forms are statements. Executes statements in order. GO can jump to a tag,
    continuing from there. Returns NIL.

    The tags live on a `TagbodyFrame` registered on a fresh child environment
    which the body evaluates in, so GO -- direct, or from a closure the body
    defines -- resolves to *this* TAGBODY's tags through the lexical chain
    (CLHS 5.3.3), and a GO resolved to an outer TAGBODY's frame passes
    through here unmatched.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    
    # Collect all forms and identify tags
    forms = []
    tag_indices = {}  # Map tag key -> index in forms list
    
    current = args
    while _consp_internal(current):
        form_item = car(current)
        # Tags are atoms (non-lists) that appear at the top level of TAGBODY
        if not _consp_internal(form_item):
            tag_key = _make_tag_key(form_item)
            tag_indices[tag_key] = len(forms)
        forms.append(form_item)
        current = cdr(current)
    
    # The frame lives on a child environment so it is lexically visible to
    # exactly the body forms and the closures they define.
    tagbody_env = lisptype.Environment(env)
    frame = establish_tagbody_frame(tagbody_env, tag_indices)
    
    # Execute forms, handling GO exceptions
    index = 0
    try:
        while index < len(forms):
            form_item = forms[index]
            # Skip tags (they're just labels)
            if not _consp_internal(form_item):
                tag_key = _make_tag_key(form_item)
                if tag_key in tag_indices:
                    index += 1
                    continue
            
            try:
                # Evaluate the form
                eval(form_item, tagbody_env)
                index += 1
            except GoException as e:
                # GO was called: jump only when it resolved to *this*
                # TAGBODY's frame; a frame for another TAGBODY re-raises.
                if e.tagbody_frame is frame:
                    index = frame.tags[_make_tag_key(e.tag)] + 1
                else:
                    raise
    finally:
        deactivate_frame(frame)
    
    return lisptype.NIL


def eval_go(form, env):
    """Evaluate GO special form: (GO tag)
    
    Jumps to the specified tag in the lexically enclosing TAGBODY.
    Tags are atoms (symbols, numbers, or other atoms) that must match a tag in the TAGBODY.

    The target is resolved *lexically* -- the innermost visible TAGBODY
    frame that has the tag (CLHS 5.3.3). GOing to a tag that is not visible,
    or whose dynamic extent has ended, signals a CONTROL-ERROR.
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("GO requires a tag argument")
    
    tag = car(args)
    
    # Tags can be any atom (symbol, number, etc.)
    # They are not evaluated - the tag itself is used as the target
    if _consp_internal(tag):
        raise lisptype.LispNotImplementedError(f"GO tag must be an atom, got {tag}")
    
    frame = find_tagbody_frame(env, _make_tag_key(tag))
    if frame is None or not frame.active:
        from .evaluation_conditions import signal_error_object
        return signal_error_object(lisptype.ControlError(
            message=("attempt to GO to nonexistent tag: {}".format(tag)
                     if frame is None else
                     "attempt to GO to a tag whose TAGBODY has exited: {}".format(tag))))
    
    # Raise exception to be caught by the TAGBODY that established the frame
    raise GoException(tag, frame)


__all__ = [
    'eval_block',
    'eval_return_from',
    'eval_catch',
    'eval_throw',
    'eval_unwind_protect',
    'eval_tagbody',
    'eval_go',
    'BlockFrame',
    'TagbodyFrame',
    'establish_block_frame',
    'find_block_frame',
    'establish_tagbody_frame',
    'find_tagbody_frame',
    'deactivate_frame',
]
