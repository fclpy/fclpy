"""Control flow special forms: BLOCK, CATCH, THROW, UNWIND-PROTECT, TAGBODY, GO."""

import fclpy.lisptype as lisptype
from .core import car, cdr, _consp_internal
from . import registry as _registry
from .evaluation_core import ReturnFromException, ThrowException, GoException


def eval_block(form, env):
    """Evaluate BLOCK special form: (BLOCK name body-form*)
    
    Establishes a block with the given name. Evaluates body forms in sequence.
    Can be exited early with RETURN-FROM.
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
    
    try:
        # Evaluate body forms in sequence
        result = lisptype.NIL
        current = body_forms
        while _consp_internal(current):
            result = eval(car(current), env)
            current = cdr(current)
        return result
    except ReturnFromException as e:
        # Check if this exception is for our block
        # Need to handle both symbol names and NIL
        block_match = False
        if e.tag == block_name:
            block_match = True
        elif isinstance(e.tag, lisptype.LispSymbol) and isinstance(block_name, lisptype.LispSymbol):
            block_match = (e.tag.name == block_name.name)
        elif (e.tag is None or e.tag == lisptype.NIL) and (block_name is None or block_name == lisptype.NIL):
            block_match = True
        
        if block_match:
            return e.value
        else:
            # Not for us, re-raise for outer block
            raise


def eval_return_from(form, env):
    """Evaluate RETURN-FROM special form: (RETURN-FROM name value?)
    
    Exits the named BLOCK, returning the specified value (or NIL).
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
    
    # Evaluate the value form (default to NIL)
    if _consp_internal(value_forms):
        value = eval(car(value_forms), env)
    else:
        value = lisptype.NIL
    
    # Raise exception to exit the block
    raise ReturnFromException(block_name, value)


@_registry.cl_macro('RETURN', documentation='RETURN macro: exits innermost NIL block')
def return_macro_expander(*args):
    """RETURN macro expander: converts (RETURN [value]) to (RETURN-FROM NIL [value])
    
    RETURN is equivalent to RETURN-FROM with NIL as the block name.
    Macro arguments: optional value form)
    """
    # Validate argument count (0 or 1 argument)
    if len(args) > 1:
        raise lisptype.LispProgramError(
            message=f"RETURN macro takes 0 or 1 argument, got {len(args)}"
        )
    
    # Build expansion: (RETURN-FROM NIL [value])
    if args:
        # (RETURN value) -> (RETURN-FROM NIL value)
        value_form = args[0]
        return lisptype.lispCons(
            lisptype.LispSymbol('RETURN-FROM'),
            lisptype.lispCons(
                lisptype.NIL,
                lisptype.lispCons(value_form, lisptype.NIL)
            )
        )
    else:
        # (RETURN) -> (RETURN-FROM NIL)
        return lisptype.lispCons(
            lisptype.LispSymbol('RETURN-FROM'),
            lisptype.lispCons(lisptype.NIL, lisptype.NIL)
        )


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
    
    # Execute forms, handling GO exceptions
    index = 0
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
            eval(form_item, env)
            index += 1
        except GoException as e:
            # GO was called - find the tag and jump to it
            tag_key = _make_tag_key(e.tag)
            if tag_key in tag_indices:
                # Jump to after the tag
                index = tag_indices[tag_key] + 1
            else:
                # Tag not in this TAGBODY - re-raise for outer TAGBODY
                raise
    
    return lisptype.NIL


def eval_go(form, env):
    """Evaluate GO special form: (GO tag)
    
    Jumps to the specified tag in the lexically enclosing TAGBODY.
    Tags are atoms (symbols, numbers, or other atoms) that must match a tag in the TAGBODY.
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("GO requires a tag argument")
    
    tag = car(args)
    
    # Tags can be any atom (symbol, number, etc.)
    # They are not evaluated - the tag itself is used as the target
    if _consp_internal(tag):
        raise lisptype.LispNotImplementedError(f"GO tag must be an atom, got {tag}")
    
    # Raise exception to be caught by enclosing TAGBODY
    raise GoException(tag)


__all__ = [
    'eval_block',
    'eval_return_from',
    'eval_catch',
    'eval_throw',
    'eval_unwind_protect',
    'eval_tagbody',
    'eval_go',
]
