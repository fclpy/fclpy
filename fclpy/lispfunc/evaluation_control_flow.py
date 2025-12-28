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
    
    if not isinstance(block_name, lisptype.LispSymbol):
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
        if e.tag == block_name or (isinstance(e.tag, lisptype.LispSymbol) and 
                                    isinstance(block_name, lisptype.LispSymbol) and
                                    e.tag.name == block_name.name):
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
    
    if not isinstance(block_name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError(f"RETURN-FROM name must be a symbol, got {block_name}")
    
    # Evaluate the value form (default to NIL)
    if _consp_internal(value_forms):
        value = eval(car(value_forms), env)
    else:
        value = lisptype.NIL
    
    # Raise exception to exit the block
    raise ReturnFromException(block_name, value)


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
    
    try:
        # Evaluate body forms in sequence
        result = lisptype.NIL
        current = body_forms
        while _consp_internal(current):
            result = eval(car(current), env)
            current = cdr(current)
        return result
    except ThrowException as e:
        # Check if tag matches
        if e.tag == tag or (isinstance(e.tag, lisptype.LispSymbol) and 
                           isinstance(tag, lisptype.LispSymbol) and
                           e.tag.name == tag.name):
            # Caught! Return the thrown value
            return e.value
        else:
            # Not for us, re-raise for outer catch
            raise


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
    
    # Raise exception
    raise ThrowException(tag, value)


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


def eval_tagbody(form, env):
    """Evaluate TAGBODY special form: (TAGBODY {tag | statement}*)
    
    Establishes tags for GO to jump to. Tags are symbols; other forms are statements.
    Executes statements in order. GO can jump to a tag, continuing from there.
    Returns NIL.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    
    # Collect all forms and identify tags
    forms = []
    tag_indices = {}  # Map tag name -> index in forms list
    
    current = args
    while _consp_internal(current):
        form_item = car(current)
        # Tags are symbols that appear at the top level of TAGBODY
        if isinstance(form_item, lisptype.LispSymbol):
            tag_indices[form_item.name] = len(forms)
        forms.append(form_item)
        current = cdr(current)
    
    # Execute forms, handling GO exceptions
    index = 0
    while index < len(forms):
        form_item = forms[index]
        # Skip tags (they're just labels)
        if isinstance(form_item, lisptype.LispSymbol) and form_item.name in tag_indices:
            index += 1
            continue
        
        try:
            # Evaluate the form
            eval(form_item, env)
            index += 1
        except GoException as e:
            # GO was called - find the tag and jump to it
            tag_name = e.tag.name if hasattr(e.tag, 'name') else str(e.tag)
            if tag_name in tag_indices:
                # Jump to after the tag
                index = tag_indices[tag_name] + 1
            else:
                # Tag not in this TAGBODY - re-raise for outer TAGBODY
                raise
    
    return lisptype.NIL


def eval_go(form, env):
    """Evaluate GO special form: (GO tag)
    
    Jumps to the specified tag in the lexically enclosing TAGBODY.
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("GO requires a tag argument")
    
    tag = car(args)
    
    if not isinstance(tag, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError(f"GO tag must be a symbol, got {tag}")
    
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
