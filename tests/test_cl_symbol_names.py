"""Regression tests for plan.md M1 step 1 (canonical CL symbol table).

Guards two invariants that used to be violated (plan.md Finding A):
  1. All 978 symbols ansi-test/cl-symbol-names.lsp requires to be external in
     COMMON-LISP are present and external, regardless of registry state.
  2. Nothing else is external in COMMON-LISP - the ~114 implementation-internal
     helpers the registry auto-discovers (EVAL-IF, PUTPROP, ...) must not leak
     into the namespace real Common Lisp code relies on being clean.
"""
import fclpy.lisptype as lisptype
import fclpy.lispenv as lispenv
import fclpy.state as state
from fclpy.cl_symbol_names import CL_SYMBOL_NAMES


def _fresh_environment():
    state.current_environment = None
    state.functions_loaded = False
    return lispenv.setup_standard_environment()


def test_canonical_symbol_count_is_978():
    assert len(CL_SYMBOL_NAMES) == 978


def test_all_canonical_symbols_present_and_external():
    _fresh_environment()
    cl = lisptype.COMMON_LISP_PACKAGE
    missing = sorted(n for n in CL_SYMBOL_NAMES if n not in cl.external_symbols)
    assert not missing, f"Not external in COMMON-LISP: {', '.join(missing)}"


def test_no_extra_external_symbols_in_common_lisp():
    _fresh_environment()
    cl = lisptype.COMMON_LISP_PACKAGE
    extra = sorted(n for n in cl.external_symbols if n not in CL_SYMBOL_NAMES)
    assert not extra, f"Non-ANSI symbols wrongly external in COMMON-LISP: {', '.join(extra)}"


def test_registry_internals_are_routed_to_fclpy_internal_not_cl():
    _fresh_environment()
    cl = lisptype.COMMON_LISP_PACKAGE
    internal = lisptype.FCLPY_INTERNAL_PACKAGE
    # A couple of the specific leaks plan.md's Finding A names by example.
    # LIST-STAR was one of them; it is gone entirely now that the duplicate
    # (and broken) `list_star` implementation of LIST* was deleted, so the
    # property to hold is "not in CL", with routing checked only for the
    # internals that still exist.
    for leaked_name in ('EVAL-IF', 'PUTPROP', 'LIST-STAR', 'GET-ENV'):
        assert leaked_name not in cl.symbols, (
            f"{leaked_name} should not be interned in COMMON-LISP at all"
        )
    for leaked_name in ('EVAL-IF', 'PUTPROP', 'GET-ENV'):
        assert leaked_name in internal.symbols, (
            f"{leaked_name} should have been routed to FCLPY-INTERNAL"
        )
