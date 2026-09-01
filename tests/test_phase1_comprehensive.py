"""Comprehensive tests for Phase 1 improvements.

Tests all symbol and registry improvements including:
- Symbol identity and interning
- Self-evaluation properties
- Registry completeness and structure
- Package-based symbol creation
"""
import pytest
import fclpy.lisptype as lisptype
import fclpy.lispfunc as lispfunc
from fclpy.lispfunc import registry
from fclpy.lispenv import setup_standard_environment


class TestSymbolIdentity:
    """Test symbol identity properties."""
    
    def test_same_symbol_has_same_identity(self):
        """Test that interning the same symbol twice returns the same object."""
        sym1 = lisptype.intern_symbol('TEST-SYMBOL')
        sym2 = lisptype.intern_symbol('TEST-SYMBOL')
        assert sym1 is sym2, "Same symbol should have same identity"
    
    def test_symbol_identity_across_packages(self):
        """Test that different packages create different symbols with same name."""
        pkg1_sym = lisptype.intern_symbol('SYMBOL', lisptype.COMMON_LISP_USER_PACKAGE)
        pkg2_sym = lisptype.intern_symbol('SYMBOL', lisptype.KEYWORD_PACKAGE)
        assert pkg1_sym is not pkg2_sym, "Same name in different packages should be different objects"
    
    def test_symbol_has_slots(self):
        """Test that symbols have value, function, and plist slots."""
        sym = lisptype.LispSymbol('TEST')
        
        # Check slots exist
        assert hasattr(sym, 'value'), "Symbol should have value slot"
        assert hasattr(sym, 'function'), "Symbol should have function slot"
        assert hasattr(sym, 'plist'), "Symbol should have plist slot"
    
    def test_symbol_value_operations(self):
        """Test symbol value getter/setter."""
        sym = lisptype.LispSymbol('TEST-VAR')
        
        # Initially unbound
        assert lisptype.symbol_value(sym) is None
        
        # Set value
        lisptype.set_symbol_value(sym, 42)
        assert lisptype.symbol_value(sym) == 42
        
        # Update value
        lisptype.set_symbol_value(sym, 100)
        assert lisptype.symbol_value(sym) == 100
    
    def test_symbol_function_operations(self):
        """Test symbol function getter/setter."""
        sym = lisptype.LispSymbol('TEST-FN')
        test_func = lambda x: x + 1
        
        # Initially no function
        assert lisptype.symbol_function(sym) is None
        
        # Set function
        lisptype.set_symbol_function(sym, test_func)
        assert lisptype.symbol_function(sym) is test_func
        
        # Function should be callable
        assert lisptype.symbol_function(sym)(5) == 6
    
    def test_symbol_plist_operations(self):
        """Test symbol plist getter/setter."""
        sym = lisptype.LispSymbol('TEST-PLIST')
        
        # Initially empty
        assert lisptype.symbol_plist(sym) == {}
        
        # Set plist
        test_plist = {'key1': 'value1', 'key2': 42}
        lisptype.set_symbol_plist(sym, test_plist)
        assert lisptype.symbol_plist(sym) == test_plist


class TestKeywordBehavior:
    """Test keyword self-evaluation and behavior."""
    
    def test_keyword_created_in_keyword_package(self):
        """Test that keywords are created in the KEYWORD-PACKAGE."""
        keyword = lisptype.intern_keyword('TEST-KEYWORD')
        assert isinstance(keyword, lisptype.lispKeyword)
        assert keyword.name == 'TEST-KEYWORD'
    
    def test_keyword_self_evaluates(self):
        """Test that keywords self-evaluate in the evaluator."""
        keyword = lisptype.intern_keyword('SELF-EVAL')
        from fclpy.lispfunc.evaluation import eval
        env = setup_standard_environment()
        
        result = eval(keyword, env)
        assert result is keyword, "Keyword should self-evaluate"
    
    def test_keyword_identity(self):
        """Test that same keywords have same identity."""
        kw1 = lisptype.intern_keyword('KW')
        kw2 = lisptype.intern_keyword('KW')
        assert kw1 is kw2, "Same keyword should have same identity"
    
    def test_keyword_vs_symbol(self):
        """Test that keywords and symbols with same name are different."""
        keyword = lisptype.intern_keyword('NAME')
        symbol = lisptype.intern_symbol('NAME')
        assert keyword is not symbol
        assert isinstance(keyword, lisptype.lispKeyword)
        assert isinstance(symbol, lisptype.LispSymbol)


class TestPackageBasedSymbols:
    """Test package-based symbol creation and interning."""
    
    def test_intern_symbol_uses_cl_user_by_default(self):
        """Test that intern_symbol uses COMMON-LISP-USER by default."""
        sym = lisptype.intern_symbol('DEFAULT-PKG')
        assert sym.package is lisptype.COMMON_LISP_USER_PACKAGE
    
    def test_intern_symbol_with_explicit_package(self):
        """Test intern_symbol with explicit package."""
        sym = lisptype.intern_symbol('KEYWORD-TEST', lisptype.KEYWORD_PACKAGE)
        assert sym.package is lisptype.KEYWORD_PACKAGE
    
    def test_symbol_package_identity(self):
        """Test that symbol package is consistent."""
        sym1 = lisptype.intern_symbol('CONSISTENT')
        sym2 = lisptype.intern_symbol('CONSISTENT')
        assert sym1.package is sym2.package
    
    def test_py_str_to_sym_uses_intern_symbol(self):
        """Test that py_str_to_sym uses intern_symbol internally."""
        sym = lisptype.py_str_to_sym('CONVERTED')
        # Should be in COMMON-LISP-USER by default
        assert sym.package is lisptype.COMMON_LISP_USER_PACKAGE


class TestRegistryEntry:
    """Test the new RegistryEntry dataclass."""
    
    def test_registry_entry_has_required_fields(self):
        """Test that RegistryEntry has all required fields."""
        entry = registry.RegistryEntry(
            name='TEST',
            py_name='test_func',
            kind='function',
            arg_spec='(x y)',
            documentation='Test function',
            side_effects=True,
            extra={'custom': 'value'}
        )
        
        assert entry.name == 'TEST'
        assert entry.py_name == 'test_func'
        assert entry.kind == 'function'
        assert entry.arg_spec == '(x y)'
        assert entry.documentation == 'Test function'
        assert entry.side_effects is True
        assert entry.extra == {'custom': 'value'}
    
    def test_registry_entry_backward_compatibility_get(self):
        """Test that RegistryEntry.get() works like dict.get()."""
        entry = registry.RegistryEntry(
            name='FUNC',
            py_name='func_impl',
            arg_spec='(a b)',
            extra={'meta': 'data'}
        )
        
        assert entry.get('py_name') == 'func_impl'
        assert entry.get('name') == 'FUNC'
        assert entry.get('arg_spec') == '(a b)'
        assert entry.get('meta') == 'data'
        assert entry.get('nonexistent') is None
        assert entry.get('nonexistent', 'default') == 'default'
    
    def test_registry_entry_backward_compatibility_getitem(self):
        """Test that RegistryEntry[key] works like dict[key]."""
        entry = registry.RegistryEntry(
            name='TEST',
            py_name='test',
            extra={'extra_field': 'value'}
        )
        
        assert entry['py_name'] == 'test'
        assert entry['name'] == 'TEST'
        assert entry['extra_field'] == 'value'
    
    def test_registry_entry_backward_compatibility_items(self):
        """Test that RegistryEntry.items() works like dict.items()."""
        entry = registry.RegistryEntry(
            name='FUNC',
            py_name='func',
            kind='special'
        )
        
        items = dict(entry.items())
        assert 'py_name' in items
        assert 'name' in items
        assert 'kind' in items
        assert items['py_name'] == 'func'
        assert items['name'] == 'FUNC'


class TestRegistryCompleteness:
    """Test that all registered functions are accessible."""
    
    def test_function_registry_populated(self):
        """Test that function registry is not empty."""
        assert len(registry.function_registry) > 0
        
        # Check some expected functions are there
        assert 'NOT' in registry.function_registry
        assert 'CAR' in registry.function_registry
        assert '+' in registry.function_registry
    
    def test_special_registry_populated(self):
        """Test that special registry is not empty."""
        assert len(registry.special_registry) > 0
        
        # Check some expected special operators
        assert 'IF' in registry.special_registry
        assert 'QUOTE' in registry.special_registry
    
    def test_registry_entries_are_registryentry_objects(self):
        """Test that all registry entries are RegistryEntry objects."""
        for lisp_name, entry in registry.function_registry.items():
            assert isinstance(entry, registry.RegistryEntry), \
                f"{lisp_name} is not a RegistryEntry: {type(entry)}"
        
        for lisp_name, entry in registry.special_registry.items():
            assert isinstance(entry, registry.RegistryEntry), \
                f"{lisp_name} is not a RegistryEntry: {type(entry)}"
    
    def test_all_registry_entries_have_py_name(self):
        """Test that all registry entries have a py_name."""
        for lisp_name, entry in registry.function_registry.items():
            assert entry.py_name is not None, \
                f"Function {lisp_name} missing py_name"
        
        for lisp_name, entry in registry.special_registry.items():
            assert entry.py_name is not None, \
                f"Special {lisp_name} missing py_name"
    
    def test_functions_accessible_in_environment(self):
        """Test that registered functions are accessible in environment."""
        env = setup_standard_environment()
        
        # Test a few known functions
        test_funcs = [
            ('NOT', lisptype.LispSymbol('NOT')),
            ('CAR', lisptype.LispSymbol('CAR')),
            ('+', lisptype.LispSymbol('+')),
        ]
        
        for name, sym in test_funcs:
            func = env.find_func(sym)
            assert func is not None, f"Function {name} not found in environment"
            assert callable(func), f"Function {name} is not callable"


class TestPhase1Integration:
    """Integration tests for all Phase 1 features working together."""
    
    def test_symbol_to_function_binding(self):
        """Test that symbols correctly bind to functions."""
        env = setup_standard_environment()
        
        # Create a symbol via intern
        sym = lisptype.intern_symbol('CAR')
        
        # Find function in environment
        func = env.find_func(sym)
        assert func is not None
        
        # Test it works with a cons cell
        cons_cell = lisptype.lispCons(1, lisptype.lispCons(2, lisptype.lispCons(3, lisptype.NIL)))
        result = func(cons_cell)
        assert result == 1
    
    def test_keyword_in_expression(self):
        """Test that keywords work in expressions."""
        keyword = lisptype.intern_keyword('MY-KEYWORD')
        env = setup_standard_environment()
        
        from fclpy.lispfunc.evaluation import eval
        result = eval(keyword, env)
        assert result is keyword
    
    def test_symbol_with_slots_in_context(self):
        """Test symbol slots work with environment bindings."""
        sym = lisptype.LispSymbol('CONTEXT-VAR')
        env = setup_standard_environment()
        
        # Set value in symbol
        test_value = 42
        lisptype.set_symbol_value(sym, test_value)
        
        # Set function in symbol
        test_func = lambda x: x * 2
        lisptype.set_symbol_function(sym, test_func)
        
        # Both should be accessible
        assert lisptype.symbol_value(sym) == test_value
        assert lisptype.symbol_function(sym) is test_func
    
    def test_registry_supports_dynamic_registration(self):
        """Test that registry can still be extended dynamically."""
        # Create a simple decorator
        @registry.cl_function('CUSTOM-FUNC', arg_spec='(x)', side_effects=False)
        def custom_func(x):
            return x + 100
        
        assert 'CUSTOM-FUNC' in registry.function_registry
        entry = registry.function_registry['CUSTOM-FUNC']
        assert entry.py_name == 'custom_func'
        assert entry.arg_spec == '(x)'
        assert entry.side_effects is False
