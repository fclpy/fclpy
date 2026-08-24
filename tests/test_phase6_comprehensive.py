"""Comprehensive tests for Phase 6 type system.

Tests:
- All built-in type predicates work correctly
- User-defined classes integrate with type system
- Method dispatch chooses correct methods
- Inheritance works as expected
"""

import pytest
import fclpy.lisptype as lisptype
import fclpy.lispfunc as lispfunc
from fclpy import classes
from fclpy.lispfunc import comparison


class TestBuiltInTypePredicates:
    """Test all built-in type predicates."""
    
    def test_typep_integer(self):
        """Test TYPEP for integers."""
        assert lispfunc.typep(42, 'INTEGER') == lisptype.T
        assert lispfunc.typep(3.14, 'INTEGER') == lisptype.NIL
        assert lispfunc.typep('hello', 'INTEGER') == lisptype.NIL
    
    def test_typep_float(self):
        """Test TYPEP for floats."""
        assert lispfunc.typep(3.14, 'FLOAT') == lisptype.T
        assert lispfunc.typep(3.14, 'SINGLE-FLOAT') == lisptype.T
        assert lispfunc.typep(3.14, 'DOUBLE-FLOAT') == lisptype.T
        assert lispfunc.typep(42, 'FLOAT') == lisptype.NIL
    
    def test_typep_number(self):
        """Test TYPEP for numbers (both int and float)."""
        assert lispfunc.typep(42, 'NUMBER') == lisptype.T
        assert lispfunc.typep(3.14, 'NUMBER') == lisptype.T
        assert lispfunc.typep('hello', 'NUMBER') == lisptype.NIL
    
    def test_typep_string(self):
        """Test TYPEP for strings."""
        assert lispfunc.typep('hello', 'STRING') == lisptype.T
        assert lispfunc.typep('', 'STRING') == lisptype.T
        assert lispfunc.typep(42, 'STRING') == lisptype.NIL
    
    def test_typep_character(self):
        """Test TYPEP for characters."""
        assert lispfunc.typep('a', 'CHARACTER') == lisptype.T
        assert lispfunc.typep('hello', 'CHARACTER') == lisptype.NIL
        assert lispfunc.typep(42, 'CHARACTER') == lisptype.NIL
    
    def test_typep_symbol(self):
        """Test TYPEP for symbols."""
        sym = lisptype.LispSymbol('TEST')
        assert lispfunc.typep(sym, 'SYMBOL') == lisptype.T
        assert lispfunc.typep('hello', 'SYMBOL') == lisptype.NIL
        assert lispfunc.typep(42, 'SYMBOL') == lisptype.NIL
    
    def test_typep_keyword(self):
        """Test TYPEP for keywords."""
        kw = lisptype.lispKeyword('KEY')
        assert lispfunc.typep(kw, 'KEYWORD') == lisptype.T
        assert lispfunc.typep(lisptype.LispSymbol('TEST'), 'KEYWORD') == lisptype.NIL
    
    def test_typep_list(self):
        """Test TYPEP for lists."""
        cons_list = lisptype.lispCons(1, lisptype.lispCons(2, lisptype.NIL))
        assert lispfunc.typep(cons_list, 'LIST') == lisptype.T
        assert lispfunc.typep(lisptype.NIL, 'LIST') == lisptype.T
        assert lispfunc.typep(42, 'LIST') == lisptype.NIL
    
    def test_typep_cons(self):
        """Test TYPEP for cons cells."""
        cons_cell = lisptype.lispCons(1, lisptype.NIL)
        assert lispfunc.typep(cons_cell, 'CONS') == lisptype.T
        assert lispfunc.typep(lisptype.NIL, 'CONS') == lisptype.NIL
    
    def test_typep_null(self):
        """Test TYPEP for null/nil."""
        assert lispfunc.typep(lisptype.NIL, 'NULL') == lisptype.T
        assert lispfunc.typep(None, 'NULL') == lisptype.T
        assert lispfunc.typep(42, 'NULL') == lisptype.NIL
    
    def test_typep_atom(self):
        """Test TYPEP for atoms (non-cons)."""
        assert lispfunc.typep(42, 'ATOM') == lisptype.T
        assert lispfunc.typep('hello', 'ATOM') == lisptype.T
        assert lispfunc.typep(lisptype.NIL, 'ATOM') == lisptype.T
        cons_cell = lisptype.lispCons(1, lisptype.NIL)
        assert lispfunc.typep(cons_cell, 'ATOM') == lisptype.NIL
    
    def test_typep_function(self):
        """Test TYPEP for functions."""
        def my_func():
            pass
        assert lispfunc.typep(my_func, 'FUNCTION') == lisptype.T
        assert lispfunc.typep(lambda x: x, 'FUNCTION') == lisptype.T
        assert lispfunc.typep(42, 'FUNCTION') == lisptype.NIL
    
    def test_typep_vector(self):
        """Test TYPEP for vectors/arrays."""
        assert lispfunc.typep([1, 2, 3], 'VECTOR') == lisptype.T
        assert lispfunc.typep((1, 2, 3), 'VECTOR') == lisptype.T
        assert lispfunc.typep(42, 'VECTOR') == lisptype.NIL
    
    def test_typep_hash_table(self):
        """Test TYPEP for hash tables.

        A bare Python `dict` is *not* a Lisp hash table. This test used to
        assert that it was, which is the defect it was written against: TYPEP
        decided HASH-TABLE by `isinstance(obj, dict)` while HASH-TABLE-P asked
        about a different class entirely, so the two disagreed about the very
        object MAKE-HASH-TABLE returns. Both now ask
        `misc_hashtables.is_hash_table`.
        """
        ht = lispfunc.make_hash_table()
        assert lispfunc.typep(ht, 'HASH-TABLE') == lisptype.T
        assert lispfunc.hash_table_p(ht) == lisptype.T
        assert lispfunc.typep({'a': 1}, 'HASH-TABLE') == lisptype.NIL
        assert lispfunc.typep([1, 2, 3], 'HASH-TABLE') == lisptype.NIL
    
    def test_typep_t(self):
        """Test TYPEP for T (everything)."""
        assert lispfunc.typep(42, 'T') == lisptype.T
        assert lispfunc.typep('hello', 'T') == lisptype.T
        assert lispfunc.typep(lisptype.NIL, 'T') == lisptype.T


class TestUserDefinedTypePredicates:
    """Test TYPEP with user-defined classes."""
    
    def test_typep_user_defined_class(self):
        """Test TYPEP with user-defined class."""
        # Define a class
        lispfunc.defclass(lisptype.LispSymbol('PERSON'), [], [lisptype.LispSymbol('NAME')])
        
        # Create an instance
        instance = lispfunc.make_instance('PERSON')
        
        # Test TYPEP
        assert lispfunc.typep(instance, 'PERSON') == lisptype.T
        assert lispfunc.typep(instance, 'STANDARD-OBJECT') == lisptype.T
        assert lispfunc.typep(instance, 'INTEGER') == lisptype.NIL
    
    def test_typep_inheritance(self):
        """Test TYPEP with inheritance."""
        # Define parent class
        lispfunc.defclass(lisptype.LispSymbol('ANIMAL'), [], [lisptype.LispSymbol('NAME')])
        
        # Define child class
        parent_cls = classes.find_class('ANIMAL')
        lispfunc.defclass(lisptype.LispSymbol('DOG'), [parent_cls], [lisptype.LispSymbol('BREED')])
        
        # Create instance of child class
        instance = lispfunc.make_instance('DOG')
        
        # Test TYPEP - should match both child and parent
        assert lispfunc.typep(instance, 'DOG') == lisptype.T
        assert lispfunc.typep(instance, 'ANIMAL') == lisptype.T
        assert lispfunc.typep(instance, 'STANDARD-OBJECT') == lisptype.T
    
    def test_typep_with_class_object(self):
        """Test TYPEP using actual class object as type specifier."""
        # Define a class
        lispfunc.defclass(lisptype.LispSymbol('PERSON'), [], [lisptype.LispSymbol('NAME')])
        
        # Create an instance
        instance = lispfunc.make_instance('PERSON')
        
        # Get the class object
        cls = classes.find_class('PERSON')
        
        # Test TYPEP with class object
        assert lispfunc.typep(instance, cls) == lisptype.T
        
        # Create instance of different class
        lispfunc.defclass(lisptype.LispSymbol('ANIMAL'), [], [])
        animal = lispfunc.make_instance('ANIMAL')
        animal_cls = classes.find_class('ANIMAL')
        
        assert lispfunc.typep(animal, animal_cls) == lisptype.T
        assert lispfunc.typep(animal, cls) == lisptype.NIL


class TestTypeOf:
    """Test TYPE-OF function."""
    
    def test_typeof_integers(self):
        """Test TYPE-OF for integers."""
        result = lispfunc.type_of(42)
        assert isinstance(result, lisptype.LispSymbol)
        assert result.name == 'INTEGER'
        
        result = lispfunc.type_of(0)
        assert isinstance(result, lisptype.LispSymbol)
        assert result.name == 'BIT'
        
        result = lispfunc.type_of(1)
        assert isinstance(result, lisptype.LispSymbol)
        assert result.name == 'BIT'
    
    def test_typeof_floats(self):
        """Test TYPE-OF for floats."""
        result = lispfunc.type_of(3.14)
        assert isinstance(result, lisptype.LispSymbol)
        assert result.name == 'SINGLE-FLOAT'
    
    def test_typeof_strings(self):
        """Test TYPE-OF for strings."""
        result = lispfunc.type_of('hello')
        assert isinstance(result, lisptype.LispSymbol)
        assert result.name == 'STRING'
        
        result = lispfunc.type_of('a')
        assert isinstance(result, lisptype.LispSymbol)
        assert result.name == 'CHARACTER'
    
    def test_typeof_symbols(self):
        """Test TYPE-OF for symbols."""
        sym = lisptype.LispSymbol('TEST')
        result = lispfunc.type_of(sym)
        assert isinstance(result, lisptype.LispSymbol)
        assert result.name == 'SYMBOL'
    
    def test_typeof_keywords(self):
        """Test TYPE-OF for keywords."""
        kw = lisptype.lispKeyword('KEY')
        result = lispfunc.type_of(kw)
        assert isinstance(result, lisptype.LispSymbol)
        assert result.name == 'KEYWORD'
    
    def test_typeof_lists(self):
        """Test TYPE-OF for lists."""
        result = lispfunc.type_of(lisptype.NIL)
        assert isinstance(result, lisptype.LispSymbol)
        assert result.name == 'NULL'
        
        cons = lisptype.lispCons(1, lisptype.NIL)
        result = lispfunc.type_of(cons)
        assert isinstance(result, lisptype.LispSymbol)
        assert result.name == 'CONS'
    
    def test_typeof_user_defined(self):
        """Test TYPE-OF for user-defined instances."""
        # Define a class
        lispfunc.defclass(lisptype.LispSymbol('PERSON'), [], [])
        
        # Create an instance
        instance = lispfunc.make_instance('PERSON')
        
        # TYPE-OF should return the class name
        result = lispfunc.type_of(instance)
        assert isinstance(result, lisptype.LispSymbol)
        assert result.name == 'PERSON'


class TestInstancePredicate:
    """Test INSTANCEP function."""
    
    def test_instancep_true(self):
        """Test INSTANCEP returns T for instances."""
        lispfunc.defclass(lisptype.LispSymbol('THING'), [], [])
        instance = lispfunc.make_instance('THING')
        assert lispfunc.instancep(instance) == lisptype.T
    
    def test_instancep_false(self):
        """Test INSTANCEP returns NIL for non-instances."""
        assert lispfunc.instancep(42) == lisptype.NIL
        assert lispfunc.instancep('hello') == lisptype.NIL
        assert lispfunc.instancep([1, 2, 3]) == lisptype.NIL


class TestMethodDispatchWithTypes:
    """Test that method dispatch works with type checking."""
    
    def test_dispatch_selects_correct_method(self):
        """Test that dispatcher selects the most specific method."""
        # Define two classes
        lispfunc.defclass(lisptype.LispSymbol('SHAPE'), [], [])
        lispfunc.defclass(lisptype.LispSymbol('CIRCLE'), [classes.find_class('SHAPE')], [])
        
        # Create generic function
        gf = lispfunc.ensure_generic_function(lisptype.LispSymbol('AREA'))
        
        # Add methods
        shape_cls = classes.find_class('SHAPE')
        circle_cls = classes.find_class('CIRCLE')
        
        def shape_area(obj):
            return 0
        
        def circle_area(obj):
            return 3.14159
        
        # Add more specific method first (shouldn't matter due to sorting)
        lispfunc.add_method(gf, [circle_cls], circle_area)
        lispfunc.add_method(gf, [shape_cls], shape_area)
        
        # Test dispatch
        circle = lispfunc.make_instance('CIRCLE')
        shape = lispfunc.make_instance('SHAPE')
        
        # Circle should use circle_area method
        assert abs(lispfunc.call_generic_function(gf, circle) - 3.14159) < 0.0001
        
        # Shape should use shape_area method
        assert lispfunc.call_generic_function(gf, shape) == 0
    
    def test_dispatch_fallback_to_general(self):
        """Test dispatch falls back to general method."""
        # Define a class
        lispfunc.defclass(lisptype.LispSymbol('ANIMAL'), [], [])
        
        # Create generic function
        gf = lispfunc.ensure_generic_function(lisptype.LispSymbol('SPEAK'))
        
        # Add a general method (no specializer)
        def speak_general(*args):
            return "sound"
        
        lispfunc.add_method(gf, [None], speak_general)
        
        # Call with any object
        animal = lispfunc.make_instance('ANIMAL')
        result = lispfunc.call_generic_function(gf, animal)
        assert result == "sound"


class TestIntegrationWithPhase6:
    """Integration tests combining all Phase 6 features."""
    
    def test_class_hierarchy_with_typep(self):
        """Test class hierarchy with TYPEP."""
        # Create a small hierarchy
        lispfunc.defclass(lisptype.LispSymbol('BEING'), [], [])
        lispfunc.defclass(lisptype.LispSymbol('ANIMAL'), [classes.find_class('BEING')], [])
        lispfunc.defclass(lisptype.LispSymbol('MAMMAL'), [classes.find_class('ANIMAL')], [])
        
        # Create instance
        mammal = lispfunc.make_instance('MAMMAL')
        
        # Test inheritance chain
        assert lispfunc.typep(mammal, 'MAMMAL') == lisptype.T
        assert lispfunc.typep(mammal, 'ANIMAL') == lisptype.T
        assert lispfunc.typep(mammal, 'BEING') == lisptype.T
        assert lispfunc.typep(mammal, 'STANDARD-OBJECT') == lisptype.T
    
    def test_polymorphic_dispatch_over_hierarchy(self):
        """Test polymorphic dispatch over class hierarchy."""
        # Create classes
        lispfunc.defclass(lisptype.LispSymbol('VEHICLE'), [], [])
        lispfunc.defclass(lisptype.LispSymbol('CAR'), [classes.find_class('VEHICLE')], [])
        lispfunc.defclass(lisptype.LispSymbol('BIKE'), [classes.find_class('VEHICLE')], [])
        
        # Create generic function
        gf = lispfunc.ensure_generic_function(lisptype.LispSymbol('WHEELS'))
        
        # Add specific methods
        car_cls = classes.find_class('CAR')
        bike_cls = classes.find_class('BIKE')
        
        def car_wheels(obj):
            return 4
        
        def bike_wheels(obj):
            return 2
        
        lispfunc.add_method(gf, [car_cls], car_wheels)
        lispfunc.add_method(gf, [bike_cls], bike_wheels)
        
        # Test dispatch
        car = lispfunc.make_instance('CAR')
        bike = lispfunc.make_instance('BIKE')
        
        assert lispfunc.call_generic_function(gf, car) == 4
        assert lispfunc.call_generic_function(gf, bike) == 2
