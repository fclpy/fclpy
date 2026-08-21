"""Comprehensive tests for Phase 6 - Class System Foundation.

Tests DEFCLASS, MAKE-INSTANCE, slot operations, and basic generic functions.
"""

import pytest
import fclpy.lisptype as lisptype
import fclpy.lispfunc as lispfunc
from fclpy import classes
from fclpy.lispenv import setup_standard_environment


class TestClassDefinition:
    """Test DEFCLASS and class creation."""
    
    def setup_method(self):
        """Set up each test."""
        # Clear the class registry
        classes._class_registry._classes.clear()
    
    def test_defclass_creates_class(self):
        """Test that DEFCLASS creates a class."""
        name = lisptype.LispSymbol('PERSON')
        slots = [lisptype.LispSymbol('NAME')]
        
        result = lispfunc.defclass(name, [], slots)

        # DEFCLASS returns the new class object (CLHS 7.7 defclass, Values),
        # not its name -- unlike DEFUN/DEFVAR, which return the name.
        cls = classes.find_class('PERSON')
        assert cls is not None
        assert result is cls
        assert cls.name is name
    
    def test_defclass_with_slots(self):
        """Test DEFCLASS with multiple slots."""
        name = lisptype.LispSymbol('PERSON')
        slots = [
            lisptype.LispSymbol('NAME'),
            lisptype.LispSymbol('AGE'),
            lisptype.LispSymbol('EMAIL')
        ]
        
        lispfunc.defclass(name, [], slots)
        cls = classes.find_class('PERSON')
        
        # Check slots were created
        assert len(cls.direct_slots) == 3
        assert cls.direct_slots[0].name.name == 'NAME'
        assert cls.direct_slots[1].name.name == 'AGE'
        assert cls.direct_slots[2].name.name == 'EMAIL'
    
    def test_defclass_with_initargs(self):
        """Test DEFCLASS with :initarg in slot specs."""
        name = lisptype.LispSymbol('POINT')
        
        # Create slot with initarg
        x_slot = [
            lisptype.LispSymbol('X'),
            lisptype.lispKeyword('INITARG'),
            lisptype.lispKeyword('X'),
            lisptype.lispKeyword('INITFORM'),
            0
        ]
        y_slot = [
            lisptype.LispSymbol('Y'),
            lisptype.lispKeyword('INITARG'),
            lisptype.lispKeyword('Y'),
            lisptype.lispKeyword('INITFORM'),
            0
        ]
        
        lispfunc.defclass(name, [], [x_slot, y_slot])
        cls = classes.find_class('POINT')
        
        # Check initargs were set
        assert cls.direct_slots[0].initargs
        assert cls.direct_slots[0].initform == 0
        assert cls.direct_slots[1].initargs
        assert cls.direct_slots[1].initform == 0
    
    def test_class_has_slots_method(self):
        """Test CLASS-SLOTS function."""
        name = lisptype.LispSymbol('PERSON')
        slots = [
            lisptype.LispSymbol('NAME'),
            lisptype.LispSymbol('AGE')
        ]
        
        lispfunc.defclass(name, [], slots)
        cls = classes.find_class('PERSON')
        
        # Get slots via function
        slot_names = lispfunc.class_slots(cls)
        assert len(slot_names) == 2
        assert 'NAME' in slot_names
        assert 'AGE' in slot_names


class TestInstanceCreation:
    """Test MAKE-INSTANCE and instance creation."""
    
    def setup_method(self):
        """Set up each test."""
        classes._class_registry._classes.clear()
    
    def test_make_instance_basic(self):
        """Test basic MAKE-INSTANCE."""
        # Define a class
        name = lisptype.LispSymbol('PERSON')
        slots = [lisptype.LispSymbol('NAME')]
        lispfunc.defclass(name, [], slots)
        
        # Create an instance
        instance = lispfunc.make_instance('PERSON')
        
        assert isinstance(instance, classes.LispInstance)
        assert instance.lisp_class.name is name
    
    def test_make_instance_with_initargs(self):
        """Test MAKE-INSTANCE with initialization arguments."""
        # Define a class with initargs
        name = lisptype.LispSymbol('POINT')
        x_slot = [
            lisptype.LispSymbol('X'),
            lisptype.lispKeyword('INITARG'),
            lisptype.lispKeyword('X'),
            lisptype.lispKeyword('INITFORM'),
            0
        ]
        y_slot = [
            lisptype.LispSymbol('Y'),
            lisptype.lispKeyword('INITARG'),
            lisptype.lispKeyword('Y'),
            lisptype.lispKeyword('INITFORM'),
            0
        ]
        
        lispfunc.defclass(name, [], [x_slot, y_slot])
        
        # Create instance with initargs
        instance = lispfunc.make_instance('POINT', X=10, Y=20)
        
        # Check values were set
        assert lispfunc.slot_value(instance, 'X') == 10
        assert lispfunc.slot_value(instance, 'Y') == 20
    
    def test_slot_value_get(self):
        """Test SLOT-VALUE getter."""
        name = lisptype.LispSymbol('COUNTER')
        count_slot = [
            lisptype.LispSymbol('COUNT'),
            lisptype.lispKeyword('INITFORM'),
            0
        ]
        lispfunc.defclass(name, [], [count_slot])
        
        instance = lispfunc.make_instance('COUNTER')
        
        # Get slot value
        value = lispfunc.slot_value(instance, 'COUNT')
        assert value == 0
    
    def test_slot_value_set(self):
        """Test (SETF SLOT-VALUE) setter."""
        name = lisptype.LispSymbol('COUNTER')
        count_slot = [
            lisptype.LispSymbol('COUNT'),
            lisptype.lispKeyword('INITFORM'),
            0
        ]
        lispfunc.defclass(name, [], [count_slot])
        
        instance = lispfunc.make_instance('COUNTER')
        
        # Set slot value
        lispfunc.set_slot_value(42, instance, 'COUNT')
        
        # Verify it was set
        value = lispfunc.slot_value(instance, 'COUNT')
        assert value == 42
    
    def test_multiple_instances_independent(self):
        """Test that multiple instances are independent."""
        name = lisptype.LispSymbol('BOX')
        content_slot = [
            lisptype.LispSymbol('CONTENT'),
            lisptype.lispKeyword('INITFORM'),
            lisptype.NIL
        ]
        lispfunc.defclass(name, [], [content_slot])
        
        # Create two instances
        box1 = lispfunc.make_instance('BOX')
        box2 = lispfunc.make_instance('BOX')
        
        # Modify one
        lispfunc.set_slot_value('apple', box1, 'CONTENT')
        
        # Check the other is unaffected
        assert lispfunc.slot_value(box1, 'CONTENT') == 'apple'
        assert lispfunc.slot_value(box2, 'CONTENT') is lisptype.NIL


class TestClassInheritance:
    """Test class inheritance and slot inheritance."""
    
    def setup_method(self):
        """Set up each test."""
        classes._class_registry._classes.clear()
    
    def test_class_superclasses(self):
        """Test CLASS-SUPERCLASSES function."""
        # Define a parent class
        parent_name = lisptype.LispSymbol('ANIMAL')
        parent_slots = [lisptype.LispSymbol('NAME')]
        lispfunc.defclass(parent_name, [], parent_slots)
        
        # Define a child class
        child_name = lisptype.LispSymbol('DOG')
        parent_cls = classes.find_class('ANIMAL')
        child_slots = [lisptype.LispSymbol('BREED')]
        lispfunc.defclass(child_name, [parent_cls], child_slots)
        
        # Check superclasses
        child_cls = classes.find_class('DOG')
        supers = lispfunc.class_superclasses(child_cls)
        
        # supers is a lispCons list - convert to Python list to check
        super_list = []
        current = supers
        while isinstance(current, lisptype.lispCons):
            super_list.append(current.car)
            current = current.cdr
        
        assert len(super_list) > 0
    
    def test_slot_inheritance(self):
        """Test that child class inherits parent slots."""
        # Define a parent class
        parent_name = lisptype.LispSymbol('ANIMAL')
        parent_slots = [lisptype.LispSymbol('NAME')]
        lispfunc.defclass(parent_name, [], parent_slots)
        
        # Define a child class
        child_name = lisptype.LispSymbol('DOG')
        parent_cls = classes.find_class('ANIMAL')
        child_slots = [lisptype.LispSymbol('BREED')]
        lispfunc.defclass(child_name, [parent_cls], child_slots)
        
        # Check that child has both slots
        child_cls = classes.find_class('DOG')
        all_slots = child_cls.get_all_slots()
        
        assert 'NAME' in all_slots
        assert 'BREED' in all_slots
    
    def test_instance_of_child_class(self):
        """Test creating instances of child classes."""
        # Define a parent class
        parent_name = lisptype.LispSymbol('ANIMAL')
        parent_slots = [
            [lisptype.LispSymbol('NAME'), lisptype.lispKeyword('INITARG'),
             lisptype.lispKeyword('NAME'), lisptype.lispKeyword('INITFORM'), None]
        ]
        lispfunc.defclass(parent_name, [], parent_slots)
        
        # Define a child class
        child_name = lisptype.LispSymbol('DOG')
        parent_cls = classes.find_class('ANIMAL')
        child_slots = [
            [lisptype.LispSymbol('BREED'), lisptype.lispKeyword('INITARG'),
             lisptype.lispKeyword('BREED'), lisptype.lispKeyword('INITFORM'), None]
        ]
        lispfunc.defclass(child_name, [parent_cls], child_slots)
        
        # Create instance with values from both classes
        instance = lispfunc.make_instance('DOG', NAME='Fido', BREED='Labrador')
        
        # Check both slots exist and have correct values
        assert lispfunc.slot_value(instance, 'NAME') == 'Fido'
        assert lispfunc.slot_value(instance, 'BREED') == 'Labrador'


class TestInstancePredicate:
    """Test INSTANCEP function."""
    
    def setup_method(self):
        """Set up each test."""
        classes._class_registry._classes.clear()
    
    def test_instancep_true(self):
        """Test INSTANCEP returns T for instances."""
        name = lisptype.LispSymbol('THING')
        lispfunc.defclass(name, [], [])
        
        instance = lispfunc.make_instance('THING')
        
        result = lispfunc.instancep(instance)
        assert result is lisptype.T
    
    def test_instancep_false(self):
        """Test INSTANCEP returns NIL for non-instances."""
        result = lispfunc.instancep(42)
        assert result is lisptype.NIL
        
        result = lispfunc.instancep("string")
        assert result is lisptype.NIL
        
        result = lispfunc.instancep(lisptype.LispSymbol('SYMBOL'))
        assert result is lisptype.NIL


class TestGenericFunctions:
    """Test generic function creation and dispatch."""
    
    def setup_method(self):
        """Set up each test."""
        classes._class_registry._classes.clear()
        classes._generic_registry._generics.clear()
    
    def test_ensure_generic_function(self):
        """Test ENSURE-GENERIC-FUNCTION."""
        name = lisptype.LispSymbol('MY-GENERIC')
        gf = lispfunc.ensure_generic_function(name)
        
        assert isinstance(gf, classes.GenericFunction)
        assert gf.name is name
    
    def test_ensure_generic_idempotent(self):
        """Test that ENSURE-GENERIC-FUNCTION returns same function."""
        name = lisptype.LispSymbol('MY-GENERIC')
        
        gf1 = lispfunc.ensure_generic_function(name)
        gf2 = lispfunc.ensure_generic_function(name)
        
        assert gf1 is gf2
    
    def test_add_method_basic(self):
        """Test ADD-METHOD basic functionality."""
        gf = lispfunc.ensure_generic_function(lisptype.LispSymbol('PROCESS'))
        
        def my_method(x):
            return x * 2
        
        # Add a method with no specializers
        result = lispfunc.add_method(gf, [None], my_method)
        
        assert result is gf
        assert len(gf.methods) == 1
    
    def test_method_dispatch_simple(self):
        """Test basic method dispatch."""
        # Define a simple class
        cls_name = lisptype.LispSymbol('NUMBER-HOLDER')
        lispfunc.defclass(cls_name, [], [lisptype.LispSymbol('VALUE')])
        
        # Create generic function
        gf = lispfunc.ensure_generic_function(lisptype.LispSymbol('DOUBLE'))
        
        # Define method that works on NUMBER-HOLDER
        lisp_class = classes.find_class('NUMBER-HOLDER')
        
        def double_method(obj):
            val = lispfunc.slot_value(obj, 'VALUE')
            return val * 2
        
        # Add method specialized on NUMBER-HOLDER
        lispfunc.add_method(gf, [lisp_class], double_method)
        
        # Create instance
        instance = lispfunc.make_instance('NUMBER-HOLDER', VALUE=5)
        
        # Call generic function (pass instance as first arg, not in a list)
        result = lispfunc.call_generic_function(gf, instance)
        assert result == 10
    
    def test_method_dispatch_fallback_to_t(self):
        """Test method dispatch falls back to T specializer."""
        gf = lispfunc.ensure_generic_function(lisptype.LispSymbol('DESCRIBE'))
        
        def default_method(x):
            return "Unknown"
        
        # Add method with no specializer (T)
        lispfunc.add_method(gf, [None], default_method)
        
        # Call with any argument
        result = lispfunc.call_generic_function(gf, [42])
        assert result == "Unknown"


class TestClassSystemErrors:
    """Test error handling in class system."""
    
    def test_make_instance_undefined_class(self):
        """Test MAKE-INSTANCE with undefined class raises error."""
        with pytest.raises(NameError):
            lispfunc.make_instance('UNDEFINED-CLASS')
    
    def test_slot_value_nonexistent_slot(self):
        """Test SLOT-VALUE on a slot the class doesn't define invokes
        SLOT-MISSING (CLHS 7.5.3), whose default method signals a Lisp-level
        error -- not a bare Python AttributeError, which would be a Python
        exception leaking as a Lisp value the moment this same code path
        runs from a Lisp-level (slot-value ...) call."""
        classes._class_registry._classes.clear()

        name = lisptype.LispSymbol('THING')
        lispfunc.defclass(name, [], [lisptype.LispSymbol('A')])

        instance = lispfunc.make_instance('THING')

        with pytest.raises(lisptype.LispError):
            lispfunc.slot_value(instance, 'NONEXISTENT')
    
    def test_defclass_duplicate_registration(self):
        """Test that redefining a class updates the registry."""
        classes._class_registry._classes.clear()
        
        name = lisptype.LispSymbol('MUTABLE')
        
        # Define initially with one slot
        lispfunc.defclass(name, [], [lisptype.LispSymbol('X')])
        cls1 = classes.find_class('MUTABLE')
        assert len(cls1.direct_slots) == 1
        
        # Redefine with two slots
        lispfunc.defclass(name, [], [
            lisptype.LispSymbol('X'),
            lisptype.LispSymbol('Y')
        ])
        cls2 = classes.find_class('MUTABLE')
        
        # Should be registered (though it's a new class object)
        assert len(cls2.direct_slots) == 2


class TestIntegration:
    """Integration tests combining multiple class system features."""
    
    def setup_method(self):
        """Set up each test."""
        classes._class_registry._classes.clear()
        classes._generic_registry._generics.clear()
    
    def test_create_and_use_simple_class(self):
        """Test creating and using a simple class."""
        # Define a Person class
        person_slots = [
            [lisptype.LispSymbol('NAME'),
             lisptype.lispKeyword('INITARG'),
             lisptype.lispKeyword('NAME')],
            [lisptype.LispSymbol('AGE'),
             lisptype.lispKeyword('INITARG'),
             lisptype.lispKeyword('AGE'),
             lisptype.lispKeyword('INITFORM'),
             0]
        ]
        
        lispfunc.defclass(lisptype.LispSymbol('PERSON'), [], person_slots)
        
        # Create an instance
        person = lispfunc.make_instance('PERSON', NAME='Alice', AGE=30)
        
        # Access values
        assert lispfunc.slot_value(person, 'NAME') == 'Alice'
        assert lispfunc.slot_value(person, 'AGE') == 30
        
        # Modify values
        lispfunc.set_slot_value(31, person, 'AGE')
        assert lispfunc.slot_value(person, 'AGE') == 31
    
    def test_class_with_methods(self):
        """Test using classes with generic functions."""
        # Define a Point class
        point_slots = [
            [lisptype.LispSymbol('X'),
             lisptype.lispKeyword('INITARG'),
             lisptype.lispKeyword('X'),
             lisptype.lispKeyword('INITFORM'),
             0],
            [lisptype.LispSymbol('Y'),
             lisptype.lispKeyword('INITARG'),
             lisptype.lispKeyword('Y'),
             lisptype.lispKeyword('INITFORM'),
             0]
        ]
        
        lispfunc.defclass(lisptype.LispSymbol('POINT'), [], point_slots)
        
        # Create generic function for distance
        gf = lispfunc.ensure_generic_function(lisptype.LispSymbol('DISTANCE'))
        
        # Add method for POINT
        point_class = classes.find_class('POINT')
        
        def distance_method(point):
            x = lispfunc.slot_value(point, 'X')
            y = lispfunc.slot_value(point, 'Y')
            return (x ** 2 + y ** 2) ** 0.5
        
        lispfunc.add_method(gf, [point_class], distance_method)
        
        # Test the method
        point = lispfunc.make_instance('POINT', X=3, Y=4)
        result = lispfunc.call_generic_function(gf, point)
        
        assert abs(result - 5.0) < 0.0001


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
