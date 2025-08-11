import functools
from operator import abs
import math
import re as _re

from fclpy.lispenv import current_environment
import fclpy.lisptype as lisptype
import fclpy.lispreader as lispreader


__reader_macros = {}

def abort(cond=True):
    raise(Exception("Not implemented"))

# abs() : provided

def acons(x,v,seq):
 """Creates a fresh cons, the cdr of which is alist and the car of which is 
 another fresh cons, the car of which is key and the cdr of which is 
 datum. """
 return lisptype.lispCons(lisptype.lispCons(x,v),seq)

def acos(x):
    return math.acos(x)

def acosh(x):
    return math.acosh(x)

def add_method(s,package=None):
    raise(Exception("Not implemented"))

def adjoin(x,seq,test=lambda x,y: x is y):
    """Tests whether item is the same as an existing element of list. If the 
    item is not an existing element, adjoin adds it to list (as if by cons) and 
    returns the resulting list; otherwise, nothing is added and the original 
    list is returned. """
    return seq if any(map(functools.partial(test,x),seq)) else cons(x,seq)

def adjust_array(*args):
    raise(Exception("Not implemented"))

def adjustable_array_p():
    raise(Exception("Not implemented"))

def allocate_instance(s,package=None):
    raise(Exception("Not implemented"))

def alpha_char_p(c):
    return type(c) is str and c.isalpha()

def alphanumericp(c):
    """Returns true if character is an alphabetic character or a numeric 
    character; otherwise, returns false. """
    return type(c) is str and c.isalpha() or c.isnumeric()
# def # and : macro?

def append(*args):
    if len(args)<1:
        return None
    val = functools.reduce(lambda l1,l2: l1 if l2 == None or type(l2) is lisptype.lispNull else l1 + tuple(l2),args[:-1],())
    if len(val) < 1:
        return args[-1]
    val = lisptype.lispCons(val[0],val[1:])  
    c = val  
    while c.cdr != None and c.cdr != lisptype.NIL:
        c = c.cdr     
    c.cdr = args[-1]
    if type(c.cdr) == tuple:
        c.cdr = lisptype.lispCons(c.cdr[0],c.cdr[1:])
    return val

def apply(f, *args):
    return f(*args)

def apropos(s,package=None):
    raise(Exception("Not implemented"))

def apropos_list(s,package=None):
    raise(Exception("Not implemented"))

def aref(s,package=None):
    raise(Exception("Not implemented"))

# def # arithmetic_error

def arithmetic_error_operands(s,package=None):
    raise(Exception("Not implemented"))

def arithmetic_error_operation(s,package=None):
    raise(Exception("Not implemented"))

# def # array

def array_dimension(arr):
    raise(Exception("Not implemented"))

# def # array-dimension-limit
# def # array-dimensions
# def # array-displacement
# def # array-element-type
# def # array-has-fill-pointer-p
# def # array-in-bounds-p
# def # array-rank
# def # array-rank-limit
# def # array-row-major-index
# def # array-total-size
# def # array-total-size-limit

def arrayp(arr):
    raise(Exception("Not implemented"))

def ash(i,count):
    return i << count if count >= 0 else i >> -count

def asin(x):
    return math.asin(x)

def asinh(x):
    return math.asinh(x)

# def # assert : macro?

def assoc(item,alist):
    return next(filter(lambda x: car(x) == item,map(tuple,alist)),None)

def assoc_if(arr):
    raise(Exception("Not implemented"))

def assoc_if_not(arr):
 raise(Exception("Not implemented"))

def atan(x):
    return math.atan(x)

def atanh(x):
    return math.atanh(x)

def atom(obj):
    return type(obj) is not lisptype.lispCons

# def # def base-char
# def base-string
# def bignum
# def bit
# def bit-and
# def bit-andc1
# def bit-andc2
# def bit-eqv
# def bit-ior
# def bit-nand
# def bit-nor
# def bit-not
# def bit-orc1
# def bit-orc2
# def bit-vector
# def bit-vector-p
# def bit-xor
# def block
# def boole
# def boole-1
# def boole-2
# def boole-and
# def boole-andc1
# def boole-andc2
# def boole-c1
# def boole-c2
# def boole-clr
# def boole-eqv
# def boole-ior
# def boole-nand
# def boole-nor
# def boole-orc1
# def boole-orc2
# def boole-set
# def boole-xor
# def boolean
# def both-case-p
# def boundp
# def break
# def broadcast-stream
# def broadcast-stream-streams
# def built-in-class

def butlast(seq):
 return tuple(seq[:-1])

# def byte
# def byte-position
# def byte-size 
# def #boole-c1
#boole-c2

def caaaar(x):
    return car(car(car(car(x))))

def caaadr(x):
    return car(car(car(cdr(x))))

def caaar(x):
    return car(car(car(x)))

def caadar(x):
    return car(car(cdr(car(x))))

def caaddr(x):
    return car(car(cdr(cdr(x))))

def caadr(x):
    return car(car(cdr(x)))

def caar(x):
    return car(car(x))

def cadaar(x):
    return car(cdr(car(car(x))))

def cadadr(x):
    return car(cdr(car(cdr(x))))

def cadar(x):
    return car(cdr(car(x)))

def caddar(x):
    return car(cdr(cdr(car(x))))

def cadddr(x):
    return car(cdr(cdr(cdr(x))))

def caddr(x):
    return car(cdr(cdr(x)))

def cadr(x):
    return car(cdr(x))

#multiple-value-call
# def call-arguments-limit
# def call-method
# def call-next-method
#nstring-capitalize
#string-capitalize

def car(seq):
 if type(seq) is lisptype.lispCons:
    return seq.car
 if type(seq) is tuple:
    return seq[0]
 return seq

# def case
#handler-case
#readtable-case
#restart-case
#*print-case*
#both-case-p
#lower-case-p
#upper-case-p
# def catch
# def ccase

def cdaaar(x):
    return cdr(car(car(car(x))))

def cdaadr(x):
    return cdr(car(car(cdr(x))))

def cdaar(x):
    return cdr(car(car(x)))
def cdadar(x):
    return cdr(car(cdr(car(x))))

def cdaddr(x):
    return cdr(car(cdr(cdr(x))))

def cdadr(x):
    return cdr(car(cdr(x)))

def cdar(x):
    return cdr(car(x))

def cddaar(x):
    return cdr(cdr(car(car(x))))

def cddadr(x):
    return cdr(cdr(car(cdr(x))))

def cddar(x):
    return cdr(cdr(car(x)))

def cdddar(x):
    return cdr(cdr(cdr(car(x))))

def cddddr(x):
    return cdr(cdr(cdr(cdr)))

def cdddr(x):
    return cdr(cdr(cdr(x)))

def cddr(x):
    return cdr(cdr(x))

def cdr(seq):
 if type(seq) is lisptype.lispCons:
    return seq.cdr
 if type(seq) is tuple:
    return seq[1:]
 return seq

def ceiling(x, divisor=1):
    """Return ceiling as integer"""
    return math.ceil(x / divisor)

# def cell-error
# def cell-error-name
# def cerror
# def change-class
#base-char
# def char
# def code-char
#digit-char
#extended-char
#name-char
#peek-char
#read-char
#set-syntax-from-char
#standard-char
#unread-char
#write-char
# def char-code
# def char-code-limit
# def char-downcase
# def char-equal
# def char-greaterp
# def char-int
# def char-lessp
# def char-name
#read-char-no-hang
# def char-not-equal
# def char-not-greaterp
# def char-not-lessp
#alpha-char-p
#digit-char-p
#graphic-char-p
#standard-char-p
# def char-upcase
# def char/=
# def char<
# def char<=
# def char=
# def char>
# def char>=
# def character
#get-dispatch-macro-character
def get_macro_character(x):
    return __reader_macros.get(x)

#make-dispatch-macro-character
def set_dispatch_macro_character(x,f):
    __reader_macros[x] = f

#set-macro-character
# def characterp
# def check-type
#*print-circle*
# def cis
#built-in-class
# def change-class
# def class
#find-class
#standard-class
#structure-class
# update-instance-for-different-class
# update-instance-for-redefined-class
# def class-name
# def class-of
# def clear-input
# def clear-output
# def close
#boole-clr
# def clrhash
# def char-code
# def code-char
# def char-code-limit
# def coerce
#define-method-combination
#method-combination
#method-combination-error
# def compilation-speed
#with-compilation-unit
# def compile
# def compile-file
# def compile-file-pathname
#*compile-file-pathname*
#*compile-file-truename*
#*compile-print*
#*compile-verbose*
# def compiled-function
# def compiled-function-p
# def compiler-macro
#define-compiler-macro
# def compiler-macro-function
# def complement
# def complex
#upgraded-complex-part-type
# def complexp
# def compute-applicable-methods
# def compute-restarts
# def concatenate
# def concatenated-stream
#make-concatenated-stream
# def concatenated-stream-streams
# def cond
# def condition
#define-condition
#make-condition
#serious-condition
#simple-condition
#storage-condition
#simple-condition-format-arguments
#simple-condition-format-control
#with-condition-restarts
# def conjugate
def cons(x,seq):
    """Returns a new tuple where x is the first element and seq is the rest."""
    return lisptype.lispCons(x,seq)

def consp(obj):
    return type(obj) is lisptype.lispCons

# def constantly
# def constantp
# def continue
#simple-condition-format-control
# def control-error
# def copy-alist
# def copy-list
# def copy-pprint-dispatch
# def copy-readtable
# def copy-seq
# def copy-structure
# def copy-symbol

def copy_tree(tree):
    if atom(tree):
        return tree
    return cons(copy_tree(tree.car),copy_tree(tree.cdr))

def cos(a):
    return math.cos(a)

# def cosh
# def count
#hash-table-count
# def count-if
# def count-if-not
#*gensym-counter*
# def ctypecase
# def # file-write-date
# type-error-datum
#  def debug
# *debug-io*
# invoke-debugger
# *debugger-hook*
#  def decf
#  def declaim
#  def declaration
#  def declare
#  def decode-float
# integer-decode-float
#  def decode-universal-time
# get-decoded-time
# *read-default-float-format*
# *default-pathname-defaults*
# *default-pathname-defaults*
#  def defclass
#  def defconstant
#  def defgeneric
#  def define-compiler-macro
#  def define-condition
#  def define-method-combination
#  def define-modify-macro
#  def define-setf-expander
#  def define-symbol-macro
#  def defmacro
#  def defmethod
#  def defpackage
#  def defparameter
#  def defsetf
#  def defstruct
#  def deftype
#  def defun
#  def defvar
#  def delete
#  def delete-duplicates
#  def delete-file
#  def delete-if
#  def delete-if-not
#  def delete-package
# read-delimited-list
#  def denominator
#  def deposit-field
#  def describe
#  def describe-object
#  def destructuring-bind
# pathname-device
# nset-difference
# set-difference
# update-instance-for-different-class
#  def digit-char
#  def digit-char-p
# float-digits
# array-dimension
# array-dimension-limit
# array-dimensions
# ensure-directories-exist
#  def directory
# pathname-directory
#  def directory-namestring
#  def disassemble
# copy-pprint-dispatch
# pprint-dispatch
# set-pprint-dispatch
# *print-pprint-dispatch*
# get-dispatch-macro-character
# make-dispatch-macro-character
# set-dispatch-macro-character
# array-displacement
#  def division-by-zero
#  def do
#  def do*
#  def do-all-symbols
#  def do-external-symbols
#  def do-symbols
#  def documentation
#  def dolist
#  def dotimes
#  def double-float
# least-negative-double-float
# least-negative-normalized-double-float
# least-positive-double-float
# least-positive-normalized-double-float
# most-negative-double-float
# most-positive-double-float
#  def double-float-epsilon
#  def double-float-negative-epsilon
# char-downcase
# nstring-downcase
# string-downcase
#  def dpb
#  def dribble
#  def delete-duplicates
# remove-duplicates
#  def dynamic-extent
# def # def ecase
# def echo-stream
# def echo-stream-input-stream
# def echo-stream-output-stream
# def ed
# def eighth
# def elt
# def encode-universal-time
# def end-of-file
# def endp
# def enough-namestring
# def ensure-directories-exist
# def ensure-generic-function
# def eq
def eq(a, b):
    """Test if two objects are the same object (identity)."""
    return a is b

# def eql
def eql(a, b):
    """Test if two objects are equal (value equality)."""
    return a == b

# def equal
def equal(a, b):
    """Test if two objects are structurally equal."""
    if type(a) != type(b):
        return False
    if isinstance(a, lisptype.lispCons) and isinstance(b, lisptype.lispCons):
        return equal(a.car, b.car) and equal(a.cdr, b.cdr)
    return a == b

# def equalp
# def error
def error(msg):
    """Signal an error."""
    raise Exception(str(msg))

# def etypecase

def eval(form, env=None):
    """Evaluate a Lisp form in the given environment."""
    if env is None:
        env = current_environment
    
    # Self-evaluating forms (numbers, strings, etc.)
    if form is None or isinstance(form, (int, float, str)):
        return form
    
    # NIL evaluates to itself
    if form is lisptype.NIL or type(form) is lisptype.lispNull:
        return lisptype.NIL
    
    # Symbols - look up in environment
    if isinstance(form, lisptype.LispSymbol):
        value = env.find_variable(form)
        if value is not None:
            return value
        # If not found as variable, might be a function
        func = env.find_func(form)
        if func is not None:
            return func
        raise Exception(f"Unbound variable: {form.name}")
    
    # Lists - function calls or special forms
    if isinstance(form, lisptype.lispCons):
        operator = car(form)
        
        # Handle special forms
        if isinstance(operator, lisptype.LispSymbol):
            if operator.name == 'QUOTE':
                return cadr(form)
            elif operator.name == 'IF':
                return eval_if(form, env)
            elif operator.name == 'SETQ':
                return eval_setq(form, env)
            elif operator.name == 'LET':
                return eval_let(form, env)
            elif operator.name == 'DEFUN':
                return eval_defun(form, env)
            elif operator.name == 'DEFVAR':
                return eval_defvar(form, env)
            elif operator.name == 'LAMBDA':
                return eval_lambda(form, env)
        
        # Regular function call
        func = eval(operator, env)
        if callable(func):
            # Evaluate all arguments
            args = []
            arg_list = cdr(form)
            while arg_list != lisptype.NIL and arg_list is not None:
                if isinstance(arg_list, lisptype.lispCons):
                    args.append(eval(car(arg_list), env))
                    arg_list = cdr(arg_list)
                else:
                    # Improper list
                    args.append(eval(arg_list, env))
                    break
            return func(*args)
        else:
            raise Exception(f"Not a function: {operator}")
    
    # Default case
    return form

def eval_if(form, env):
    """Evaluate IF special form: (if test then else)"""
    if_args = cdr(form)
    if if_args == lisptype.NIL:
        raise Exception("IF requires at least 2 arguments")
    
    test = car(if_args)
    then_part = cadr(if_args)
    else_part = caddr(if_args) if cdr(cdr(if_args)) != lisptype.NIL else lisptype.NIL
    
    test_result = eval(test, env)
    if test_result != lisptype.NIL and test_result is not None and test_result is not False:
        return eval(then_part, env)
    else:
        return eval(else_part, env)

def eval_setq(form, env):
    """Evaluate SETQ special form: (setq var value)"""
    args = cdr(form)
    if args == lisptype.NIL or cdr(args) == lisptype.NIL:
        raise Exception("SETQ requires 2 arguments")
    
    var = car(args)
    value_form = cadr(args)
    
    if not isinstance(var, lisptype.LispSymbol):
        raise Exception("SETQ first argument must be a symbol")
    
    value = eval(value_form, env)
    env.set_variable(var, value)
    return value

def eval_defvar(form, env):
    """Evaluate DEFVAR special form: (defvar var [value])"""
    args = cdr(form)
    if args == lisptype.NIL:
        raise Exception("DEFVAR requires at least 1 argument")
    
    var = car(args)
    if not isinstance(var, lisptype.LispSymbol):
        raise Exception("DEFVAR first argument must be a symbol")
    
    # Only set if not already bound
    if env.find_variable(var) is None:
        if cdr(args) != lisptype.NIL:
            value = eval(cadr(args), env)
            env.add_variable(var, value)
        else:
            env.add_variable(var, lisptype.NIL)
    
    return var

def eval_let(form, env):
    """Evaluate LET special form: (let ((var value) ...) body...)"""
    args = cdr(form)
    if args == lisptype.NIL:
        raise Exception("LET requires at least 1 argument")
    
    bindings = car(args)
    body = cdr(args)
    
    # Create new environment
    new_env = lisptype.Environment(env)
    
    # Process bindings
    while bindings != lisptype.NIL and bindings is not None:
        if isinstance(bindings, lisptype.lispCons):
            binding = car(bindings)
            if isinstance(binding, lisptype.lispCons):
                var = car(binding)
                value_form = cadr(binding)
                if isinstance(var, lisptype.LispSymbol):
                    value = eval(value_form, env)  # Evaluate in original environment
                    new_env.add_variable(var, value)
            bindings = cdr(bindings)
        else:
            break
    
    # Evaluate body in new environment
    result = lisptype.NIL
    while body != lisptype.NIL and body is not None:
        if isinstance(body, lisptype.lispCons):
            result = eval(car(body), new_env)
            body = cdr(body)
        else:
            result = eval(body, new_env)
            break
    
    return result

def eval_defun(form, env):
    """Evaluate DEFUN special form: (defun name (params...) body...)"""
    args = cdr(form)
    if args == lisptype.NIL or cdr(args) == lisptype.NIL:
        raise Exception("DEFUN requires at least 2 arguments")
    
    name = car(args)
    params = cadr(args)
    body = cddr(args)
    
    if not isinstance(name, lisptype.LispSymbol):
        raise Exception("DEFUN name must be a symbol")
    
    # Create a closure
    def user_function(*call_args):
        # Create new environment for function execution
        func_env = lisptype.Environment(env)
        
        # Bind parameters
        param_list = params
        arg_index = 0
        while param_list != lisptype.NIL and param_list is not None and arg_index < len(call_args):
            if isinstance(param_list, lisptype.lispCons):
                param = car(param_list)
                if isinstance(param, lisptype.LispSymbol):
                    func_env.add_variable(param, call_args[arg_index])
                    arg_index += 1
                param_list = cdr(param_list)
            else:
                break
        
        # Execute body
        result = lisptype.NIL
        body_forms = body
        while body_forms != lisptype.NIL and body_forms is not None:
            if isinstance(body_forms, lisptype.lispCons):
                result = eval(car(body_forms), func_env)
                body_forms = cdr(body_forms)
            else:
                result = eval(body_forms, func_env)
                break
        
        return result
    
    # Add function to environment
    env.add_function(name, user_function)
    return name

def eval_lambda(form, env):
    """Evaluate LAMBDA special form: (lambda (params...) body...)"""
    args = cdr(form)
    if args == lisptype.NIL:
        raise Exception("LAMBDA requires at least 1 argument")
    
    params = car(args)
    body = cdr(args)
    
    # Create a closure
    def lambda_function(*call_args):
        # Create new environment for function execution
        func_env = lisptype.Environment(env)
        
        # Bind parameters
        param_list = params
        arg_index = 0
        while param_list != lisptype.NIL and param_list is not None and arg_index < len(call_args):
            if isinstance(param_list, lisptype.lispCons):
                param = car(param_list)
                if isinstance(param, lisptype.LispSymbol):
                    func_env.add_variable(param, call_args[arg_index])
                    arg_index += 1
                param_list = cdr(param_list)
            else:
                break
        
        # Execute body
        result = lisptype.NIL
        body_forms = body
        while body_forms != lisptype.NIL and body_forms is not None:
            if isinstance(body_forms, lisptype.lispCons):
                result = eval(car(body_forms), func_env)
                body_forms = cdr(body_forms)
            else:
                result = eval(body_forms, func_env)
                break
        
        return result
    
    return lambda_function

def eval_when(situations, *forms):
    """Evaluate WHEN special form"""
    raise(Exception("Not implemented"))

def evenp(x):
    """Return T if x is an even integer, NIL otherwise"""
    if not isinstance(x, int):
        raise TypeError("evenp: expected an integer")
    return lisptype.LispSymbol("T") if x % 2 == 0 else lisptype.NIL

def every(predicate, *sequences):
    """Return T if predicate returns true for every element"""
    raise(Exception("Not implemented"))

def exp(x):
    """Return e raised to the power x"""
    return math.exp(x)

def export_symbol(symbols, package=None):
    """Export symbols from package"""
    raise(Exception("Not implemented"))

def expt(base, power):
    """Return base raised to the power"""
    return base ** power

def extended_char():
    """Extended character type"""
    raise(Exception("Not implemented"))

def fboundp(symbol):
    """Return T if symbol has a function binding"""
    raise(Exception("Not implemented"))

def fceiling(x, divisor=1):
    """Return ceiling as float"""
    return float(math.ceil(x / divisor))

def fdefinition(function_name):
    """Return function definition"""
    raise(Exception("Not implemented"))

def ffloor(x, divisor=1):
    """Return floor as float"""
    return float(math.floor(x / divisor))

#*features*
def deposit_field(newbyte, bytespec, integer):
    """Deposit field in integer"""
    raise(Exception("Not implemented"))

def mask_field(bytespec, integer):
    """Mask field in integer"""
    raise(Exception("Not implemented"))

def fifth(seq):
    """Get fifth element of sequence"""
    return car(cdr(cdr(cdr(cdr(seq)))))

def compile_file(input_file, output_file=None, **kwargs):
    """Compile file"""
    raise(Exception("Not implemented"))

def delete_file(filespec):
    """Delete file"""
    raise(Exception("Not implemented"))

def end_of_file():
    """End of file condition"""
    raise(Exception("Not implemented"))

def probe_file(pathspec):
    """Probe file existence"""
    raise(Exception("Not implemented"))

def rename_file(filespec, new_name):
    """Rename file"""
    raise(Exception("Not implemented"))

def with_open_file(*args):
    """With open file macro"""
    raise(Exception("Not implemented"))

def file_author(pathspec):
    """Get file author"""
    raise(Exception("Not implemented"))

def file_error():
    """File error condition"""
    raise(Exception("Not implemented"))

def file_error_pathname(condition):
    """Get pathname from file error"""
    raise(Exception("Not implemented"))

def file_length(stream):
    """Get file length"""
    raise(Exception("Not implemented"))

def file_namestring(pathspec):
    """Get file namestring"""
    raise(Exception("Not implemented"))

def compile_file_pathname(input_file, **kwargs):
    """Get compile file pathname"""
    raise(Exception("Not implemented"))

def file_position(stream, position=None):
    """Get or set file position"""
    raise(Exception("Not implemented"))

def file_stream():
    """File stream type"""
    raise(Exception("Not implemented"))

def file_string_length(stream, string):
    """Get file string length"""
    raise(Exception("Not implemented"))

def file_write_date(pathspec):
    """Get file write date"""
    raise(Exception("Not implemented"))

def fill(sequence, item, **kwargs):
    """Fill sequence with item"""
    raise(Exception("Not implemented"))

def pprint_fill(*args):
    """Pretty print fill"""
    raise(Exception("Not implemented"))

def fill_pointer(vector):
    """Get fill pointer"""
    raise(Exception("Not implemented"))

def array_has_fill_pointer_p(array):
    """Test if array has fill pointer"""
    raise(Exception("Not implemented"))

def find(item, sequence, **kwargs):
    """Find item in sequence"""
    raise(Exception("Not implemented"))

#compile-file
#delete-file
#end-of-file
#probe-file
#rename-file
#with-open-file
#compile-file-pathname
#*compile-file-pathname*
#*compile-file-truename*
def find_all_symbols(symbol):
    """Find all symbols"""
    raise(Exception("Not implemented"))

def find_class(symbol, errorp=True, environment=None):
    """Find class"""
    raise(Exception("Not implemented"))

def find_if(predicate, sequence, **kwargs):
    """Find if predicate is true"""
    raise(Exception("Not implemented"))

def find_if_not(predicate, sequence, **kwargs):
    """Find if predicate is not true"""
    raise(Exception("Not implemented"))

def find_method(generic_function, qualifiers, specializers, errorp=True):
    """Find method"""
    raise(Exception("Not implemented"))

def find_package(name):
    """Find package"""
    raise(Exception("Not implemented"))

def find_restart(identifier, condition=None):
    """Find restart"""
    raise(Exception("Not implemented"))

def find_symbol(string, package=None):
    """Find symbol"""
    raise(Exception("Not implemented"))

def loop_finish():
    """Loop finish"""
    raise(Exception("Not implemented"))

def finish_output(stream=None):
    """Finish output"""
    raise(Exception("Not implemented"))
# def first
def first(seq):
    """Get first element of sequence (same as CAR)."""
    return car(seq)

def second(seq):
    """Get second element of sequence."""
    return cadr(seq)

def third(seq):
    """Get third element of sequence."""
    return caddr(seq)

def fourth(seq):
    """Get fourth element of sequence."""
    return car(cdr(cdr(cdr(seq))))

def fixnum():
    """Fixnum type"""
    raise(Exception("Not implemented"))

def most_negative_fixnum():
    """Most negative fixnum constant"""
    raise(Exception("Not implemented"))

def most_positive_fixnum():
    """Most positive fixnum constant"""
    raise(Exception("Not implemented"))

def flet(*args):
    """FLET special form"""
    raise(Exception("Not implemented"))

def decode_float(float_num):
    """Decode float"""
    raise(Exception("Not implemented"))

def double_float():
    """Double float type"""
    raise(Exception("Not implemented"))

def float_fn(number, prototype=None):
    """Convert to float"""
    return float(number)

def integer_decode_float(float_num):
    """Integer decode float"""
    raise(Exception("Not implemented"))

def scale_float(float_num, integer):
    """Scale float"""
    raise(Exception("Not implemented"))

def short_float():
    """Short float type"""
    raise(Exception("Not implemented"))

def single_float():
    """Single float type"""
    raise(Exception("Not implemented"))

def float_digits(float_num):
    """Get float digits"""
    raise(Exception("Not implemented"))

def float_precision(float_num):
    """Get float precision"""
    raise(Exception("Not implemented"))

def float_radix(float_num):
    """Get float radix"""
    raise(Exception("Not implemented"))

def float_sign(float1, float2=None):
    """Get float sign"""
    raise(Exception("Not implemented"))

def floating_point_inexact():
    """Floating point inexact condition"""
    raise(Exception("Not implemented"))

def floating_point_invalid_operation():
    """Floating point invalid operation condition"""
    raise(Exception("Not implemented"))

def floating_point_overflow():
    """Floating point overflow condition"""
    raise(Exception("Not implemented"))

def floating_point_underflow():
    """Floating point underflow condition"""
    raise(Exception("Not implemented"))

def floatp(x):
    """Test if x is a float"""
    return isinstance(x, float)

def floor(x, divisor=1):
    """Return floor of x/divisor"""
    return math.floor(x / divisor)

def fmakunbound(function_name):
    """Make function unbound"""
    raise(Exception("Not implemented"))

def update_instance_for_different_class(previous, current, **kwargs):
    """Update instance for different class"""
    raise(Exception("Not implemented"))

def update_instance_for_redefined_class(instance, **kwargs):
    """Update instance for redefined class"""
    raise(Exception("Not implemented"))

def force_output(stream=None):
    """Force output"""
    raise(Exception("Not implemented"))

def make_load_form(object, environment=None):
    """Make load form"""
    raise(Exception("Not implemented"))

def make_load_form_saving_slots(object, **kwargs):
    """Make load form saving slots"""
    raise(Exception("Not implemented"))

def format_fn(destination, control_string, *args):
    """Format function"""
    raise(Exception("Not implemented"))

def stream_external_format(stream):
    """Get stream external format"""
    raise(Exception("Not implemented"))

def simple_condition_format_arguments(condition):
    """Get simple condition format arguments"""
    raise(Exception("Not implemented"))

def simple_condition_format_control(condition):
    """Get simple condition format control"""
    raise(Exception("Not implemented"))

def formatter(control_string):
    """Create formatter function"""
    raise(Exception("Not implemented"))
#integer-decode-float
#least-negative-double-float
#least-negative-long-float
# least-negative-normalized-double-float
#least-negative-normalized-long-float
#least-negative-normalized-short-float
# least-negative-normalized-single-float
#least-negative-short-float
#least-negative-single-float
#least-positive-double-float
#least-positive-long-float
# least-positive-normalized-double-float
#least-positive-normalized-long-float
#least-positive-normalized-short-float
# least-positive-normalized-single-float
#least-positive-short-float
#least-positive-single-float
#long-float
#most-negative-double-float
#most-negative-long-float
#most-negative-short-float
#most-negative-single-float
#most-positive-double-float
#most-positive-long-float
#most-positive-short-float
#most-positive-single-float
#scale-float
#short-float
#single-float
# def float-digits
#double-float-epsilon
#long-float-epsilon
#short-float-epsilon
#single-float-epsilon
#*read-default-float-format*
#double-float-negative-epsilon
#long-float-negative-epsilon
#short-float-negative-epsilon
#single-float-negative-epsilon
# def float-precision
# def float-radix
# def float-sign
# def floating-point-inexact
# def floating-point-invalid-operation
# def floating-point-overflow
# def floating-point-underflow
# def floatp
# def floor
# def fmakunbound
#update-instance-for-different-class
#update-instance-for-redefined-class
# def force-output
#make-load-form
#make-load-form-saving-slots
# def format
#stream-external-format
#*read-default-float-format*
#simple-condition-format-arguments
#simple-condition-format-control
# def formatter
# def fourth
# def fresh-line
#return-from
#set-syntax-from-char
#read-from-string
#with-input-from-string

def fround(x, divisor=1):
    """Return rounded value as float"""
    import builtins
    return float(builtins.round(x / divisor))

def ftruncate(x, divisor=1):
    """Return truncated value as float"""
    return float(int(x / divisor))

# def ftype
# def funcall
#compiled-function
#compiler-macro-function
#ensure-generic-function
# def function
#generic-function
#macro-function
#standard-generic-function
#symbol-function
#undefined-function
# def function-keywords
# def function-lambda-expression
def compiled_function_p(object):
    """Test if object is a compiled function"""
    raise(Exception("Not implemented"))

def functionp(object):
    """Test if object is a function"""
    return callable(object)

def gcd(*integers):
    """Greatest common divisor"""
    if len(integers) == 0:
        return 0
    result = integers[0]
    for i in integers[1:]:
        result = math.gcd(result, i)
    return result

def generic_function():
    """Generic function type"""
    raise(Exception("Not implemented"))

def gentemp(prefix="T", package=None):
    """Generate temporary symbol"""
    raise(Exception("Not implemented"))

def get(symbol, indicator, default=None):
    """Get property"""
    raise(Exception("Not implemented"))

def get_decoded_time():
    """Get decoded time"""
    raise(Exception("Not implemented"))

def get_dispatch_macro_character(disp_char, sub_char, readtable=None):
    """Get dispatch macro character"""
    raise(Exception("Not implemented"))

def get_internal_real_time():
    """Get internal real time"""
    raise(Exception("Not implemented"))

def get_internal_run_time():
    """Get internal run time"""
    raise(Exception("Not implemented"))

def get_output_stream_string(stream):
    """Get output stream string"""
    raise(Exception("Not implemented"))

# def gentemp
# def get
# def get-decoded-time
# def get-dispatch-macro-character
# def get-internal-real-time
# def get-internal-run-time
# def get-macro-character
# def get-output-stream-string
_gensym_counter = 0

def gensym(prefix="G"):
    """Generate a unique symbol."""
    global _gensym_counter
    _gensym_counter += 1
    return lisptype.LispSymbol(f"{prefix}{_gensym_counter}")

# def gentemp
# def get
# def get-decoded-time
# def get-dispatch-macro-character
# def get-internal-real-time
# def get-internal-run-time
# def get-macro-character
# def get-output-stream-string
# def get-properties
# def get-setf-expansion
# def get-universal-time
# def getf
# def gethash
# def go
def graphic_char_p(character):
    """Test if character is graphic"""
    raise(Exception("Not implemented"))

def handler_bind(*args):
    """Handler bind special form"""
    raise(Exception("Not implemented"))

def handler_case(*args):
    """Handler case special form"""
    raise(Exception("Not implemented"))

def hash_table():
    """Hash table type"""
    raise(Exception("Not implemented"))

def hash_table_count(hash_table):
    """Get hash table count"""
    raise(Exception("Not implemented"))

def hash_table_p(object):
    """Test if object is hash table"""
    raise(Exception("Not implemented"))

def hash_table_rehash_size(hash_table):
    """Get hash table rehash size"""
    raise(Exception("Not implemented"))

def hash_table_rehash_threshold(hash_table):
    """Get hash table rehash threshold"""
    raise(Exception("Not implemented"))

def hash_table_size(hash_table):
    """Get hash table size"""
    raise(Exception("Not implemented"))

def hash_table_test(hash_table):
    """Get hash table test"""
    raise(Exception("Not implemented"))

def host_namestring(pathname):
    """Get host namestring"""
    raise(Exception("Not implemented"))

def identity(object):
    """Return object unchanged"""
    return object

def if_fn(*args):
    """IF special form"""
    raise(Exception("Not implemented"))

def ignorable(*vars):
    """Ignorable declaration"""
    raise(Exception("Not implemented"))

def ignore(*vars):
    """Ignore declaration"""
    raise(Exception("Not implemented"))

def ignore_errors(*forms):
    """Ignore errors macro"""
    raise(Exception("Not implemented"))

def imagpart(number):
    """Get imaginary part"""
    if isinstance(number, complex):
        return number.imag
    return 0

def import_symbol(symbols, package=None):
    """Import symbols"""
    raise(Exception("Not implemented"))

def in_package(name):
    """In package macro"""
    raise(Exception("Not implemented"))

def incf(place, delta=1):
    """Increment macro"""
    raise(Exception("Not implemented"))

def initialize_instance(instance, **kwargs):
    """Initialize instance"""
    raise(Exception("Not implemented"))

def inline_decl(*functions):
    """Inline declaration"""
    raise(Exception("Not implemented"))

def input_stream_p(stream):
    """Test if stream is input stream"""
    raise(Exception("Not implemented"))

def inspect(object):
    """Inspect object"""
    raise(Exception("Not implemented"))

def integer():
    """Integer type"""
    raise(Exception("Not implemented"))

def integer_length(integer):
    """Get integer length"""
    return integer.bit_length()

def integerp(object):
    """Test if object is integer"""
    return isinstance(object, int)

def interactive_stream_p(stream):
    """Test if stream is interactive"""
    raise(Exception("Not implemented"))

def intern(string, package=None):
    """Intern symbol"""
    raise(Exception("Not implemented"))

def internal_time_units_per_second():
    """Internal time units per second constant"""
    raise(Exception("Not implemented"))
def intersection(list1, list2, **kwargs):
    """Set intersection"""
    raise(Exception("Not implemented"))

def invalid_method_error(method, format_control, *args):
    """Invalid method error"""
    raise(Exception("Not implemented"))

def invoke_debugger(condition):
    """Invoke debugger"""
    raise(Exception("Not implemented"))

def invoke_restart(restart, *args):
    """Invoke restart"""
    raise(Exception("Not implemented"))

def invoke_restart_interactively(restart):
    """Invoke restart interactively"""
    raise(Exception("Not implemented"))

def isqrt(natural):
    """Integer square root"""
    return int(math.sqrt(natural))

def keyword():
    """Keyword type"""
    raise(Exception("Not implemented"))

def keywordp(object):
    """Test if object is keyword"""
    raise(Exception("Not implemented"))

def function_keywords(function):
    """Get function keywords"""
    raise(Exception("Not implemented"))

def lambda_list_keywords():
    """Lambda list keywords constant"""
    raise(Exception("Not implemented"))

def labels(*args):
    """LABELS special form"""
    raise(Exception("Not implemented"))

def lambda_fn(*args):
    """LAMBDA special form"""
    raise(Exception("Not implemented"))

def function_lambda_expression(function):
    """Get function lambda expression"""
    raise(Exception("Not implemented"))

def lambda_parameters_limit():
    """Lambda parameters limit constant"""
    raise(Exception("Not implemented"))

def lcm(*integers):
    """Least common multiple"""
    if len(integers) == 0:
        return 1
    result = integers[0]
    for i in integers[1:]:
        result = abs(result * i) // math.gcd(result, i)
    return result
def last(lst):
    """Return the last element of a list."""
    if not listp(lst):
        raise TypeError("last: expected a list")
    while lst is not None:
        if lst.cdr is None:
            return lst.car
        lst = lst.cdr
    return None

# def lcm
# def ldb
# def ldb-test
# def ldiff
# def least-negative-double-float
# def least-negative-long-float
# def least-negative-normalized-double-float
# def least-negative-normalized-long-float
# def least-negative-normalized-short-float
# def least-negative-normalized-single-float
# def least-negative-short-float
# def least-negative-single-float
# def least-positive-double-float
# def least-positive-long-float
# def least-positive-normalized-double-float
# def least-positive-normalized-long-float
# def least-positive-normalized-short-float
# def least-positive-normalized-single-float
# def least-positive-short-float
# def least-positive-single-float
# string-left-trim
# file-length
# file-string-length
# integer-length
def length(x):
    """Return the length of a list."""
    if not listp(x):
        raise TypeError("length: expected a list")
    count = 0
    while x is not None:
        count += 1
        x = x.cdr
    return count

# def list-length
# *print-length*
# char-lessp
# char-not-lessp
# string-lessp
# string-not-lessp
# def let
# def let*
# *print-level*
# array-dimension-limit
# array-rank-limit
# array-total-size-limit
# call-arguments-limit
# char-code-limit
# def lambda-parameters-limit
# multiple-values-limit
# fresh-line
# read-line
# write-line
# pprint-linear
# *print-lines*
# def fclpy-implementation-type
# def fclpy-implementation-version
# apropos-list
# copy-list

def list(*args):
    lst = lisptype.NIL
    cur = lisptype.NIL
    for i in args:    
        if cur == lisptype.NIL:
            cur = lisptype.lispCons(i)
        else:
            cur.cdr = lisptype.lispCons(i)
            cur = cur.cdr
        if lst == lisptype.NIL:
            lst = cur
    return lst 

# make-list
# multiple-value-list
# package-use-list
# package-used-by-list
# read-delimited-list
# values-list
def list_s_star_(*args):
    if len(args) == 1:
        return args[0]
    lst = None
    cur = None
    for i in args[:-1]:    
        if cur == None:
            cur = lisptype.lispCons(i)
        else:
            cur.cdr = lisptype.lispCons(i)
            cur = cur.cdr
        if lst == None:
            lst = cur
    if listp(args[-1]):
        if type(args[-1]) is tuple:
            cur.cdr = lisptype.lispCons(args[-1][0],args[-1][1:])
        else:
            cur.cdr = args[-1]
    else:
        cur.cdr = args[-1]
    return lst

# def list-all-packages
# pprint-exit-if-list-exhausted
# def lambda-list-keywords
# def list-length
# def listen

def listp(x):
    return x == None or type(x) == tuple or isinstance(x,lisptype.lispList)

# def load
# make-load-form
# make-load-form-saving-slots
# def load-logical-pathname-translations
# *load-pathname*
# *load-print*
# def load-time-value
# *load-truename*
# *load-verbose*
# def locally
# def log
# def logand
# def logandc1
# def logandc2
# def logbitp
# def logcount
# def logeqv
# pprint-logical-block
# def logical-pathname
# translate-logical-pathname
# def load-logical-pathname-translations
# def logical-pathname-translations
# def logior
# def lognand
# def lognor
# def lognot
# def logorc1
# def logorc2
# def logtest
# def logxor
# def least-negative-long-float
# least-negative-normalized-long-float
# def least-positive-long-float
# least-positive-normalized-long-float
# def long-float
# most-negative-long-float
# most-positive-long-float
# def long-float-epsilon
# def long-float-negative-epsilon
# def long-site-name
# def loop
# def loop-finish
# def lower-case-p
# def # def machine-instance
# def machine-type
# def machine-version
# 	  compiler-macro
#   define-compiler-macro
#  define-modify-macro
#  define-symbol-macro
#   get-dispatch-macro-character
# 		   get-macro-character
# def make-dispatch-macro-character
#   set-dispatch-macro-character
# 		   set-macro-character
# 	  compiler-macro-function
# def macro-function
# def macroexpand
# def macroexpand-1
# 			  *macroexpand-hook*
# def macrolet
# 		symbol-macrolet
# 		   row-major-aref
# 	 array-row-major-index
# def make-array
# def make-broadcast-stream
# def make-concatenated-stream
# def make-condition
# def make-dispatch-macro-character
# def make-echo-stream
# def make-hash-table
# def make-instance
# def make-instances-obsolete
# def make-list
# def make-load-form
# def make-load-form-saving-slots
# def make-method
# def make-package
# def make-pathname
# def make-random-state
# def make-sequence
# def make-string
# def make-string-input-stream
# def make-string-output-stream
# def make-symbol
# def make-synonym-stream
# def make-two-way-stream
# def makunbound
# 		  slot-makunbound
# def map
# def map-into
# def mapc
# def mapcan
# def mapcar
# def mapcon
# def maphash
# def mapl
# def maplist
#   *print-right-margin*
# def mask-field
# 	  pathname-match-p
def max_fn(*numbers):
    """Return maximum of numbers"""
    return max(numbers)

def member(item, list, **kwargs):
    """Find member in list"""
    raise(Exception("Not implemented"))

def member_if(predicate, list, **kwargs):
    """Find member if predicate is true"""
    raise(Exception("Not implemented"))

def member_if_not(predicate, list, **kwargs):
    """Find member if predicate is not true"""
    raise(Exception("Not implemented"))

def merge(result_type, sequence1, sequence2, predicate, **kwargs):
    """Merge sequences"""
    raise(Exception("Not implemented"))

def merge_pathnames(pathname, default_pathname=None, **kwargs):
    """Merge pathnames"""
    raise(Exception("Not implemented"))

def min_fn(*numbers):
    """Return minimum of numbers"""
    return min(numbers)

def minusp(number):
    """Test if number is negative"""
    return number < 0

def mismatch(sequence1, sequence2, **kwargs):
    """Find mismatch between sequences"""
    raise(Exception("Not implemented"))

def slot_missing(class_, object, slot_name, operation, **kwargs):
    """Slot missing method"""
    raise(Exception("Not implemented"))

def mod(number, divisor):
    """Modulo operation"""
    return number % divisor

def define_modify_macro(*args):
    """Define modify macro"""
    raise(Exception("Not implemented"))

def most_negative_double_float():
    """Most negative double float constant"""
    raise(Exception("Not implemented"))

def most_negative_long_float():
    """Most negative long float constant"""
    raise(Exception("Not implemented"))

def most_negative_short_float():
    """Most negative short float constant"""
    raise(Exception("Not implemented"))
# 		   add-method
# 		  call-method
# 	 call-next-method
# 		  find-method
# def make-method
# def method
#  no-applicable-method
# 	   no-next-method
# 		remove-method
# 	  standard-method
# 		define-method-combination
# def method-combination
# def method-combination-error
# 	   invalid-method-error
# 		  next-method-p
# def method-qualifiers
# compute-applicable-methods
# def min
# def minusp
# 		*print-miser-width*
# def mismatch
# 		  slot-missing
# def mod
# 		define-modify-macro
# 			  *modules*
# def most-negative-double-float
# def most-negative-fixnum
# def most-negative-long-float
# def most-negative-short-float
# def most-negative-single-float
# def most-positive-double-float
# def most-positive-fixnum
# def most-positive-long-float
# def most-positive-short-float
# def most-positive-single-float
# def muffle-warning
# def multiple-value-bind
# def multiple-value-call
# def multiple-value-list
# def multiple-value-prog1
# def multiple-value-setq
# def multiple-values-limit
# def # def name-char
# def namestring
# def nbutlast
# def nconc
# def next-method-p
# def nil
# def nintersection
# def ninth
# def no-applicable-method
# def no-next-method
# def not
def not_fn(x):
    """Logical NOT - returns T if x is NIL, otherwise NIL."""
    if x == lisptype.NIL or x is None or x is False:
        return lisptype.LispSymbol("T")
    else:
        return lisptype.NIL

# def notany
# def notevery
# def notinline
# def nreconc
# def nreverse
# def nset-difference
# def nset-exclusive-or
# def nstring-capitalize
# def nstring-downcase
# def nstring-upcase
# def nsublis
# def nsubst
# def nsubst-if
# def nsubst-if-not
# def nsubstitute
# def nsubstitute-if
# def nsubstitute-if-not
# def nth
# def nth-value
# def nthcdr
def null(lst):
    """Return T if the list is empty, otherwise NIL."""
    if not listp(lst):
        raise TypeError("null: expected a list")
    return lisptype.LispSymbol("T") if lst is None else lisptype.NIL

# def number
# def numberp
# def numerator
# def nunion 
# def # describe-object
# print-not-readable-object
# print-object
# print-unreadable-object
# standard-object
# structure-object
# make-instances-obsolete
def oddp(integer):
    """Test if integer is odd"""
    if not isinstance(integer, int):
        raise TypeError("oddp: expected an integer")
    return integer % 2 == 1
# class-of
# type-of
# end-of-file
# *break-on-signals*
# def open
# with-open-file
# with-open-stream
# def open-stream-p
# arithmetic-error-operands
# arithmetic-error-operation
# floating-point-invalid-operation
# special-operator-p
# def optimize
# &optional
# nset-exclusive-or
# def or
# set-exclusive-or
# y-or-n-p
# yes-or-no-p
# bit-orc1
# boole-orc1
# bit-orc2
# boole-orc2
# &allow-other-keys
# def otherwise
# clear-output
# finish-output
# force-output
# *error-output*
# *standard-output*
# *trace-output*
# echo-stream-output-stream
# make-string-output-stream
# two-way-stream-output-stream
# def output-stream-p
# get-output-stream-string
# with-output-to-string
# floating-point-overflow
# def # def package
# def package-error
# def package-error-package
# def package-name
# def package-nicknames
# def package-shadowing-symbols
# def package-use-list
# def package-used-by-list
# def packagep
# def pairlis
# def parse-error
# def parse-integer
# def parse-namestring
# def pathname
# def pathname-device
# def pathname-directory
# def pathname-host
# def pathname-match-p
# def pathname-name
# def pathname-type
# def pathname-version
# def pathnamep
# def peek-char
# def phase
# def pi
def plusp(number):
    """Test if number is positive"""
    return number > 0
# def pop
# def position
# def position-if
# def position-if-not
# def pprint
# def pprint-dispatch
# def pprint-exit-if-list-exhausted
# def pprint-fill
# def pprint-indent
# def pprint-linear
# def pprint-logical-block
# def pprint-newline
# def pprint-pop
# def pprint-tab
# def pprint-tabular
# def prin1
# def prin1-to-string
# def princ
# def princ-to-string
def _s_print_(x):
    print(x)
    return lisptype.NIL
# def print-not-readable
# def print-not-readable-object
# def print-object
# def print-unreadable-object
# def probe-file
# def proclaim
# def prog
# def prog*
# def prog1
# def prog2
# def progn
# def program-error
# def progv
# def provide
# def psetf
# def psetq
# def push 
# def # method-qualifiers
# *query-io*
class quote(lisptype.SpecialForm):
    def __call__(self,obj):
        return obj
    
# def # float-radix
# *print-radix*
# def random
# make-random-state
# def random-state
# *random-state*
# def random-state-p
# array-rank
# array-rank-limit
# def rassoc
# def rassoc-if
# def rassoc-if-not
# def ratio
# def rational
# def rationalize
# def rationalp
def read(stream=None):
    if stream == None:
        r = lispreader.LispReader(get_macro_character)
    else:
        r = lispreader.LispReader(get_macro_character,stream)
    return r.read_1()
   
# *read-base*
# def read-byte
# def read-char
# def read-char-no-hang
# *read-default-float-format*
# def read-delimited-list
# *read-eval*
# def read-from-string
# def read-line
# def read-preserving-whitespace
# def read-sequence
# *read-suppress*
# print-not-readable
# print-not-readable-object
# *print-readably*
# def reader-error
# copy-readtable
# def readtable
# *readtable*
# def readtable-case
# def readtablep
# def real
# get-internal-real-time
# def realp
# def realpart
# update-instance-for-redefined-class
# def reduce
# hash-table-rehash-size
# hash-table-rehash-threshold
# def reinitialize-instance
# def rem
# def remf
# def remhash
# def remove
# def remove-duplicates
# def remove-if
# def remove-if-not
# def remove-method
# def remprop
# def rename-file
# def rename-package
# def replace
# def require
# &rest
def rest(seq):
    return tuple(seq[1:])

# def # find-restart
# invoke-restart
# def restart
# with-simple-restart
# def restart-bind
# def restart-case
# invoke-restart-interactively
# def restart-name
# compute-restarts
# with-condition-restarts
# def return
# def return-from
# def revappend
def reverse(x):
    """Return a new list that is the reverse of the input list."""
    if not listp(x):
        raise TypeError("reverse: expected a list")
    reversed_list = lisptype.NIL
    while x is not None:
        reversed_list = lisptype.lispCons(x.car, reversed_list)
        x = x.cdr
    return reversed_list

# *print-right-margin*
# string-right-trim
# def room
# def rotatef

def round(number, divisor=1):
    """Return rounded value as integer"""
    import builtins
    return int(builtins.round(number / divisor))

# def row-major-aref
# array-row-major-index

def rplaca(cons, obj):
    cons.car = obj
    return cons

def rplacd(cons,obj):
    cons.cdr = obj
    return cons

# get-internal-run-time
# def # def safety
# def satisfies
# make-load-form-saving-slots
# def sbit
# def scale-float
# def schar
# def search
# internal-time-units-per-second
# def second
# copy-seq
# make-sequence
# read-sequence
# def sequence
# write-sequence
# def serious-condition
# boole-set
# def set
# def set-difference
# def set-dispatch-macro-character
# def set-exclusive-or
# def set-macro-character
# def set-pprint-dispatch
# def set-syntax-from-char
# def setf
# define-setf-expander
# get-setf-expansion
# multiple-value-setq
# def setq
# def seventh
# def shadow
# def shadowing-import
# package-shadowing-symbols
# def shared-initialize
# def shiftf
# least-negative-normalized-short-float
# least-negative-short-float
# least-positive-normalized-short-float
# least-positive-short-float
# most-negative-short-float
# most-positive-short-float
# def short-float
# def short-float-epsilon
# def short-float-negative-epsilon
# def short-site-name
# float-sign
# def signal
# *break-on-signals*
# def signed-byte
# def signum
# def simple-array
# def simple-base-string
# def simple-bit-vector
# def simple-bit-vector-p
# def simple-condition
# def simple-condition-format-arguments
# def simple-condition-format-control
# def simple-error
# with-simple-restart
# def simple-string
# def simple-string-p
# def simple-type-error
# def simple-vector
# def simple-vector-p
# def simple-warning
# def sin
# least-negative-normalized-single-float
# least-negative-single-float
# least-positive-normalized-single-float
# least-positive-single-float
# most-negative-single-float
# most-positive-single-float
# def single-float
# def single-float-epsilon
# def single-float-negative-epsilon
# def sinh
# long-site-name
# def short-site-name
# def sixth
# array-total-size
# byte-size
# hash-table-rehash-size
# hash-table-size
# array-total-size-limit
# def sleep
# unbound-slot
# def slot-boundp
# def slot-exists-p
# unbound-slot-instance
# def slot-makunbound
# def slot-missing
# def slot-unbound
# def slot-value
# make-load-form-saving-slots
# with-slots
# def software-type
# def software-version
# def some
# def sort
# def stable-sort
# def space
# def special
# def special-operator-p
# compilation-speed
# def speed
# def sqrt
# def stable-sort
# def standard
# def standard-char
# def standard-char-p
# def standard-class
# def standard-generic-function
# *standard-input*
# with-standard-io-syntax
# def standard-method
# def standard-object
# *standard-output*
# make-random-state
# random-state
# *random-state*
# random-state-p
# def step
# def storage-condition
# def store-value
# broadcast-stream
# concatenated-stream
# echo-stream
# file-stream
# make-broadcast-stream
# make-concatenated-stream
# make-echo-stream
# make-string-input-stream
# make-string-output-stream
# make-synonym-stream
# make-two-way-stream
# def stream
# def string-stream
# def synonym-stream
# two-way-stream
# with-open-stream
# def stream-element-type
# def stream-error
# def stream-error-stream
# def stream-external-format
# echo-stream-input-stream
# two-way-stream-input-stream
# echo-stream-output-stream
# two-way-stream-output-stream
# input-stream-p
# interactive-stream-p
# open-stream-p
# output-stream-p
# broadcast-stream-streams
# concatenated-stream-streams
# get-output-stream-string
# def synonym-stream-symbol
# def streamp
# broadcast-stream-streams
# concatenated-stream-streams
# base-string
# get-output-stream-string
# make-string
# prin1-to-string
# princ-to-string
# read-from-string
# def simple-base-string
# def simple-string
# def string
# with-input-from-string
# with-output-to-string
# write-string
# write-to-string
# def string-capitalize
# def string-downcase
# def string-equal
# def string-greaterp
# make-string-input-stream
# def string-left-trim
# file-string-length
# def string-lessp
# def string-not-equal
# def string-not-greaterp
# def string-not-lessp
# make-string-output-stream
# def simple-string-p
# def string-right-trim
# def string-stream
# def string-trim
# def string-upcase
# def string/=
# def string<
# def string<=
# def string=
# def string>
# def string>=
# def stringp
# copy-structure
# def structure
# def structure-class
# def structure-object
# def style-warning
# def sublis
# def subseq
# def subsetp
# def subst
# def subst-if
# def subst-if-not
# def substitute
# def substitute-if
# def substitute-if-not
# def subtypep
# *read-suppress*
# def svref
# def sxhash
# copy-symbol
# find-symbol
# make-symbol
# def symbol
# def synonym-stream-symbol
# def symbol-function
# define-symbol-macro
# def symbol-macrolet
# def symbol-name
# def symbol-package
# def symbol-plist
# def symbol-value
# def symbolp
def symbolp(x):
    """Test if x is a symbol."""
    return isinstance(x, lisptype.LispSymbol)

def stringp(x):
    """Test if x is a string."""
    return isinstance(x, str)

def numberp(x):
    """Test if x is a number."""
    return isinstance(x, (int, float))

# do-all-symbols
# do-external-symbols
# do-symbols
# find-all-symbols
# package-shadowing-symbols
# make-synonym-stream
# def synonym-stream
# def synonym-stream-symbol
# with-standard-io-syntax
# def set-syntax-from-char
# def t
# def tagbody
# def tailp
# def tan
# def tanh
# def tenth
# def terpri
# def the
# def third
# def throw
# def time
# def trace
# def translate-logical-pathname
# def translate-pathname
# def tree-equal
# def truename

def truncate(number, divisor=1):
    """Return truncated value as integer"""
    return int(number / divisor)

# def two-way-stream
# def two-way-stream-input-stream
# def two-way-stream-output-stream
# def type
# def type-error
# def type-error-datum
# def type-error-expected-type
# def type-of
# def typecase
# def typep 



# unbound-slot
# unbound-slot-instance
# unbound-variable
# undefined-function
# unexport
# unintern
# union
# unless
# unread-char
# unsigned-byte
# untrace
# unuse-package
# unwind-protect
# update-instance-for-different-class
# update-instance-for-redefined-class
# upgraded-array-element-type
# upgraded-complex-part-type
# upper-case-p
# use-package
# use-value
# user-homedir-pathname 
# load-time-value
# nth-value
# slot-value
# store-value
# symbol-value
# use-value
# multiple-value-bind
# multiple-value-call
# multiple-value-list
# multiple-value-prog1
# multiple-value-setq
# def values
# multiple-values-limit
# def values-list
# unbound-variable
# def variable
# bit-vector
# simple-bit-vector
# simple-vector
# def vector
# bit-vector-p
# simple-bit-vector-p
# simple-vector-p
# def vector-pop
# def vector-push
# def vector-push-extend
# def vectorp
# *compile-verbose*
# *load-verbose*
# fclpy-implementation-version
# machine-version
# pathname-version
# software-version
# # def warn
# muffle-warning
# simple-warning
# style-warning
# def warning
# make-two-way-stream
# two-way-stream
# two-way-stream-input-stream
# two-way-stream-output-stream
# eval-when
# def when
# read-preserving-whitespace
# &whole
# *print-miser-width*
# def wild-pathname-p
# def with-accessors
# def with-compilation-unit
# def with-condition-restarts
# def with-hash-table-iterator
# def with-input-from-string
# def with-open-file
# def with-open-stream
# def with-output-to-string
# def with-package-iterator
# def with-simple-restart
# def with-slots
# def with-standard-io-syntax
# def write
# def write-byte
# def write-char
# file-write-date
# def write-line
# def write-sequence
# def write-string
# def write-to-string


# def bit-xor
# def boole-xor


# def y-or-n-p
# def yes-or-no-p 
    
def zerop(number):
    """Test if number is zero"""
    return number == 0

# def zerop
    

# def _s_amp_allow-other-keys
# def _s_amp_aux
# def _s_amp_body
# def _s_amp_environment
# def _s_amp_key
# def _s_amp_optional
# def _s_amp_rest
# def _s_amp_whole
# def _s_star_
# def _s_star__s_star_
# def _s_star__s_star__s_star_
# def _s_star_break-on-signals_s_star_
# def _s_star_compile-file-pathname_s_star_
# def _s_star_compile-file-truename_s_star_
# def _s_star_compile-print_s_star_
# def _s_star_compile-verbose_s_star_
# def _s_star_debug-io_s_star_
# def _s_star_debugger-hook_s_star_
# def _s_star_default-pathname-defaults_s_star_
# def _s_star_error-output_s_star_
# def _s_star_features_s_star_
# def _s_star_gensym-counter_s_star_
# def _s_star_load-pathname_s_star_
# def _s_star_load-print_s_star_
# def _s_star_load-truename_s_star_
# def _s_star_load-verbose_s_star_
# def _s_star_macroexpand-hook_s_star_
# def _s_star_modules_s_star_
# def _s_star_package_s_star_
# def _s_star_print-array_s_star_
# def _s_star_print-base_s_star_
# def _s_star_print-case_s_star_
# def _s_star_print-circle_s_star_
# def _s_star_print-escape_s_star_
# def _s_star_print-gensym_s_star_
# def _s_star_print-length_s_star_
# def _s_star_print-level_s_star_
# def _s_star_print-lines_s_star_
# def _s_star_print-miser-width_s_star_
# def _s_star_print-pprint-dispatch_s_star_
# def _s_star_print-pretty_s_star_
# def _s_star_print-radix_s_star_
# def _s_star_print-readably_s_star_
# def _s_star_print-right-margin_s_star_
# def _s_star_query-io_s_star_
# def _s_star_random-state_s_star_
# def _s_star_read-base_s_star_
# def _s_star_read-default-float-format_s_star_
# def _s_star_read-eval_s_star_
# def _s_star_read-suppress_s_star_
# def _s_star_readtable_s_star_
# def _s_star_standard-input_s_star_
# def _s_star_standard-output_s_star_
# def _s_star_terminal-io_s_star_
# def _s_star_trace-output_s_star_

def _s_plus_(*args):
    """Addition operator (+) - variadic"""
    return sum(args)

def _s_minus_(*args):
    """Subtraction operator (-) - variadic"""
    if len(args) == 0:
        raise ValueError("- requires at least one argument")
    if len(args) == 1:
        return -args[0]
    result = args[0]
    for arg in args[1:]:
        result -= arg
    return result

def _s_star_(*args):
    """Multiplication operator (*) - variadic"""
    result = 1
    for arg in args:
        result *= arg
    return result

def _s_slash_(*args):
    """Division operator (/) - variadic"""
    if len(args) == 0:
        raise ValueError("/ requires at least one argument")
    if len(args) == 1:
        return 1.0 / args[0]
    result = args[0]
    for arg in args[1:]:
        result /= arg
    return result

def _s_eq_(x, y):
    """Numeric equality (=)"""
    return x == y

def _s_lt_(x, y):
    """Less than (<)"""
    return x < y

def _s_gt_(x, y):
    """Greater than (>)"""
    return x > y

def _s_le_(x, y):
    """Less than or equal (<=)"""
    return x <= y

def _s_ge_(x, y):
    """Greater than or equal (>=)"""
    return x >= y

# def _s_plus__s_plus_
# def _s_plus__s_plus__s_plus_
# def _s_minus_
# def _s_slash_
# def _s_slash__s_slash_
# def _s_slash__s_slash__s_slash_
# def _s_slash__s_eq_
# def _s_one_s_plus_
# def _s_one_s_minus_
# def _s_lt_
# def _s_lt__s_eq_
# def _s_eq_
# def _s_gt_
def _s_plus__s_plus_():
    """++ increment operator"""
    raise(Exception("Not implemented"))

def _s_plus__s_plus__s_plus_():
    """+++"""
    raise(Exception("Not implemented"))

def _s_slash__s_slash_(x, y):
    """Integer division operator (//)"""
    return x // y

def _s_slash__s_slash__s_slash_():
    """/// operator"""
    raise(Exception("Not implemented"))

def _s_slash__s_eq_(x, y):
    """Not equal (/=)"""
    return x != y

def _s_one_s_plus_(x):
    """1+ increment by one"""
    return x + 1

def _s_one_s_minus_(x):
    """1- decrement by one"""
    return x - 1

def _s_lt__s_eq_(x, y):
    """Less than or equal (<=)"""
    return x <= y

def _s_gt__s_eq_(x, y):
    """Greater than or equal (>=)"""
    return x >= y