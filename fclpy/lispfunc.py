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

# def ceiling
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
# def eql
# def equal
# def equalp
# def error
# def etypecase

def eval(form):
    symbol = car(form)
    args = tuple(cdr(form))
    f = current_environment.find_func(symbol)
    return f(*args)

# def eval-when
# def evenp
# def every
# def exp
# def export
# def expt
# def extended-char 
# def # def fboundp
# def fceiling
# def fdefinition
#*features*
# def ffloor
#deposit-field
#mask-field
# def fifth
#compile-file
#delete-file
#end-of-file
#probe-file
#rename-file
#with-open-file
# def file-author
# def file-error
# def file-error-pathname
# def file-length
# def file-namestring
#compile-file-pathname
#*compile-file-pathname*
# def file-position
# def file-stream
# def file-string-length
#*compile-file-truename*
# def file-write-date
# def fill
#pprint-fill
# def fill-pointer
#array-has-fill-pointer-p
# def find
# def find-all-symbols
# def find-class
# def find-if
# def find-if-not
# def find-method
# def find-package
# def find-restart
# def find-symbol
#loop-finish
# def finish-output
# def first
# def fixnum
#most-negative-fixnum
#most-positive-fixnum
# def flet
#decode-float
#double-float
# def float
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
# def fround
# def ftruncate
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
#compiled-function-p
# def functionp
# def # def gcd
# def generic-function
# def gensym
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
# def graphic-char-p 
# def handler-bind
# def handler-case
# def hash-table
# def hash-table-count
# def hash-table-p
# def hash-table-rehash-size
# def hash-table-rehash-threshold
# def hash-table-size
# def hash-table-test
# def host-namestring 
# def identity
# def if
# def ignorable
# def ignore
# def ignore-errors
# def imagpart
# def import
# def in-package
# def incf
# def initialize-instance
# def inline
# def input-stream-p
# def inspect
# def integer
# def integer-decode-float
# def integer-length
# def integerp
# def interactive-stream-p
# def intern
# def internal-time-units-per-second
# def intersection
# def invalid-method-error
# def invoke-debugger
# def invoke-restart
# def invoke-restart-interactively
# def isqrt 
# def # &key
# &allow-other-keys
# def keyword
# def keywordp
# function-keywords
# lambda-list-keywords
# def # def labels
# def lambda
# function-lambda-expression
# def lambda-list-keywords
# def lambda-parameters-limit
# def last
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
# def length
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
# def max
# def member
# def member-if
# def member-if-not
# def merge
# def merge-pathnames
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
# def null
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
# def oddp
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
# def plusp
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
# def reverse
# *print-right-margin*
# string-right-trim
# def room
# def rotatef
# def round
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
# def truncate
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

def _s_plus_(a,b):
    return a + b

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
# def _s_gt__s_eq_ 




  


