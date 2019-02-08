

class Binding:
    def __init__(self,symbol,value,next,env=None):
        self.symbol = symbol
        self.value = value
        self.next = next
        self.env = env
    def __repr__(self):
        return repr(self.symbol)

py_str_map = [
    ["_S_STAR_","*"],
    ["_S_AMP_","&"],
    ["_S_LT_","<"],
    ["_S_GT_",">"],
    ["_S_EQ_",">"],
    ["_S_PLUS_","+"],
    ["_S_MINUS_","-"],
    ["_S_PRINT_","PRINT"],
]


class SpecialForm(object):
    pass

class FunctionBinding:
    def __init__(self,symbol,value,next):
        self.symbol = symbol
        self.value = value
        self.next = next
    def __repr__(self):
        o = self
        s= []        
        while o != None:
            s.append(repr(o.symbol))
            o = o.next
        return ",".join(s)

class Environment:
    def __init__(self, parent=None):
        self.parent = parent
        if parent == None:
            self.function_bindings = None
            self.variable_bindings = None
            self.tag_bindings = None
        else:
            self.function_bindings = parent.function_bindings
            self.variable_bindings = parent.variable_bindings
            self.tag_bindings = parent.tag_bindings
    def add_function(self, symbol, value):
        self.function_bindings = FunctionBinding(symbol,value, self.function_bindings)
    
    
    def find_func(self,sym):
        b = self.function_bindings
        while b != None:
            if b.symbol.name == sym.name:
                return b.value
            b = b.next

    def find_variable(self, sym):
        b = self.variable_bindings
        while b != None:
            if b.symbol.name == sym.name:
                return b.value
            b = b.next
        raise Exception("Unbound variable")

    def add_variable(self, sym, value):
        self.variable_bindings = Binding(sym,value,self.variable_bindings)

    def read_module(self, mod):
        for k,v in mod.__dict__.items():
            if callable(v) and not k.startswith("__"):
                self.add_function(py_str_to_sym(k),v)
    def __repr__(self):
        return "Environment(function_bindings="+repr(self.function_bindings)+")"


class lispT:
    pass

class lispSequence(lispT):
    pass

class lispList(lispSequence):
    pass

class lispNull(lispList):
    def __str__(self):
        return "NIL"
    def __repr__(self):
        return "NIL"

NIL = lispNull()

class LispSymbol(lispT):
    def __init__(self, name):
        self.name = name.upper()
    def __repr__(self):
        return self.name

class lispKeyword(LispSymbol):
    pass



class lispConsIterator:    
    def __init__(self, cons):
        self.cons = cons
    def __iter__(self):
        return self
    def __next__(self):
        if self.cons == None or type(self.cons) is lispNull:
            raise StopIteration()
        value = self.cons.car
        self.cons = self.cons.cdr
        return value
    def next(self):
        return self.__next__()

class lispCons(lispList):
    def __init__(self,car,cdr=NIL):
        self.car = car
        if cdr == None or type(cdr) is lispNull:
            self.cdr = NIL
        elif type(cdr) is tuple:
            cdrlen = len(cdr)
            if cdrlen == 0:
                self.cdr = NIL
            elif cdrlen == 1:
                self.cdr = lispCons(cdr[0])
            else:
                self.cdr = lispCons(cdr[0],cdr[1:])
        else:
            self.cdr = cdr
    def __str__(self):
        values = []
        values.append("(")
        values.append("NIL" if self.car == None else str(self.car))
        cdr = self.cdr
        while cdr != None:
            values.append(" ")
            if type(cdr) is lispCons:
                values.append(str(cdr.car))
                cdr = cdr.cdr if type(cdr.cdr) is not lispNull else None

            else:
                values.append(". ")
                values.append(str(cdr))
                cdr = None            
        values.append(")")
        return ''.join(values)
    
    def __repr__(self):
        values = []
        values.append("(")
        values.append("NIL" if self.car == None else repr(self.car))
        cdr = self.cdr if type(self.cdr) is not lispNull else None

        while cdr != None:
            values.append(" ")
            if type(cdr) is lispCons:
                values.append(repr(cdr.car))
                cdr = cdr.cdr if type(cdr.cdr) is not lispNull else None
            else:
                values.append(". ")
                values.append(repr(cdr))
                cdr = None
        values.append(")")
        return ''.join(values)
    
    def __iter__(self):
        return lispConsIterator(self)


class lispPackage(lispT):
    def __init__(self, name):
        self.name = name


class lispPackages():
    
    _packages = {}
    
    @staticmethod
    def find_package(name):
        if name in lispPackages._pacckages:
            return lispPackages._pacckages[name]
        return NIL
    
    @staticmethod
    def make_package(name):
        if name in lispPackages._packages:
            raise Exception("Package exists")
        pkg = lispPackage(name)
        lispPackages._packages[name] = pkg
        return pkg
    
    @staticmethod
    def get_packages():
        result = NIL
        for pkg in lispPackages._packages.values():
            result = lispCons(pkg,result)
        return result
        


def py_str_to_sym(s):
    s = s.upper()
    for p in py_str_map:
        s = s.replace(*p)
    return LispSymbol(s)

            

