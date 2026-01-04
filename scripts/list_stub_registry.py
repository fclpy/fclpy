import importlib
from pprint import pprint
import fclpy.lispfunc.registry as r
mods = importlib.import_module('fclpy.lispfunc.evaluation_stubs')
entries = {name:entry for name,entry in r.function_registry.items()}
stub_entries = {k:v for k,v in entries.items() if getattr(v,'py_name',None) and hasattr(mods, v.py_name)}
print('Registry entries implemented by evaluation_stubs:')
for k in sorted(stub_entries.keys()):
    print(k, '->', stub_entries[k].py_name)
