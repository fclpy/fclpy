import importlib
from pprint import pprint
import fclpy.lispfunc.registry as r
import fclpy.lispfunc as lfunc

stub_mod_name = 'fclpy.lispfunc.evaluation_stubs'

entries = {}
for lisp_name, entry in r.function_registry.items():
    py = entry.py_name
    fn = getattr(lfunc, py, None)
    mod = fn.__module__ if fn is not None else None
    entries[lisp_name] = (py, mod)

# Filter entries whose implementation lives in evaluation_stubs
stub_impls = {k:v for k,v in entries.items() if v[1]==stub_mod_name}
print('Registry entries whose Python implementation module is evaluation_stubs:')
for k,v in sorted(stub_impls.items()):
    print(k, '->', v)
