import importlib
import fclpy.lispfunc.registry as r
import fclpy.lispfunc as lfunc

print('Initial registry size:', len(r.function_registry))

# Remove entries whose python implementation module is evaluation_stubs
removed = []
for name, entry in list(r.function_registry.items()):
    py = entry.py_name if hasattr(entry, 'py_name') else (entry.get('py_name') if isinstance(entry, dict) else None)
    fn = getattr(lfunc, py, None) if py else None
    mod = getattr(fn, '__module__', None)
    if mod and mod.endswith('evaluation_stubs'):
        del r.function_registry[name]
        removed.append(name)

print('Removed stub-backed entries:', len(removed))
for n in removed:
    print(' -', n)

# Reload lispfunc to trigger register_module again
importlib.reload(lfunc)
print('After reload registry size:', len(r.function_registry))

# Show any remaining entries that still point to evaluation_stubs
stub_mod_name = 'fclpy.lispfunc.evaluation_stubs'
for lisp_name, entry in sorted(r.function_registry.items()):
    py = entry.py_name
    fn = getattr(lfunc, py, None)
    mod = fn.__module__ if fn is not None else None
    if mod == stub_mod_name:
        print('STILL STUB:', lisp_name, '->', (py, mod))

print('Done')
