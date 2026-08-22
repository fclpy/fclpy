# fclpy — Changelog

Split out of [plan.md](../plan.md) on 2026-08-22. It was 1385 lines, 45% of a
document that gets read at the start of every session, and none of it is needed
to decide what to do next — the live sections are plan.md §1 (status), §2 (how
to work) and §5 (temporary deviations).

Each entry is a *mechanism* landed, not a test count. Several also record a
diagnosis that turned out to be **wrong**, and how; that is the part worth
keeping, and the reason this is an archive rather than a deletion.

- **2026-08-22** — **One ordinary lambda list.** `flet.lsp` + `labels.lsp` +
  `lambda.lsp` + `macrolet.lsp` **170 → 232 passing of 249** (79 → 17
  failures): FLET 34 → 1, LABELS 19 → 0, LAMBDA 18 → 8, MACROLET unchanged at
  8 (a different lambda list — see below). `pytest` 1969 passed, 3 xfailed.

  **1. Three binders, and only one of them was right.** DEFUN reached
  `_bind_ordinary_lambda_list_tail`, which correctly locates the keyword
  region *from the lambda list* (CLHS 3.4.1), handles all four `&key` spec
  shapes, evaluates init forms in order, and signals the CLHS 3.4.1.4 errors.
  `eval_lambda` had a full second copy that located the keyword region by
  **scanning the arguments for the first keyword-shaped value** — the exact
  defect that tail binder was extracted to remove — so `&rest` never received
  the keyword arguments and a repeated keyword took the *rightmost* value.
  `make_lambda_closure` (FLET/LABELS) had a third, written from scratch: it
  did not call `parse_lambda_list` at all, and `&allow-other-keys` and `&aux`
  were two literal `pass` branches, supplied-p variables were parsed and
  discarded, and a call could not be malformed enough to raise. So a *local*
  function was a materially different kind of function from a global one,
  which is not a distinction Common Lisp makes.

  `make_ordinary_function` is now the one constructor for all four operators.
  The fix was deletion, not construction: the correct binder already existed.

  **2. Parameters bind through `BindingFrame`, and that closes
  `(declare (special x))` on a parameter** — previously ignored everywhere, by
  every one of the nine binders the audit found. Two CLHS subtleties the shape
  of the code now encodes. Free declarations are installed *after* the
  parameters are bound (`defer_free_declarations`), because CLHS 3.3.4
  excludes initialization forms from a free declaration's scope — FLET.67 and
  LABELS.46 read the *lexical* X in an `&aux` init form under
  `(declare (special x))`. And the implicit block encloses the **body** only,
  so a `RETURN-FROM` in an `&aux` init form leaves the function rather than
  returning from it (FLET.6).

  **3. A dynamically bound variable was invisible under a lexical one of the
  same name.** Found by LAMBDA.63 and general, not specific to parameters: a
  locally declared special binds the *value cell* and adds nothing to the
  environment, so the ordinary "environment chain, then value cell" lookup
  still found an enclosing **lexical** binding first —
  `(let ((y :bad)) (let ((y :other)) (declare (special y)) (flet ((f () y)) (f))))`
  answered `:BAD`. `BindingFrame.bind` now installs the same `%SPECIAL-REF`
  redirection for a locally declared special that a *free* declaration
  already used. (A globally proclaimed special needs none of it: a global
  variable *is* the value cell, so nothing can shadow it.)

  **4. `%SPECIAL-REF` had a read and no write.** Consequence of (3), but it
  was already broken for LOCALLY, DO and DOLIST: any `(setq x ...)`,
  `(incf x)` or `(push v x)` governed by a special declaration expanded to the
  place `(%SPECIAL-REF x)`, fell through to CLHS 5.1.2.9's generic `(SETF fn)`
  rule, and answered **"Undefined function: (SETF %SPECIAL-REF)" as the value
  of the form** (standing rule 2). Read and write are now the matched pair
  `_get_special_reference`/`_set_special_reference`, reached by SETQ, SETF and
  every other place operator through the one `_place_accessor`.

  **5. A `&key` parameter and an actual argument match on *(package, name)*,
  not on name.** CLHS 3.4.1.4: `&key b` declares the keyword `:B`, while
  `((b var) init)` declares whatever symbol `b` was read as. Comparing
  upper-cased names conflated them, so `((lambda (&key b) b) 'b 100)` bound B
  from a symbol the lambda list never named. `keyword_argument_key` is the one
  comparison; `_keyword_param_parts` now returns the keyword *symbol* rather
  than a string, and the DEFMETHOD congruence check reuses it.
  Relatedly, `:ALLOW-OTHER-KEYS` is always *permissible* (3.4.1.4.1) but is
  not thereby excluded from the argument list — a lambda list may declare
  `((:allow-other-keys aok))` and must receive the value.

  **6. Arity is checked** (`_check_ordinary_arity`, CLHS 3.5.1.2/3.5.1.3).
  Every binder used to pad a missing required argument with NIL and discard a
  surplus one, so `((lambda (a) a))` answered NIL and `((lambda (a) a) 1 2)`
  answered 1 — wrong *values*, not merely missing errors, because a caller
  cannot tell a legitimately NIL argument from one never passed.

  **Not fixed, and each is its own thing.** `MACROLET`'s 8 remain: the *macro*
  lambda list is a different lambda list (CLHS 3.4.4, nested destructuring
  patterns) built by `_create_macro_function` + `bind_destructuring_pattern`,
  which are two more binders that ignore `&aux` and `&allow-other-keys` and
  signal nothing — the rest of M3. LAMBDA.57/58/60/61/62 are the **reader**:
  `#1=#:foo` / `#1#` labels do not work at all (`'(#1=#:foo #1#)` reads as a
  one-element list), which is C12, not lambda lists. `FLET.45`
  (`(flet ((nil () 'a)) (nil))`) fails at the *call* site, in the evaluator's
  function-call path. `FLET.51`/`LABELS.26` need a local `(setf f)` function
  to be reachable from a `(setf (f) v)` place — the name is now stored
  correctly, but SETF's place ladder resolves `(SETF f)` globally (M5).

  **Cost.** A pure function-call microbenchmark (`(fib 18)`, naive recursion)
  went 0.80s → 0.84s, about 5%: every call now constructs a `BindingFrame`,
  which is what decides lexical vs. dynamic. `BindingFrame.__init__` was
  flattening `bound_vars` into a set on every construction to separate free
  from bound declarations — a question with no possible non-empty answer when
  the form has no declarations at all — so that is now skipped, which took the
  cost from ~15% to ~5% and speeds up LET/DO/LOOP too.

  **Discovered, not fixed.** `(hash-table-p (make-hash-table))` answers **NIL**
  while `(typep ht 'hash-table)` answers T, and `(hash-table-test ht)` answers
  the Python string `'EQL'`. All 29 tests in `hash-tables/make-hash-table.lsp`
  open with `(notnot (hash-table-p ht))`, so that one predicate is the whole
  100%-failing file — *not* the `:test`-designator diagnosis
  [§3](#files-failing-100--the-strongest-mechanism-absent-signal) carried for
  three revisions. Both symptoms point at the second, dead hash-table
  implementation in `lispfunc/hashtables.py` that registers the same operators
  (standing rule 3, [§5](#5-known-temporary-deviations)). **This is the
  recommended next task**: one duplicate deleted, 29 tests in the largest
  100%-failing file, and plausibly a large share of the other 41 in
  `hash-tables/` (55.7%).

- **2026-08-21** — **`Pathname` is a component record.** `pathnames` (targeted)
  82 → 214 of 215 (38.1% → 99.5%), `pytest` 1964 passed. Two real
  regressions surfaced and were fixed (item 5 below); `files`,
  `system-construction`, `streams`, `packages`, `conditions` and `hash-tables`
  were re-run and diffed name-for-name against `git stash`d unmodified code
  with none remaining. `objects`/`structures`/`iteration`/`sequences`/`cons`/
  `strings`/`symbols` were not individually diffed this way — `cons` alone
  already runs several minutes on *unmodified* code (confirmed, not a new
  hang), which is what made one combined 13-directory sweep look stuck; treat
  those seven as spot-checked by `pytest` and the shared-mechanism reasoning
  in items 3-4, not as independently confirmed regression-free.

  **1. There was nothing to compose.** `Pathname` stored a `pathlib.Path`
  parse plus the original string, so `MAKE-PATHNAME`, `MERGE-PATHNAMES` and
  `DIRECTORY` re-derived every answer from a flat string instead of
  combining components. Rewrote it as host/device/directory/name/type/
  version (CLHS 19.2), using interned keyword objects (`:WILD`,
  `:WILD-INFERIORS`, `:UP`, `:BACK`, `:ABSOLUTE`/`:RELATIVE`,
  `:UNSPECIFIC`, `:NEWEST`) as component markers so a marker and a literal
  string component can never be confused (they're different Python types).
  `MAKE-PATHNAME`'s defaulting (host alone falls back to
  `*DEFAULT-PATHNAME-DEFAULTS*` when no `:defaults` is given; every other
  component defaults to NIL), `MERGE-PATHNAMES`'s per-component merge
  (including CLHS 19.3.3's easy-to-get-backward VERSION rule — it comes from
  `defaults` only when `pathname` supplies *no* name and *no* type, and from
  `default-version` otherwise), `WILD-PATHNAME-P`, `PATHNAME-MATCH-P` and
  `TRANSLATE-PATHNAME` (a real capture-based directory-wildcard matcher,
  not `fnmatch` on the whole string) are all built on it.

  **2. `misc_macros.py` was silently shadowing three logical-pathname
  functions with no-op stubs.** It defines `LOAD-LOGICAL-PATHNAME-
  TRANSLATIONS`/`LOGICAL-PATHNAME-TRANSLATIONS`/`DIRECTORY`/
  `ENSURE-DIRECTORIES-EXIST` too, and because `lispfunc/__init__.py` imports
  it *after* `pathnames.py`, the registry decorator overwrote pathnames.py's
  real implementations every time — standing rule 3 (two implementations of
  one operator), the same shape `system-construction`'s `WITH-COMPILATION-
  UNIT` had. `(setf (logical-pathname-translations "CLTEST") ...)` therefore
  always looked like a no-op, independent of anything pathnames.py did.
  Deleted the stubs; a basic logical-pathname mechanism (host registry,
  namestring parse/render, `TRANSLATE-LOGICAL-PATHNAME` built on the same
  wildcard matcher as `TRANSLATE-PATHNAME`) now actually works, including
  a stream's `LOGICAL-PATHNAME` remembering the logical designator OPEN was
  given even after `resolve_filespec` has already turned it into the real
  OS path used for I/O.

  **3. `&key` default-value forms could not see earlier `&key` arguments.**
  `_bind_keyword_parameters` (a user lambda list's own binder, distinct from
  the builtin-signature path CLAUDE.md documents) evaluated *every*
  parameter's default form first and only afterward overwrote the supplied
  ones — so `(defun f (&key (defaults nil) (device (if defaults (pathname-
  device defaults) ...))) ...)` called as `(f :defaults d)` always evaluated
  DEVICE's default with DEFAULTS still NIL, regardless of what was passed.
  CLHS 3.4.1.1 requires left-to-right visibility: each parameter's init-form
  runs in an environment where every earlier parameter, defaulted or
  supplied, already has its real value. This is not a pathname-specific
  bug — it silently mis-evaluated *any* user function with one `&key`
  parameter's default referencing another — and it was invisible until a
  test helper happened to be shaped exactly that way:
  `pathnames/make-pathname.lsp`'s own `make-pathname-test` derives every
  expected component from `:defaults` through nested `&key` defaults.

  **4. `EQUAL`/`EQUALP` had their own narrower copy of "is this a string."**
  `comparison.py`'s `_string_characters` recognized `LispString`/`str` but
  not the third representation `characters.is_string` already knows about:
  a rank-1 array whose element type is a subtype of CHARACTER, which is what
  a *displaced* character vector always is (`LispString` has no displacement
  support). So `(equalp displaced-string "foo")` was NIL whenever one side
  happened to be displaced — plan.md's already-recorded deviation on this
  exact gap, previously believed confined to "the STRING-specific
  operators." Extended the array branch here to match; found via
  `make-pathname.lsp`'s `do-special-strings`, which exercises every
  fill-pointer/adjustable/displaced/base-char combination of a name argument
  and compares the result with `EQUALP`.

  **5. `EQUAL` on a MERGE-PATHNAMES result regressed two `system-
  construction` tests the moment MERGE-PATHNAMES stopped being a string
  operation.** CLHS 19.3.3's version rule (item 1 above) makes
  `(merge-pathnames (make-pathname :name "foo"))` answer `:NEWEST`, correctly
  — `merge-pathnames.1`-`.7` require exactly that. But `*LOAD-PATHNAME*`,
  built straight from LOAD's own namestring (which has no version syntax at
  all), answers `NIL` for the identical file, and `load.17`/`.18` require
  `(equal (pathname (merge-pathnames f)) *load-pathname*)`. Confirmed via
  `git stash`: this really is new, not pre-existing (`COMPILE-FILE.16`,
  `LOAD.17` — `LOAD.18` itself turned out to be pre-existing, see below).
  `Pathname._key()` now treats `:NEWEST` and NIL as the same version for
  `EQUAL`/`EQUALP`/hashing purposes only — `PATHNAME-VERSION` still answers
  the one actually stored, so `merge-pathnames.6`'s "answer `:NEWEST`
  literally" and `load.17`'s "be `EQUAL` to a NIL-version pathname" are both
  satisfied without contradiction.

  **Discovered, not yet fixed:**

  - `LOAD.18` (`system-construction`) fails identically on the
    pre-`git stash` code: `(declare (special ...))` on a *free* variable
    reference inside a `LET*` (no binding form for that name in the same
    `LET*`) appears to leave a phantom entry that a later `SETQ` of the
    truly-global variable of the same name -- inside a nested `(load ...)`,
    after `MAKUNBOUND` -- finds and reports as unbound, even though
    `(declaim (special ...))` inside the loaded file has already proclaimed
    it. Reproducible outside pathnames entirely; not touched here.
  - `cons/` (and likely other large directories) already runs to several
    minutes under `run_ansi.py` on unmodified code -- confirmed with
    `git stash` plus a wall-clock cap, not assumed. Don't read a long
    multi-directory `run_ansi.py` invocation's silence as a hang; run
    directories individually if you need a clean per-directory timing.
  - a physical pathname's `VERSION` has no namestring syntax to round-trip
    through when `NAME`/`TYPE` are both NIL — `(make-pathname :version
    :newest)` and `(make-pathname :version :wild)` both print `#P""` and
    read back with `VERSION` NIL (`PATHNAMES-PRINT-AND-READ-PROPERLY`, the
    one remaining failure). Real Unix-style physical pathnames have no
    version syntax at all; inventing one only for this round trip would be
    exactly the kind of test-specific hack the rules of this project forbid.

- **2026-08-20** — **`system-construction` 12 → 77 of 77 (100%)**, and the
  mechanisms it took to get there were mostly not in `system-construction`.
  `files` 29 → 47 of 87, `streams` +10 on a targeted run, 1961 unit tests
  passing. This was the C11 rung, and the shape of it is the point: a directory
  at 16% was failing on **eleven** distinct mechanisms, of which only three
  were about building systems at all.

  **1. `LOAD` is one operation whether it reads a file or a stream (CLHS
  24.2).** `load-file.lsp` 3 → 27 of 27. The old LOAD ran `str(filespec)` on
  whatever it got, so a stream became the *pathname*
  `"<StringInputStream pos=0 len=59>"`. Four more things were wrong and each
  was a mechanism rather than a detail: forms are read **one at a time through
  READ**, not through one reader built at the top, because READ consults
  `*READTABLE*` and `*PACKAGE*` per call and a form in the file that assigns
  either governs how the rest of the file is *read* (load.15a, load.16a);
  `*PACKAGE*`/`*READTABLE*`/`*LOAD-PATHNAME*`/`*LOAD-TRUENAME*` are **bound**
  through `BindingFrame` — the mechanism LET uses — rather than saved and
  restored by hand, so a file's IN-PACKAGE is undone however the load exits;
  `:if-does-not-exist` was **inverted** (`is NIL or is None` *raised*), so
  `(load f :if-does-not-exist nil)` signalled; and all four keyword parameters
  are now spelled keyword-only, which is what makes `(load f :bad-key-arg t)`
  the PROGRAM-ERROR CLHS 3.5.1.5 requires.

  **2. `COMPILE-FILE` did not read the file.** It was `shutil.copy2`. So it
  evaluated nothing (no `(eval-when (:compile-toplevel) ...)`), bound none of
  the compile-file variables, resolved no `#.`, and reported `warnings-p` and
  `failure-p` as constant NIL. It now reads each top-level form, evaluates the
  ones CLHS 3.2.3.1 says the compiler must (`COMPILE_TIME_OPERATORS`, and
  EVAL-WHEN's `:compile-toplevel`, recursing through PROGN/LOCALLY), and
  *prints* the forms to the output. Printing rather than copying is the
  mechanism: `#.` is **read**-time evaluation, so a byte copy defers it to load
  time when `*compile-file-truename*` is NIL, which is exactly what
  `compile-file.16` measures. The printer controls are pinned while writing
  (`OUTPUT_PRINTER_CONTROLS`) because a caller's `*print-length*` would
  otherwise truncate the output to `...` — a corrupt file reported as a
  successful compilation. `warnings-p`/`failure-p` come from a handler cluster
  pushed on `state.handler_stack`, i.e. the same mechanism HANDLER-BIND uses,
  declining every condition so the compiled program's own handlers still see
  them.

  **3. `WITH-OUTPUT-TO-STRING (var string)` never wrote to `string`.** It bound
  `var` to a fresh `MAKE-STRING-OUTPUT-STREAM` and then transferred its
  contents nowhere. This is a **measurement gate**, not a wrong value: the ANSI
  suite captures an operator's output with exactly this form and then asserts
  about it, so every test of what something *prints* compared its expectation
  against the empty string, and no amount of correct printing could pass.
  `streams.FillPointerOutputStream` is the object CLHS describes — output
  appends to the supplied fill-pointered string as the body runs, so text
  written before a non-local exit is already there.

  **4. `&rest` did not get the rest of the arguments.** The user-lambda-list
  binder located the keyword region by *scanning the arguments* for the first
  keyword-shaped value, rather than reading it off the lambda list as CLHS
  3.4.1 defines it (after the required and `&optional` parameters, full stop).
  So `(defun g (a &rest args) ...)` called as `(g 1 :b 2)` bound ARGS to
  **NIL**, and the `&rest args &key ...` forward-my-arguments idiom the ANSI
  suite's own helpers are written in silently forwarded nothing — which is why
  `load-file-test` could pass `:verbose t` to LOAD and LOAD never saw it. Fixed
  with `_bind_keyword_parameters`, which applies 3.4.1.4/3.5.1.5 to a user
  lambda list the way `evaluation_core._split_declared_keywords` already
  applies it to a builtin's Python signature: leftmost pair wins, an odd count
  is a PROGRAM-ERROR, an undeclared keyword is a PROGRAM-ERROR unless
  `&allow-other-keys` or `:allow-other-keys` says otherwise. `&allow-other-keys`
  had been discarded by the parser as "informational". The
  `((keyword-name var) init)` spec shape was not handled at all, because the two
  loops that decomposed a `&key` spec — once for defaults, once for matching —
  each assumed the keyword and the variable were the same name.

  **5. A copied readtable's built-in readers read through the readtable they
  were copied from.** `Readtable.copy()` copied the macro-character dict, whose
  entries are **bound methods**, each of which reads its sub-expressions
  through `self._read_item`. So `(copy-readtable nil)` followed by
  `set-macro-character` — the standard idiom — worked at top level, where
  `read_1` looks the character up in the *current* readtable, and was invisible
  inside any list: `(list 1 !good)` read as `(LIST 1 !GOOD)`. `_rebind` rebinds
  a table's own methods onto the copy; a function that is not one of them (a
  user function, or a reader borrowed with `(get-macro-character #\')`) is
  carried across untouched, because there the function really is the value.

  **6. `SYMBOLP` and `TYPEP` disagreed about what a symbol is.** `SYMBOLP` was
  `type(obj) is LispSymbol` — an *exact* type test — so `(symbolp :foo)` and
  `(symbolp nil)` were NIL while `(typep :foo 'symbol)` and `(typep nil
  'symbol)` were T. `lisptype.is_symbol` is now the one predicate and both go
  through it. Anything dispatching on SYMBOLP (`(every #'symbolp *features*)`,
  place processing, LOOP var-specs) had been seeing keywords as non-symbols.

  **7. A `LispString` reported two different contents.** `__len__` and
  `__iter__` honoured the fill pointer; `__str__` and `__repr__` returned the
  whole backing store. For a fill-pointered "FOO" over "FOOZZZZ", `len(s)` was
  3 and `str(s)` was "FOOZZZZ" — so every Python-side reader that goes through
  `str()` (the string-designator resolvers, FORMAT, the printer) saw the
  inactive characters.

  **8. There was one "resolve a relative pathname" search, written five times.**
  LOAD, COMPILE-FILE, COMPILE-FILE-PATHNAME, DELETE-FILE and OPEN each had
  their own ~35-line copy, and the copies had drifted: LOAD read
  `*DEFAULT-PATHNAME-DEFAULTS*` out of `COMMON-LISP-USER` while COMPILE-FILE
  read it out of `COMMON-LISP`, and since a global variable's home is the
  symbol's value cell and lookup is by symbol *identity*, those are two
  different variables. OPEN's copy took the `LISP_CWD` candidate
  unconditionally where the others took it only if it existed, so OPEN and
  PROBE-FILE could resolve the same relative name to two different files.
  `pathnames.resolve_filespec` is the one search, and it also owns the CLHS
  *pathname designator* rule, including "a stream associated with a file" —
  which is why `:output-file <stream>` and `(compile-file <stream>)` work now.
  It always returns a path, existing or not, so a caller can *name* a missing
  file. `MERGE-PATHNAMES` also now defaults its `defaults` argument to
  `*DEFAULT-PATHNAME-DEFAULTS*` instead of being the identity.

  **9. A missing file was a Python exception.** ~1 in 5 of `files/`'s failures
  were `FileNotFoundError`/`FileExistsError` surfacing as the *value* of the
  form, because a Python exception is not a condition and matches no handler
  clause. `evaluation_conditions.signal_file_error` is the one place a file
  operation reports failure; `FileError` gained the PATHNAME slot CLHS gives it
  and `FILE-ERROR-PATHNAME` reads it (returning the slot **unchanged** — the
  suite passes namestrings, pathnames and streams as `:pathname` and requires
  each back out). LOAD, COMPILE-FILE, OPEN, DELETE-FILE, RENAME-FILE and
  TRUENAME go through it. Two pathnames naming the same file are also EQUAL
  now, which they were not however identically they printed.

  **10. An uncaught THROW aborted the process.** `eval_throw` raised its Python
  `ThrowException` unconditionally, so a throw with no outstanding catcher left
  the evaluator as a Python exception — matching no handler, escaping
  `do-tests`, and killing the whole run rather than failing one test. CLHS 5.2
  makes it a CONTROL-ERROR, which requires knowing what is outstanding:
  `state.catch_tags`, pushed by `eval_catch` for its body's dynamic extent.
  Found because fixing READ-FROM-STRING made `read-suppress.lsp`'s
  `#.(throw 'foo 1)` actually *run*.

  **11. `FMAKUNBOUND` removed nothing observable.** A function definition lives
  in two structures on an `Environment` — the `function_bindings` list and the
  `_function_map` name cache `find_func` reads first — and FMAKUNBOUND unlinked
  the list node only, so `(fboundp g)` stayed T for ever after. It also looked
  only in `state.current_environment` while DEFUN defines at the root, and
  returned T/NIL rather than its argument. `Environment.unbind_function` is now
  the one place a function binding is removed. **This one stale cache entry
  failed sixteen `system-construction` tests**, because `compile-file-test` and
  `load-file-test` both open with `(fmakunbound funname)` and then assert the
  function is *not* defined.

  Also: `*MODULES*`/PROVIDE/REQUIRE existed as three stubs returning their own
  argument, with `*MODULES*` unbound (`modules.lsp` 0 → 13 of 13);
  `WITH-COMPILATION-UNIT` was a `cl_function`, so its option list
  `(:OVERRIDE NIL)` was *evaluated as a function call* and its body's multiple
  values were lost (0 → 7 of 7 — the registry defect CLAUDE.md documents,
  found for the fourth time); `READ-FROM-STRING` had its own copy of READ's
  plumbing which raised `TypeError: initial_value must be str or None, not
  LispString` for any Lisp string and returned one value where CLHS requires
  two; and "the current package" was resolved four different ways, all reading
  `state.current_package` — a mirror only written when a *binding form* binds
  `*PACKAGE*`, so a plain `(setq *package* ...)` in a loaded file changed the
  variable and left every reader interning into the old package.
  `state.current_package_value()` is the one resolver.

  **What this says about the ranking.** `system-construction` was ranked as a
  subsystem gap (C11). Three of its eleven mechanisms were: LOAD, COMPILE-FILE,
  modules. The other eight were core defects — a lambda-list rule, a type
  predicate, a string's own length, a readtable's identity, a control-transfer
  rule — that a 75-test directory happened to be the *only* place exercising
  all of them at once. A directory at 16% is evidence about mechanisms, not
  about the subsystem it is named after.

- **2026-08-18 (b)** — **List traversal is one primitive; a builtin's `&key`
  set is declared rather than guessed; and the printer cannot be made to
  diverge.** `cons` **217 → 16** failures of 1638 (99.0%), `sequences`
  171 → 161, and **+41 in seven directories that were never targeted**, 0
  per-file regressions.

  **1. There was no primitive that walked a Lisp list.** Roughly thirty CLHS
  14.2 operators each open-coded `while isinstance(cur, lispCons)`, and not one
  of them checked what it was walking, so `(member 'a 1)` answered NIL instead
  of signalling. The expensive half was not the missing errors, though: a dotted
  list's terminator was appended by `seq_elements` as **one more element**,
  "so callers can detect it" — which no caller did. That is a wrong *value*, not
  a missing signal, and it is why `(append '(a . b) '(z))` answered `(A B Z)`,
  `(pairlis '(a . b) '(c . d))` paired B with D, and
  `(list-length '(a b c d . e))` answered 4.
  `sequence_protocol.list_cells` is that primitive. Three properties are load-
  bearing and `CLAUDE.md` records them: the terminator is never an element; the
  proper-vs-dotted policy is a CLHS distinction rather than a convenience (a
  LIST argument requires a proper list, while LAST/BUTLAST/NTHCDR/LDIFF/TAILP
  and NCONC's non-final arguments are *defined* on a dotted one because they
  count conses); and traversal is **lazy**, because `(nthcdr 1 (cons 'a 'b))` is
  `B` while `(nthcdr 3 (cons 'a 'b))` signals. `seq_elements` accepts a vector
  and so cannot express "must be a list" — that is why CLHS 14.2 calls
  `list_elements` and CLHS 17 calls `seq_elements`, and picking the wrong one
  makes `(mapcar #'identity "ab")` answer instead of signal.
  Consequences that fell out rather than being aimed at: NCONC had to become
  genuinely destructive (`nconc.4` requires `(cdddr x)` to *be* the second
  argument, `nconc.5` requires a circular result), NBUTLAST likewise, and
  MAPCAN/MAPCON became `(apply #'nconc ...)` instead of folding results
  themselves — `(mapcan (constantly 1) '(a))` is `1`, not `(1)`.

  **2. A builtin's `&key` parameter set was undecidable, so CLHS 3.4.1.4 could
  not be enforced.** `inspect.signature` was read as "every defaulted parameter
  is a `&key` name", which cannot tell `(union nil nil :bad t)` — a
  PROGRAM-ERROR — from `(intern "a" :cl-test)`, where :CL-TEST is `package`'s
  *value*. `split_keyword_args` therefore had to **guess**, and guessed by
  letting an unrecognized keyword fall through to a positional argument: a
  silently wrong answer where the standard wants a signal (standing rule 4).
  Python can express an ANSI ordinary lambda list exactly, but only if the whole
  parameter model is used: `&optional` is positional-or-keyword *with* a
  default, **`&key` is keyword-only**. `LambdaListShape` reads that off the
  signature and is the one place 3.4.1.4/3.5.1.5 is applied, for direct calls,
  FUNCALL and APPLY alike — including the rules that a name need only be a
  *symbol* (3.4.1.4.1.1, so `'#:x` is well-formed) and that the **leftmost**
  `:allow-other-keys` governs wherever it appears. Builtins whose `&key` set is
  still inferred keep the old behaviour behind `_split_inferred_keywords`; the
  sequence, cons and set families are migrated, the rest is
  [§5](#5-known-temporary-deviations).

  **3. Two more mechanisms surfaced from residual failures, and both had blast
  radius well outside cons.** **Backquote dropped a dotted tail**: `` `(a . d) ``
  answered `(A)`, because the expander walked the template with
  `while consp(cur)` and built onto NIL. ansi-test constructs most of its
  association lists as `` `((,x . d) (,y . e)) ``, so this was corrupting the
  suite's own *test data* — `assoc.11` and `member-if.4` were failing on inputs
  that had silently lost their values. (`` `(a . ,x) `` needs its own case: it
  reads as the *proper* list `(A UNQUOTE X)`.) And a `:test`/`:key` designator's
  result was not reduced to its **primary value**, because `_call_checked` calls
  the Python callable directly and so skipped the reduction every other call
  site applies: a key ending in `(floor (/ i 2))` handed the comparison a
  `MultipleValues` *object* (standing rule 2). `lisptype.primary_value` is now
  that rule's one home and the evaluator's four open-coded copies go through it.

  **4. The printer could be made to diverge, and that is a *measurement*
  defect.** `MAX_DEPTH = 256` was documented as the cutoff standing in for the
  absent `*PRINT-CIRCLE*` — "an infinite recursion here aborts a whole ANSI
  run" — but it bounded only *recursion*. A cdr cycle keeps `depth` constant, so
  `_write_cons` appended forever and `(let ((a (list 17 nil))) (setf (cdr a) a) a)`
  answered `MemoryError` **as the value of the form**; and a cycle through an
  element re-enters the same path with each level re-walking its own cdr chain,
  which is *exponential* rather than merely unbounded. Worse,
  **`PPRINT-FILL` and five siblings were stubs calling Python's `print()`** —
  wrong stream (so every `(with-output-to-string (s) (pprint-fill s obj))` in
  `printer/` saw `""` regardless, the same measurement gate that hid the whole
  printer before 08-14) *and* rendered through `lispCons.__str__`, the
  pre-printer path with no guards. Fixed with per-chain cell tracking, a
  path-based re-entry guard, and `PRINT_BUDGET`, since cutting cycles is not by
  itself a termination proof — a twenty-node graph has exponentially many simple
  paths. `printer/print-cons.lsp` **0/20 → 11/20**; `pprint-fill.lsp` went from
  hanging to 10/20; the whole `printer` directory from 21GB to 132MB. The
  `PPRINT-*` operators deliberately do **not** implement CLHS 22.2.2 — the
  block-delimiter arguments are accepted and ignored, because building them
  would be a second printer ([§5](#5-known-temporary-deviations)).

- **2026-08-18** — **Method combination exists (CLHS 7.6.6).** A generic
  function had no method combination at all: `DEFGENERIC` parsed `:METHOD`
  and `:DOCUMENTATION` and dropped every other option on the floor, and
  `call_generic_function` hard-coded standard combination's four qualifier
  buckets. So `(:method-combination progn)` silently produced a *standard*
  generic function, and each of its `progn`-qualified methods matched none
  of `{}`/`{BEFORE}`/`{AFTER}`/`{AROUND}` and was **discarded without a
  diagnostic** — the call then failed with "no applicable method" naming
  nothing that had gone wrong (standing rule 4, twice over).
  `DEFINE-METHOD-COMBINATION` existed **twice** — a special form in
  `evaluation_special_forms.py` and a `cl_function` in `utilities_errors.py`
  — and neither copy defined anything: both built an anonymous Python object
  and bound it as a *variable* under the combination's name (standing rules
  2 and 3).
  **The mechanism is a method-combination object with one invocation
  primitive under it.** `classes.call_method(method, next_methods, args)` is
  now the only place a method is ever invoked, and the frame it pushes is
  the one `CALL-NEXT-METHOD`/`NEXT-METHOD-P` read. That replaced a frame
  carrying a `kind` discriminator plus, for `:around`, its own `core`
  closure — which is why `NEXT-METHOD-P` answered T inside *every* `:around`
  method whether or not anything remained, and why nothing except standard
  combination could build a chain at all. A generic function now holds a
  `MethodCombination`; `None` means standard, not "none".
  **The effective method is a *form*, and that is forced rather than
  stylistic.** Standard combination assembles its chain in Python because its
  shape is fixed, but the short form (CLHS 7.6.6.4's nine built-ins, plus
  `DEFINE-METHOD-COMBINATION`'s short form) and the long form build
  `(operator (call-method m1) (call-method m2) ...)` and evaluate it,
  because the operator may be a **macro whose evaluation order is the
  semantics**: `defgeneric-method-combination.and.1` asserts that the methods
  after the first NIL never run. Folding the method results in Python gives
  the right answer for `PROGN` and `LIST` and the wrong one for `AND`/`OR`.
  That in turn required `CALL-METHOD` and `MAKE-METHOD` to become real
  special operators — they were `cl_function`s that evaluated both operands
  (one of which is an unevaluated method body) and then ignored
  `next-method-list` entirely, i.e. the registry defect `CLAUDE.md` names, in
  the one place where it makes CALL-NEXT-METHOD structurally impossible.
  Building the form out of *interned* `COMMON-LISP` symbols matters for the
  same reason it did in 08-16 (c): lookup is by symbol identity.
  **Two defects surfaced only once the machinery worked**, both found by the
  regression diff rather than by aiming at them: (1) "no applicable methods"
  is decided **before** the combination is consulted (CLHS 7.6.6) — a
  long-form body mapping over an empty method group otherwise cheerfully
  returns `#()`; (2) CLHS's `:arguments` lambda list binds its variables to
  **forms**, not values, so a `&rest` list of `(:z1 4)` spliced into the
  effective method was evaluated as a call to the function `:Z1`. Each
  variable is now bound in the body's environment to its own symbol, and
  that symbol to the argument value in the environment the resulting form is
  evaluated in.
  **Measured, same runner both sides:** `run_ansi.py objects` **298 → 413
  passing of 862** (34.6% → 47.9%), **0 regressions** verified by diffing
  the passed sets across a `git stash` of the change.
  `DEFGENERIC-METHOD-COMBINATION.*` **95 → 4**, `DEFINE-METHOD-COMBINATION*`
  **20 → 0**, `DEFGENERIC.ERROR.*` 22 → 18 (the option-validation four;
  the rest want lambda-list congruence, which is M3's).
  **The disappeared-failures signal was negative** — every recovered test is
  in the eleven files that test method combination — and
  [§4 item 17](#re-derived-from-the-2026-08-16-run) records why that is the
  expected shape for an *absent operator* rather than a reason to distrust
  the fix.

- **2026-08-16 (c)** — **`WITH-STANDARD-IO-SYNTAX` establishes its bindings,
  and a predicted gate turns out not to be one.** It was a `cl_function` whose
  body was "evaluate every argument eagerly, return the last", so it
  established **none** of the twenty-one bindings CLHS 23.4 gives it —
  `(let ((*print-base* 2)) (with-standard-io-syntax (prin1-to-string 5)))`
  answered `"101"` where ANSI requires `"5"`. That is exactly the registry
  defect `CLAUDE.md` names: a form whose subforms must run in a *modified*
  dynamic environment cannot be a `cl_function`, because `cl_function`
  evaluates them before the form runs at all. It is now a `cl_macro` in
  `evaluation_special_forms.py`, beside the WITH-*-STRING expanders that were
  the same defect, expanding to the `LET` of CLHS 23.4's binding list. **This
  deliberately adds no binding mechanism:** every one of the twenty-one
  variables is proclaimed special by `lispenv.STANDARD_SPECIAL_VARIABLES`, so
  `BindingFrame` already binds them in their value cells, and the form's
  value, its multiple values and any non-local exit out of it are LET's —
  which is what `WITH-STANDARD-IO-SYNTAX.19/.20/.21/.22` check.
  **Two details the expansion cannot get wrong quietly.** The binding
  variables are the *interned* `COMMON-LISP` symbols, not bare
  `LispSymbol(...)`: a global variable's home is the symbol's own value cell
  and lookup is by symbol *identity*, so a freshly built `*PRINT-BASE*` would
  be bound and read as a different variable from the one the printer
  consults. And `*PACKAGE*` binds `(find-package "COMMON-LISP-USER")` by name
  rather than to whatever the caller had — `WITH-STANDARD-IO-SYNTAX.1` checks
  precisely that.
  **One object model came with it.** CLHS 23.4 binds
  `*PRINT-PPRINT-DISPATCH*` to "the standard pprint dispatch table", and
  there was no such object to name: `COPY-PPRINT-DISPATCH` answered a bare
  Python `dict` (standing rule 2) and `lispenv` built the initial table from a
  class declared *inline inside* `setup_standard_environment`, so nothing
  else could reach the object the macro has to rebind to.
  `io_write.standard_pprint_dispatch()` is now its one home, the same shape
  as `readtable.standard_readtable()`, and `COPY-PPRINT-DISPATCH` resolves
  NIL to it and raises on anything else rather than answering a dict.
  **Measured, same runner both sides**, over eight files that use the macro
  (`reader/with-standard-io-syntax.lsp`, `printer/print-level.lsp`,
  `pprint-indent.lsp`, `print-strings.lsp`, `write.lsp`, `print-characters.lsp`,
  `format/format-x.lsp`, `format/format-o.lsp`; 182 registered both sides):
  **122 → 142 passing, failures 60 → 40, and every failure that remains is a
  strict subset of the ones before — 0 newly failing.**
  `reader/with-standard-io-syntax.lsp` **19 → 1 failing of 23**. `pytest`
  **1669 → 1699 passed**, same single pre-existing `STREAM-ELEMENT-TYPE`
  failure; the 30 new tests are `tests/test_with_standard_io_syntax.py`, and
  they include the architectural guards [§7](#preventing-regression) asks
  for — the registration is a macro, and no competing special-form
  registration exists.
  **Checklist effect:** 6776 → **6756** failing (−20), files with failures
  544 → 543, merge recorded as `fixed 20, regressed 0, new 0`, and the
  `REGRESSION` marker count against the 08-12 baseline stays at **55** —
  none added. `printer/print-level.lsp` leaves the list entirely.
  **The prediction was wrong, and the way it was wrong is the point.**
  [§4](#recommended-order) item 16a ranked this as a *measurement gate* in
  front of the pretty-printer files, on 455 uses across 58 files — the shape
  items 4, 6, 16 and the COND fix all had. It is not one. Across the **8 of
  those 58 files** measured here — the heaviest users that finish in
  reasonable time — only **2** of the 20 recovered tests were outside the
  file that tests the operator itself
  (`printer/print-level.lsp` 2 → 0, untargeted). The pretty-printer files are
  blocked on the pretty printer *being absent*, not on the gate:
  `def-pprint-test` binds `*print-pretty*` to T in the very next form, so
  what this macro sets it to never mattered to them. By [§2](#the-development-loop)
  step 7's own test — how many failures disappeared that you did not target —
  this is a symptom-sized fix, and the four preceding gate-shaped wins made
  it look larger than it was. **`printer/`'s remaining failures belong to the
  pretty printer and to `FORMAT`'s missing directives, not to their
  preamble.**
  **Discovered, not fixed:** `WITH-STANDARD-IO-SYNTAX.23` needs
  `SET-PPRINT-DISPATCH`/`PPRINT-DISPATCH` to actually dispatch — both are
  stubs returning NIL, so the table's *contents* are still unobservable even
  though its identity now is.
  **And item 14's transcendental/float "regression cluster" does not exist.**
  All fourteen of its files run **250 passing of 254** on `run_ansi.py`
  (`atan`/`sin`/`log`/`acos`/`lcm`/`min` 123 of 125; `tan`/`cos`/`asin`/
  `acosh`/`phase`/`logbitp`/`rationalize`/`make-random-state` 127 of 129),
  and the four failures share no mechanism with each other, let alone one
  float defect:
  - `MIN.27`/`.28` are
    `(loop for i ... for x = (make-list i ...) do (setf (elt x (random i)) 0)
    unless (eql (apply #'min x) 0) collect x)`. `(apply #'min '(1 0 1))`
    answers `0` correctly in isolation; they fail on **LOOP's documented
    bucket-order execution** ([§5](#5-known-temporary-deviations)), which
    evaluates the `unless` test before the `do` that plants the zero, so it
    reads an all-ones list. A C1 follow-up.
  - `RATIONALIZE.1`/`.3` are a real **`RATIONALIZE`** defect: CLHS requires
    the simplest rational that reads back as the *same* float, and this one
    neither round-trips (`(float (rationalize x) x)` ≠ `x`) nor handles
    denormals — `(rationalize 1.4e-45)` is `0`. C15/Phase 4, one operator.

  So the +1/+2 per-file deltas item 14 reads as one shared defect are an
  artefact of measuring against a **three-run-old** baseline, not live
  breakage — which is the cost [§7](#preventing-regression)'s own "process
  lesson" predicted when it noted that refusing to refresh the baseline
  protects old evidence at the price of attributing every future diff
  against an ever-staler point.

- **2026-08-16 (b)** — **The readtable becomes an object with one home, and a
  fourth measurement gate falls.** `(copy-readtable nil)` raised
  `AttributeError: 'lispNull' object has no attribute 'copy'` *as the value of
  the form* (standing rule 2). ansi-test's `my-with-standard-io-syntax` binds
  `*readtable*` to exactly that, `def-print-test` is built on
  `my-with-standard-io-syntax`, and **189 of the 194 tests in
  `printer/print-integers.lsp` are `def-print-test`s** — so every one of them
  failed regardless of what the printer did.
  **The printer was not at fault, and [§4](#recommended-order) item 16 said it
  was.** That item attributes the 161 `PRINT.INTEGERS.BASE` /
  `PRINT.INTEGERS.RADIX.BASE` failures to "the printer's radix/base handling".
  Measured before changing anything: `(prin1-to-string 1)` is `"1"`,
  `(let ((*print-base* 2)) (prin1-to-string 5))` is `"101"`, and
  `(let ((*print-radix* t)) (prin1-to-string 5))` is `"5."` — the radix/base
  half was already correct. This is [§4](#recommended-order) items 4 and 6 and
  the COND fix a **fourth** time: rank a cluster by its failure count and you
  rank the gate in front of it, not the defect.
  **Six defects, one mechanism: there was no readtable *object model*.**
  (1) **No standard readtable existed.** CLHS 23.1.1 makes it a distinct object
  and the glossary makes NIL denote *it* — not the current readtable — wherever
  a readtable designator is accepted. `readtable.standard_readtable()` is now
  that object, and it is **immutable**: it is shared, so a form that mutated it
  would silently redefine what "standard syntax" means for every later
  `(copy-readtable nil)`. The current readtable starts as a *copy* of it.
  (2) **The designator rule was copy-pasted eight times.** Every operator in
  `io_read.py` carried its own `if readtable is None: readtable =
  get_current_readtable()`, which resolves an *omitted* argument and nothing
  else — so all eight broke on exactly the NIL the rule exists for. One
  `coerce_to_readtable` now serves them all. It needs an `_OMITTED` sentinel
  rather than the usual `=None` default, because NIL is a *meaningful* value
  here and `None` cannot tell "omitted" (current) from "given NIL" (standard).
  (3) **`*READTABLE*` was not connected to the reader** — plan.md
  [C7](#c7-the-printer--largely-done-2026-08-14)'s defect in a second
  subsystem. The reader read a module global `readtable._current_readtable`
  while `*READTABLE*` was a separate variable nothing consulted, so
  `(let ((*readtable* rt)) (read ...))` bound the variable and then read with
  the old table. `get_current_readtable()` now reads the symbol's value cell,
  which is its one home; **every reader entry point already funnels through
  that function**, so all of them were fixed at once rather than one at a time.
  (4) `READTABLEP` returned NIL unconditionally — "we don't have readtable
  objects yet" — long after `Readtable` existed, so `(readtablep *readtable*)`
  denied the object `*READTABLE*` was bound to; `TYPEP`'s `READTABLE` branch
  was absent. Both now ask the one object model, so they cannot disagree.
  (5) `READTABLE-CASE` answered the Python string `'UPCASE'` (standing rule 2),
  which is not `EQ` to the `:UPCASE` every caller compares it against, and had
  no writer at all. It answers a keyword, and `SET-READTABLE-CASE` is the
  `(setf (readtable-case rt) ...)` half — reached through SETF's existing
  `SET-<name>` fallback, so this is **not** a sixth entry in the place ladder
  (M5).
  (6) `COPY-READTABLE` ignored its `to-readtable` argument, so
  `copy-readtable.6`'s "modify and return *that* table" could not hold.
  **Measured, same runner both sides:** `printer/print-integers.lsp`
  **0 → 189 passing of 194**. `run_ansi.py reader` (a directory that was *not*
  targeted): **+20 newly passing, 0 newly failing, 0 regressions** merged into
  the checklist. `pytest` **1642 → 1664 passed**, with only the documented
  pre-existing `STREAM-ELEMENT-TYPE` failure; 20 new tests in
  `tests/test_readtable_designator.py` plus one in
  `tests/test_readtable_advanced.py`, and one non-ANSI assertion retired —
  `test_readtable_case_function` pinned `readtable_case() == 'DOWNCASE'`, i.e.
  it certified the Python-string leak.
  **A cost that is a measurement correction, not a regression.** `printer/` as
  a *whole directory* now runs far slower and at one point reached 16.9GB
  resident. The cause is the gate opening: `randomly-check-readability`
  (`printer-aux.lsp:77`) is *also* built on `my-with-standard-io-syntax`, so
  every `PRINT.*.RANDOM` test used to die at the gate immediately. They now
  really run — 1000 random iterations each, with `*print-base*` random in
  2..36 and `*print-level*`/`*print-length*` random in 0..50 — and printing the
  large random structures they build is expensive. This is the same shape as
  the 67 → 113 minute rise the `unless` repair caused: work that used to be
  skipped is really being done. **It is nevertheless unresolved as a practical
  matter, and it cost this change its directory-wide number:** `run_ansi.py
  printer` was attempted twice and abandoned both times (once at 16.9GB
  resident, once after 16 minutes on five files), so **`printer/` as a whole is
  unmeasured here** — only `print-integers.lsp` is. The watchdog printed
  `RESOLVED: progress resumed` at every warning, so this is slow, not wedged.
  See [Discovered issues](#discovered-2026-08-16-b).
  **Checklist effect:** 6985 → **6776** failing (−209), unattributable
  2202 → 2012, files with failures 546 → 544, and the `REGRESSION` marker count
  against the 08-12 baseline fell 56 → 55 — one cleared, **none added**. Note
  the `printer` directory *row* does not move despite the +189, because
  `def-print-test` expands to `deftest` at load time and the checklist's static
  scan cannot attribute those tests to a file ([§3](#the-checklist)'s property
  1); they come out of the *unattributable* bucket instead. `reader` does move,
  129 → 110.

  <a id="discovered-2026-08-16-b"></a>
  **Discovered, diagnosed, not fixed:**
  - **`WITH-STANDARD-IO-SYNTAX` establishes no bindings.** It is registered as
    a `cl_function` in `misc_macros.py` whose body is "evaluate every argument
    eagerly, return the last" — so it binds none of the fourteen variables
    CLHS requires. Reproduction:
    `(let ((*print-base* 2)) (with-standard-io-syntax (prin1-to-string 5)))`
    answers `"101"` where ANSI requires `"5"`. **58 ansi-test files use it**,
    and `def-pprint-test` — the whole pretty-printer test vocabulary — is built
    on it. It is a `cl_function` where the registry note in `CLAUDE.md` says it
    must be a `cl_special`/`cl_macro`. This is the obvious next task and it is
    cheap now that `(copy-readtable nil)` works, since the binding list needs
    it.
  - **`SET-SYNTAX-FROM-CHAR` is a stub that returns T** (standing rule 4) and
    `reader/set-syntax-from-char.lsp` is 12/12 failing. There is no
    *character syntax type* model at all — `Readtable` records macro characters
    and a case, and nothing else — so this is a genuine absent mechanism rather
    than a bug, and it is most of what `reader/` still owes.
  - **The reader ignores `readtable-case`.** `Readtable._read_symbol`
    upcases every token unconditionally, so `:preserve`/`:downcase`/`:invert`
    have no effect on reading even though the readtable now records them
    faithfully and the *printer* honours them. CLHS 23.1.2.
  - **`printer.py:integer_digits` is O(d²) in the digit count** — it `divmod`s
    a shrinking bignum one digit at a time. Fine for fixnums, and it is the
    frame the watchdog caught repeatedly during the slow `printer/` run.
  - `(copy-readtable *readtable* nil nil)` should signal a `PROGRAM-ERROR`
    (`copy-readtable.error.1`); it raises a Python `TypeError` for too many
    arguments instead, which is [X1](#x1-python-exceptions-leaking-as-lisp-values)'s
    boundary rather than anything readtable-specific.

- **2026-08-16** — **A test that never ran, a hang nothing could see, and the
  package leak under both.** The 08-15 tree **could not complete a full run**:
  it sat at 27GB of allocated memory for over half an hour and produced no
  diagnostic of any kind. Three independent defects stacked to make that
  possible, and the third is the one worth keeping.
  **(1) `check-type-error` never called the function under test.** `31c7c59`
  fixed LOOP's `unless` clause, which had never evaluated its test —
  `(loop for e in *mini-universe* unless (typep e 'unsigned-byte) collect e)`
  collected **0** before and **23** after. ansi-test's `check-type-error*` *is*
  that clause: evaluating its `unless` test is what calls the function under
  test. So every `.ERROR` test built on it returned NIL without running
  anything and **passed for the wrong reason** — MAKE-LIST.ERROR.1 among them.
  This is the largest single contributor to +3224, and it is a measurement
  repair, not a feature.
  **(2) What the repair then reached.** `TYPEP` had no branch for the *atomic*
  `UNSIGNED-BYTE`, only the compound `(unsigned-byte n)`, so
  `(typep 5 'unsigned-byte)` was NIL and the guard rejected *everything* —
  handing `(make-list 10000000000000000000000)`, a legitimate `unsigned-byte`
  from `*mini-universe*`, to a `MAKE-LIST` that coerced its size with `int()`
  and built the result one cons at a time. Both fixed: `UNSIGNED-BYTE`/
  `SIGNED-BYTE` atomic and `*`-sized forms in `comparison.py`, and
  `MAKE-LIST`/`MAKE-SEQUENCE` now validate the size (shared
  `arrays.nonnegative_integer`) *and* refuse a size they cannot build.
  **(3) No runner could see it, and that was the real gap.**
  `LoopWatchdog` evaluates its 120s warning and 600s cap inside `tick()`, once
  per iteration, so it is structurally blind to a loop wedged *inside* one
  iteration — which is exactly this. `run_all_tests.py` had **no**
  process-level watchdog at all, in any of its three commits; `run_ansi.py`
  had one, but it measured *total runtime*, so its timeout had to exceed the
  slowest legitimate run. New [`fclpy/watchdog.py`](fclpy/watchdog.py) is one
  shared detector for both runners that measures **time without progress**
  (progress = the harness writing output, so it needs no ansi-test change),
  warns at 120s and hard-stops at 900s, dumping every thread's traceback both
  times. Its last-resort escape is `faulthandler`'s C-level timer rather than a
  Python thread. Output is line-buffered now too: block buffering left
  `run_all_tests.log` ~30 minutes behind the true position, which sent the
  first investigation to the wrong test entirely.
  **Landed with it, because the run could not be trusted without them:** four
  load-time failures, each of which silently removed a whole *file* from the
  run rather than failing one test. `DECODE-FLOAT` returned a Python **tuple**
  instead of `MultipleValues` (unlike its sibling `INTEGER-DECODE-FLOAT`), so
  `(nth-value 1 (decode-float x))` was NIL and ansi-aux's `float-exponent` fed
  NIL to `ABS`; `ABS` called Python's `abs` directly instead of the
  `_ensure_number` the rest of its module uses; `_ensure_number`/`_ensure_real`
  tested `isinstance(x, lisptype.Symbol)` — **a class that has never existed**,
  so the branch raised `AttributeError` instead of the TYPE-ERROR it was
  written to signal — and rejected `Fraction`, though a RATIO is a REAL.
  **And `LOAD` did not bind `*PACKAGE*` (CLHS 24.1).** It restored the package
  only when it had been `None` on entry, and not in a `finally`, so a *nested*
  load's `IN-PACKAGE` leaked into the rest of the enclosing file: `init.lsp`'s
  second form loads `gclload1.lsp`, whose `(in-package :cl-test)` then stayed
  current, so `init.lsp`'s third form was read in `CL-TEST` and `*ROOT-PATH*`
  interned as a different symbol from the `CL-USER` one its own `DEFVAR` had
  bound. Global lookup is by symbol *identity*, not name, so the failure reads
  as "Unbound variable: `*ROOT-PATH*`" immediately after a successful `DEFVAR`
  of that name — and it aborted the rest of `init.lsp` every time.
  **Measured:** full run **11548 → 14772 of 22113**, `COMPLETENESS: OK`,
  0 missing. `pytest` 1642 passed, 1 pre-existing unrelated failure
  (`STREAM-ELEMENT-TYPE`). Four loops crossed the 120s warning and **all four
  resolved**; none reached the cap. Wall time rose ~67 → ~113 minutes, which is
  the expected direction — the old figure was partly cheap because a
  `check-type-error` that never calls its function returns fast.
  **Discovered, not fixed:** `types-and-classes` fell 14.1 points (see
  [preventing regression](#preventing-regression)); a new
  transcendental/float regression cluster of ~11 files; and the reader does not
  parse ratios — `3/5` reads as an unbound *variable*, so `*mini-universe*`'s
  ratio entry is not a ratio at all.

- **2026-08-15 (d)** — **One array object model.** CLHS 15.1 gives an array five
  properties — dimensions, element type, adjustability, fill pointer,
  displacement — and fclpy had a representation for none of them. There were
  *three* unrelated Python shapes: `vectors.AdjustableVector` (a 1-D vector with
  a fill pointer, which is also what the reader built for a `#(...)` literal, so
  every **simple** vector claimed to be adjustable), `vectors.Array` (a separate
  multi-dimensional class that was not even `ARRAYP`), and a bare Python `list`
  for everything else. None recorded an element type, so `MAKE-ARRAY` discarded
  `:element-type` outright — `(make-array 5 :element-type 'bit)` was a vector of
  NIL — `:displaced-to` was accepted and ignored, and `ARRAY-ELEMENT-TYPE`
  returned the Python string `'T'` (standing rule 2).
  **The operators were duplicated across five modules and import order picked
  the winner** (standing rule 3, Finding L): `vectors.py`'s fill-pointer-aware
  `AREF`, `VECTOR-PUSH`, `ARRAY-DIMENSION(S)` and `ADJUSTABLE-ARRAY-P` all
  *lost* to copies in `sequences_higher.py` / `misc_hashtables.py` /
  `math_arithmetic.py` / `core.py` that knew nothing about any of it — the live
  `VECTOR-PUSH` was `vector.append(...)`, which an array object does not have,
  so it leaked an `AttributeError` as the value of the form, and the live
  `AREF` indexed one subscript at a time, so a 2-D reference raised
  `IndexError: Expected 2 indices, got 1` (both are rows in [X1](#x1-python-exceptions-leaking-as-lisp-values)'s
  leak table). `ADJUSTABLE-ARRAY-P` was a stub returning NIL; `ARRAY-ROW-MAJOR-INDEX`
  returned 0; `ROW-MAJOR-AREF` returned None.
  **`lispfunc/arrays.py` is now the one model and the one home for every array
  operator**, with the same shape as `sequence_protocol.py`: **three
  representations, one protocol.** A Python `list` is a *simple general vector*,
  a `LispString` is a character vector, and `LispArray` is everything else — any
  other rank, any specialized element type, any fill pointer, adjustability or
  displacement. `_new_array` is the only place that decides which, and nothing
  asks `isinstance` (Finding M) — `array_rank_of` / `array_dimensions_of` /
  `element_type_of` / `fill_pointer_of` / `row_major_get` answer for all three.
  Displaced arrays forward every access to their target rather than copying, so
  writes are visible through both.
  **Measured, same runner both sides:** `run_ansi.py arrays` **518 → 1233 of
  1356** — **+715**, failures 838 → 123, `arrays/` 42.8% → **90.1%** in the
  checklist. `pytest` 1642 passed with the same 1 pre-existing unrelated
  failure (`STREAM-ELEMENT-TYPE`), plus 43 new tests in
  `tests/test_array_model.py`. Those replaced
  `test_phase5_task3_vectors.py`/`test_phase5_task4_arrays.py` (648 lines),
  which certified the `vectors.py` classes — i.e. the copies that had *lost* the
  registry, so no Lisp form could reach the code they tested. One of them
  asserted `(array-dimension <fill-pointer 5, size 10> 0)` = 1, a row in
  [§3's non-ANSI assertion table](#known-non-ansi-assertions-in-the-unit-suite)
  that is now gone.
  **Three mechanisms outside `arrays/` moved with it, and each was found by the
  array work rather than aimed at.** (1) **`COND` answered the unevaluated
  *form* of a body-less clause**: `(cond ((+ 1 2)))` was the list `(+ 1 2)`,
  not 3 (CLHS 5.3 says the value of the test). ansi-test's own
  `make-array-with-checks` — and every aux helper written as one long `cond` of
  test-only clauses — returns exactly that shape, so the harness compared the
  check's *source text* against the expected value and **no test using one
  could pass whatever the implementation did**. That is the measurement-gate
  shape of [§4](#recommended-order) items 4 and 6, a third time. (2) **A
  keyword argument repeated in a call took the *rightmost* pair**, where CLHS
  3.4.1.4.1 takes the leftmost — which ansi-test checks directly with
  `:allow-other-keys t :allow-other-keys nil` — and **an odd number of keyword
  arguments passed the dangling keyword on as a positional argument**, so the
  callee raised a Python `TypeError` where CLHS 3.5.1.6 requires a
  PROGRAM-ERROR. Both are in the one argument-passing site in
  `evaluation_core.py`. (3) **`EQUAL` now descends bit vectors** (CLHS 5.3), so
  `(equal #*101 #*101)` is T.
  **And one shared place accessor.** `SETF`, `PSETF`, `INCF`, `DECF` and
  `ROTATEF` each open-coded the `AREF` place, and **every copy read exactly one
  subscript** — `(setf (aref a i j) v)` silently wrote element `i` — and every
  copy "helpfully" extended a Python list when the index was out of range,
  turning an error into a longer vector (standing rule 4). One reader/writer
  pair in `arrays.py` now serves all five, and it covers `SVREF`, `BIT`,
  `SBIT`, `ROW-MAJOR-AREF` and `FILL-POINTER` as well. This is *not* M5: the
  place ladder is untouched, only its array rung.

  **Not yet measured — do this first on the next run.** Only `arrays` was run
  to completion after the last three changes, and the checklist was amended
  with it. The **cross-group regression sweep was started and stopped**, so the
  COND fix, the keyword-argument rules and `EQUAL` on bit vectors have *no*
  measured blast radius yet. All three are shared mechanisms with wide reach,
  and the COND one in particular changes what a great many ansi-aux helpers
  return, so it should move numbers well outside `arrays/` — in which
  direction is unverified. Run
  `run_ansi.py sequences printer strings types-and-classes cons misc iteration
  data-and-control-flow --update-checklist`, then diff against
  `docs/ansi_checklist_baseline.json` per [the development loop](#the-development-loop)
  step 7, before treating any of it as landed.

  <a id="discovered-2026-08-15-d"></a>
  **Discovered, diagnosed, not fixed:**
  - **A nested destructuring pattern in a `defmacro` lambda list does not
    bind.** `(defmacro multiple-value-bind* ((&rest vars) form &body body) ...)`
    — ansi-aux's own helper — signals `Unbound variable: VARS` when expanded, so
    every ansi-test check that goes through `multiple-value-bind*` (including
    `subtypep-or-unknown`, which `make-array-with-checks` calls for *every*
    array) fails there. **This is now the whole residual of
    `arrays/make-array.lsp`**, 90 of its 118 tests, and it is M3/M4's, not
    arrays'. It is a cheap, well-localized target with a large blast radius:
    the pattern is idiomatic in the harness.
  - **`TYPE-OF` returns uninterned symbols** on most of its branches
    (`lisptype.LispSymbol('VECTOR')` rather than the `CL` symbol), so its
    result prints as `#:VECTOR` and is not `EQ` to the symbol a caller wrote.
    The array branches were fixed here; the rest were left alone as their own
    change.
  - **`numbers/number-comparison.lsp` fails to load** with
    `bad operand type for abs(): 'lispNull'` — a load-time failure, so the 145
    tests in it never register. Not investigated; noticed while running a
    multi-group target.
  - `SUBTYPEP` still has no lattice ([C14](#tier-2--subsystem-gaps)), which is
    what `UPGRADED-ARRAY-ELEMENT-TYPE.8` measures, and it also makes
    `make-array-with-checks`' element-type checks vacuous rather than failing.
- **2026-08-15 (c)** — **LOOP's clause vocabulary, in the one engine.** C1 gave
  LOOP a single iteration engine over composing drivers; what it did not give
  it was the *clauses*. Nine keywords — `WITH`, `MAXIMIZE`/`MAXIMIZING`,
  `MINIMIZE`/`MINIMIZING`, `NEVER`, `OF-TYPE`, and the `BEING` families for hash
  tables and packages — were absent from the parser, and **an absent keyword was
  silently dropped**, so the loop ran and returned a plausible wrong answer:
  `(loop for x in '(1 5 3) maximize x)` was NIL, and
  `(loop for x in '(1 2 3) never (> x 5))` was NIL — which means its sibling
  `never (> x 2)` had been *passing for the wrong reason*. `WITH` was worse than
  dropped: its token fell into the loop body and evaluated as a free reference,
  so every WITH loop signalled `Unbound variable: WITH`.
  **`iteration` 424 → 636 of 843. +212, 0 regressions**, and the shape of the
  movement is the point — only ~140 of the 212 were in the files targeted:
  | file | before | after | | file | before | after |
  |---|---|---|---|---|---|---|
  | `loop8.lsp` (WITH) | 27 failing | **0** | | `loop1.lsp` | 24 | **15** |
  | `loop6.lsp` (hash) | 47 | **15** | | `loop2.lsp` | 14 | **5** |
  | `loop7.lsp` (package) | 35 | **26** | | `loop3.lsp` | 16 | **6** |
  | `loop10.lsp` (numeric) | 62 | **23** | | `loop5.lsp` | 15 | **7** |
  | `loop12.lsp` (bool) | 22 | **11** | | `loop15/16.lsp` | 19/19 | **3/3** |
  loop1/2/3/5/11/13/15/16/17 were not targeted at all; they move because
  type-specs and destructuring are used throughout them.
  **Three sub-mechanisms did that untargeted work, and each replaced a partial
  copy rather than adding a branch.** (1) **One type-spec parser**
  (`_loop_type_spec`) for all three positions a type-spec can occupy — after a
  FOR variable, after a WITH variable, after a numeric accumulation's form.
  Only the numeric accumulations may consume one, because `collect x` followed
  by `t` would otherwise lose the T. (2) **One destructurer**
  (`_loop_destructure`), a recursive walk replacing three enumerated shapes;
  the shapes the enumeration could not express are exactly what was failing —
  a dotted tail `(a b . rest)`, a NIL hole `(nil . v)`, and a pattern longer
  than its value. (3) **One early decision** for ALWAYS/NEVER/THEREIS instead of
  two flags with different "did it fire?" tests (`always_failed` versus
  `thereis_result is not None`), which is what made NEVER an addition rather
  than a third convention.
  **Landed with it, because the new hash driver could not be correct without
  it: a hash table no longer stores its own options as entries.**
  `MAKE-HASH-TABLE` returned a plain `dict` carrying its test and sizing in
  three `'__hashmeta__...'` **keys**, i.e. in the key space that holds user
  entries. Four places knew to filter them (MAPHASH, CLRHASH,
  HASH-TABLE-COUNT, the printer) and everything else did not, so
  `(loop for k being the hash-keys of h collect k)` collected the Python string
  `"__hashmeta__test"` as a Lisp value (standing rule 2) — and filtering it in
  the driver would have been a fifth copy. `HashTableDict` keeps them as
  attributes, so a traversal is correct by default and the four filters are
  **gone**, not five.
  **And one shared package enumerator.** `for x being the symbols of p` needed
  the accessible/present/external distinction that `DO-SYMBOLS` and
  `DO-EXTERNAL-SYMBOLS` already open-coded, so `coerce_to_package` and
  `package_symbols` now serve all three. The consolidation found a live bug in
  the copy it replaced: `use_packages` holds package *names* as well as
  `Package` objects (`Package.intern` handles both), and DO-SYMBOLS read
  `external_symbols` straight off each entry — a string entry yields the empty
  set, so every inherited symbol was silently skipped. LOOP's own copy was
  worse: it swallowed a failed package lookup with a bare `except Exception`
  and iterated an *empty* package, so a misspelled name returned 0 instead of
  signalling (standing rule 4).
  **Measured, before → after, each `before` in a stash of the same tree:**
  | target | before | after |
  |---|---|---|
  | `iteration` | 424 of 843 | **636** |
  | `sequences`, `misc`, `types-and-classes`, `structures` | 3505 of 6291 | **3513** |
  | `hash-tables`, `packages`, `symbols`, `data-and-control-flow`, `cons`, `conditions`, `eval-and-compile`, `environment` | 4131 of 6305 | 4131 — unchanged |
  **+220 over 13,439 tests with a per-test diff of 0 regressions in all three.**
  The +8 outside `iteration` is `SEARCH-BITVECTOR.1`, `SEARCH-LIST.1`,
  `SEARCH-STRING.2`, `SEARCH-VECTOR.3/.5/.7` and the two
  `ALL-*-CLASSES-ARE-SUBTYPES-OF-*` tests — all of them aux code that drives its
  own assertions with LOOP, which is the blast radius C1 predicted.
  `pytest` 1616 passed (from 1552), same 1 pre-existing unrelated failure
  (`test_all_expected_functions_are_registered`), plus 64 new tests in
  `tests/test_loop_clauses.py`. They are pinned together rather than beside each
  feature because they share a failure *mode*, not a feature: several assert a
  value for a loop that already "worked", since a dropped clause produced a
  wrong answer rather than an error.
  **Discovered, diagnosed, not fixed:** **`SORT` does not preserve the sequence
  type.** `(sort (list 3 1 2) #'<)` returns a *vector*, and
  `(sort (copy-seq "cba") #'char<)` returns `#("a" "b" "c")` rather than
  `"abc"` — CLHS 17.1 requires the result to be of the same type as the
  argument. This is **the whole of what is left in `loop6.lsp` and
  `loop7.lsp`**: LOOP.6.6–.18 and LOOP.7.1–.20 all wrap their result in
  `(sort ... #'symbol<)`, so 41 of the 41 residual failures in those two files
  are SORT's, not LOOP's. It is also `sequences/sort.lsp` 20/34 and
  `stable-sort.lsp` 20/34, and it belongs with the sequence result-type
  discipline (C3/M6), not here. Also found: **two hash-table implementations**
  coexist — `lispfunc/hashtables.py`'s `HashTable` class and
  `lispfunc/misc_hashtables.py`'s dict, both registering `MAKE-HASH-TABLE` and
  `GETHASH`; the dict wins and the class is dead (standing rule 3, Finding L).
- **2026-08-15 (b)** — **The divide-then-round family is exact, and returns two
  values.** All eight of FLOOR/CEILING/TRUNCATE/ROUND and their F- variants
  computed `x / divisor` — Python **float** division — before rounding, so every
  one of them silently lost precision above 2**53:
  `(ceiling (+ (expt 2 62) (1+ (expt 2 62))) 2)` was one *less* than the true
  midpoint. `_exact_quotient` routes rationals through `Fraction`, on which
  `math.floor`/`math.ceil`/`int`/`round` are all exact — including `round`'s
  half-to-even, which is the rule CLHS gives ROUND.
  **The bug presented as a hang, not a wrong answer, and that is why it had
  survived.** `integer-binary-search` (`auxiliary/numbers-aux.lsp:46`) steps
  with `(ceiling (+ lo hi) 2)`, so once `lo` passed 2**53 the midpoint rounded
  back to `lo` itself, `(setq lo mid)` became a no-op and the search ran until
  the 600s watchdog killed it — 1,335,702 iterations, **15% of the whole ANSI
  run's wall time in one form**, reached from `numbers/sqrt.lsp`'s
  `(find-largest-exactly-floatable-integer most-positive-fixnum)`. This is the
  last of the never-terminating loops [C1](#c1-loop-clause-composition--done-2026-08-12-f)
  catalogued on 08-12 (SQRT.12–.17, DEPOSIT-FIELD.1–.5, DPB.2), and **it was
  never a LOOP defect** — the 08-12 diagnosis attributed it to the wrong
  subsystem. `run_ansi.py numbers/sqrt.lsp` went from a 600s abort to **1.8s**.
  **Landed with it**, because the same eight functions were failing their whole
  files for a second, unrelated reason: they returned the quotient **alone**
  where CLHS 12.2 requires *quotient and remainder*, and every ansi-test helper
  for them opens with `(eql (length vals) 2)` — so nothing in those files could
  pass whatever the quotient was. And `REM` was Python's `%`, which is
  floor-based: right for MOD, wrong for REM whenever the operands differ in sign
  (`(rem -7 2)` gave 1, ANSI requires -1). REM and MOD are now the remainders of
  TRUNCATE and FLOOR rather than a third implementation of "remainder"
  (standing rule 3).
  **Measured, before → after on the same eight files:**
  | file | before | after | | file | before | after |
  |---|---|---|---|---|---|---|
  | `round.lsp` | 2 of 23 | **10** | | `fceiling.lsp` | 2 of 13 | **12** |
  | `truncate.lsp` | 2 of 21 | **15** | | `ffloor.lsp` | 2 of 13 | **12** |
  | `floor.lsp` | 2 of 21 | **15** | | `ftruncate.lsp` | 2 of 13 | **12** |
  | `ceiling.lsp` | 2 of 21 | **15** | | `fround.lsp` | 2 of 13 | **12** |

  **16 → 103 of 138, +87.** `pytest` 1518 passed, same 1 pre-existing unrelated
  failure, plus 34 new tests in `tests/test_math_rounding.py` — which exist
  *because* the precision defect manifested as a hang: ansi-test covers the
  two-values contract cleanly, but a watchdog kill is not a failure signal, so
  the exactness property and the loop-termination property are pinned where
  they fail fast and say why.
  **Discovered, not fixed:** a `MultipleValues` reaching the printer renders as
  `#<MULTIPLEVALUES 0x...>` (standing rule 2). It does not affect RT, which
  compares through `multiple-value-list`, but a top-level multiple-value return
  should print as its values.
- **2026-08-15** — **M2: a global variable has one home.** The global
  environment no longer has lexical variable bindings, because Common Lisp
  does not have them: CLHS 3.1.1.1 makes the global environment's variable
  bindings the *dynamic* ones. `Environment.is_global` is true for the
  parentless environment at the root of every chain, and its `add_variable`/
  `find_variable`/`has_variable`/`set_variable` read and write the symbol's
  value cell — the cell `SYMBOL-VALUE`/`BOUNDP`/`SET`/`MAKUNBOUND`/`PROGV` and
  every dynamic binding already used. So `(defvar *x* 1)` now leaves
  `(boundp '*x*)` T, `(let ((*x* 2)) *x*)` reads **2**, and `(set '*x* 4)` is
  visible to a plain reference.
  **The predicted fix was wrong, and how it was wrong is the point.** [§4](#recommended-order)
  item 8 said the fix "has to move `SETQ` and `eval`'s lookup order with it".
  It moved neither, and neither needed moving: once the global lexical binding
  is gone, `SETQ`'s chain walk already ends at the value cell, and "lexical
  chain, then value cell" already resolves to the innermost binding **because
  the value cell is the end of the chain**. The defect was never that the
  lookup order was wrong; it was that there was a home for it to find first.
  Two homes → one, by deleting the one that should not exist.
  Landed with it: `binding.proclaim_special` is the single writer of the
  proclamation table `is_proclaimed_special` reads, replacing three inline
  copies in `eval_defvar`, `eval_defparameter` and `_store_special_declaration`;
  the **standard variables are proclaimed special at bootstrap**
  (`lispenv.STANDARD_SPECIAL_VARIABLES`, CLHS Figure 25-1), without which
  `(let ((*print-base* 2)) ...)` binds lexically and the printer — which reads
  the variable from Python through the *global* environment — never sees it;
  `(defvar *x*)` with no initial-value form no longer binds the variable to NIL,
  per CLHS, and `DEFVAR`'s "already bound?" test asks the value cell rather than
  whatever lexical binding surrounds the form; and the standard stream
  variables are re-initialized on a full bootstrap rather than guarded by
  `if find_variable(...) is None`, which also removes a latent
  `UnboundLocalError` where four of them referenced a `stdout_stream` created
  inside another's guard.
  **Measured, before → after on the same targets, same runner both sides
  (each `before` run in a stash of the same working tree, so like-for-like):**
  | target | before | after |
  |---|---|---|
  | `data-and-control-flow` | 1023 of 1428 | **1037** |
  | `packages` | 140 of 500 | **147** |
  | `eval-and-compile` | 234 of 318 | **236** |
  | `iteration`, `symbols`, `conditions`, `cons`, `environment`, `hash-tables`, `types-and-classes` | — | unchanged, 0 regressions |
  **+23, 0 regressions, and only 3 of the 23 were targeted** — `DEFVAR.3`,
  `DEFPARAMETER.3`, `DEFCONSTANT.1`. The other 20 moved because the mechanism
  moved: `LET.3`, `LET*.3`, `PROGV.6A`, `SETQ.5`, `PSETQ.8`/`.9`, `SETF.5`,
  `MULTIPLE-VALUE-BIND.7`, `FLET.40`, `FLET.69`, `LAMBDA-LIST-KEYWORDS.1`,
  `DEFINE-COMPILER-MACRO.7`/`.8`, and **all seven `IN-PACKAGE.7`–`.13`**, which
  move because `*PACKAGE*` is now genuinely special rather than a global
  lexical binding with a Python-side mirror. `pytest` 1518 passed (from 1494),
  same 1 pre-existing unrelated failure (`test_all_expected_functions_are_registered`);
  the 4 `xfail`s in `TestTheGlobalValueCellDefect` are now 12 passing tests in
  `TestAGlobalVariableHasOneHome`, plus a new `TestTheStandardVariablesAreSpecial`.
  **Two unit tests asserted the defect and were corrected with citations**
  (§7: when `pytest` and `ansi-test` disagree, the unit test is wrong).
  `TestLetStar` probed "LET* left nothing behind" with `(boundp '*bv*)` **=> NIL**,
  which only read NIL *because* `DEFVAR` was broken; it now asserts the value is
  restored to the DEFVAR value. `test_condition_in_lisp_env` stored with one
  freshly built `LispSymbol('*ERROR*')` and read back with a *second* one,
  which worked only because global bindings were keyed by symbol **name** —
  they are the symbol's own value cell now, so two uninterned symbols sharing a
  name are two variables, as CLHS requires.
  **Fixed en route, in the measurement instrument itself:** `scripts/run_ansi.py`
  never established `(in-package :cl-test)`. `gclload2.lsp` — the file the
  targeted runner stands in for — opens with it, and `gclload1.lsp`'s own
  in-package does not carry over because `LOAD` binds `*PACKAGE*` for the extent
  of a file (CLHS 24.1), just as in a conforming Lisp. So every aux preamble was
  read in `CL-USER`, and `auxiliary/types-aux.lsp`'s `*subtype-table*` became a
  *different symbol* from the `CL-TEST::*SUBTYPE-TABLE*` that `ansi-aux.lsp`
  binds. The old name-keyed global environment had been silently conflating the
  two; identity-keyed value cells surfaced it as TYPES.9/TYPES.9A failing, which
  is how it was found. **A targeted run now reproduces the full-suite package
  context it is supposed to**, and TYPES.9/.9A pass for the right reason.
  **Discovered, diagnosed and deliberately not fixed here:** a lambda list binds
  a *proclaimed special* parameter lexically — `(defvar *sv* 1)` then
  `(defun f (*sv*) (g))` leaves `g` seeing 1, not the argument. Verified
  directly. The six copy-pasted binders are exactly what M3 exists to
  consolidate onto `BindingFrame`, and repairing them one at a time here is the
  "seventh incompatible mechanism" [§4](#recommended-order) warns about. See
  [§5](#5-known-temporary-deviations).
- **2026-08-14 (b)** — **M2: one binder decides lexical vs. dynamic.**
  `fclpy/lispfunc/binding.py`'s `BindingFrame` is now the only place that
  answers "is this variable special here", and LET, LET* and all eight
  iteration forms (DO, DO*, DOLIST, DOTIMES, LOOP, DO-SYMBOLS,
  DO-EXTERNAL-SYMBOLS, DO-ALL-SYMBOLS) go through it. Establishing a binding
  and stepping it are one operation, `frame.bind`: the first call decides where
  the binding lives, later calls assign to that same binding — which is also
  what makes successive iterations share one binding (DO.15).
  **The defect was not "the iteration variable is mis-bound", it was that the
  clause establishing it never bound anything.** All eight forms established
  their variable with `Environment.set_variable`, which *walks the environment
  chain and mutates the first binding of that name it finds*; since
  `Environment.__init__` hands a child its parent's `variable_bindings` list,
  that walk always reached an enclosing binding. So `(let ((x 99)) (dolist (x
  '(1 2 3))) x)` was NIL, and — because rt.lsp's failure reporter takes its
  output stream in a parameter named `s` — a `(do-all-symbols (s) ...)` or
  `(loop for s = ...)` in the suite overwrote RT's own stream with a symbol.
  **That was a measurement gate, the third of this shape after the
  string-is-a-vector gate (08-13) and the `*STANDARD-OUTPUT*` gate (08-14):
  `run_ansi.py printer` could not run to completion at all.**
  **Why the one-word fix was wrong.** `add_variable` for the establishing call
  fixes all eight leaks and measured `iteration` 410 → 408: it gains DOLIST.14
  and DOTIMES.16 and loses DO.14, DO*.14, DOTIMES.18 and .18A, which declare
  the iteration variable special in the body and so must be bound *dynamically*
  — the rule that lived in `eval_let` and, copy-pasted, in `eval_letstar`.
  **LET*'s copy was not merely duplicated, it was wrong**: for a special
  variable it called `global_env.add_variable`, putting a *lexical* binding in
  the global environment that outlived the LET* and was invisible to
  `SYMBOL-VALUE`. Two copies and eight absences → one.
  **The distinction the shared binder had to get right** is declaration vs.
  proclamation: a local `(declare (special x))` governs the form it heads and
  free references within it, but must *not* make a nested binding form bind
  dynamically, while a `DEFVAR` proclamation must. DOTIMES.17 and .18 differ
  only in whether the loop body declares the variable, and expect `(0 0 0 0)`
  and `(3 2 1 0)` respectively — so `is_proclaimed_special` consults the root
  environment only, and walking the chain (the obvious "more correct" reading)
  collapses the pair. Landed with it: a binding form's *free* special
  declarations now redirect through the same `%SPECIAL-REF` symbol macro
  LOCALLY already used (`special_reference`), which is what DOLIST.17 and DO.17
  need for a result form and a step form respectively; `eval_locally` reuses
  the shared declaration parser instead of its own inline copy; declarations
  are stripped from an iteration body before it runs as a TAGBODY, where a
  declaration is not a statement; and a bare symbol in LET*'s binding list
  binds to NIL (CLHS 3.1.2.1.1) instead of being skipped and left unbound.
  **Landed with it, and it turned out to matter more than the binder: the
  `*PACKAGE*` mirror was never restored.** The restore was guarded by
  `if old_package is not None`, which conflates "nothing was saved" with "None
  *is* the saved value" — and None is `state.current_package`'s normal state
  until something binds `*PACKAGE*`, because a plain reference falls back to a
  default. So the **first** `(let ((*package* p)) ...)` of a session never
  restored, and every symbol read afterwards interned into `p`. Found by a
  smoke test whose every later form came back with keywords where it had
  written symbols; it is why `packages` could not complete.
  **Measured, before → after on the same targets** (each run in a worktree at
  the previous commit, so these are like-for-like and not full-run numbers):
  | target | before | after |
  |---|---|---|
  | `iteration` | 410 of 843 | **424** of 843 |
  | `data-and-control-flow` | 1022 of 1428 | **1023** of 1428 |
  | `cons` | 868 of 1882 | 868 — unchanged |
  | `conditions` | 159 of 664 | 159 — unchanged |
  | `packages` | **crashes** — `ConditionException: Not an output stream: #\f` | **140 of 500, completes** |
  | `printer/print-strings.lsp` | registers 16, **no result** | **8 of 16, completes** |
  | `printer/print-symbols.lsp` | registers 31, **no result** | **7 of 31, completes** |
  The bottom three are RT's own report stream being overwritten by a loop
  variable and then printed to; **that they now complete is the untargeted
  movement that says this was a mechanism** and not a repair to `iteration`.
  A whole-directory `run_ansi.py printer` run still does **not** complete, but
  it now reaches `printer/format/` (FORMAT.S.7) instead of dying in
  `print-strings.lsp`, on an unrelated defect — `ValueError: I/O operation on
  closed file` leaking as a Lisp value ([X1](#x1-python-exceptions-leaking-as-lisp-values))
  and then an abrupt exit with no traceback. **That is the next thing in front
  of the printer directory, and it is not this one.**
  **Per-test diff on `iteration`: 16 fixed, 2 lost — and both losses are false
  passes the leak was manufacturing.** Fixed: DO.17/.18/.19, DO*.17/.18/.19,
  DOLIST.6/.14/.17, DOTIMES.16/.17/.17A/.23/.23A, LOOP.2.17, LOOP.3.17 — only
  five of which were targeted. Lost: LOOP.14.38 and LOOP.14.39, which are
  `(loop for x in '(1 2 nil 3 4 nil 5 nil) when x count it)`. **LOOP's `IT` is
  not implemented at all** — `(loop ... count it)` signals `Unbound variable:
  IT` in isolation both before and after this change. They passed only because
  `iteration/loop14.lsp:260` runs `(loop for it on '(a b c d) ...)` earlier in
  the same file and the old leak left `it` bound at the root to a truthy value,
  so `count it` counted a leaked constant once per iteration where `when x`
  held, arriving at 5 by coincidence. Standing rule: a test that passes for the
  wrong reason is not progress, so these were not preserved; `IT` is now a
  visible [C1 follow-up](#c1-follow-ups-still-open) instead of an invisible one.
  `pytest` 1494 passed (from 1457), same 1 pre-existing unrelated failure
  (`test_all_expected_functions_are_registered`); the 13 `xfail`s in
  `tests/test_iteration_variable_binding.py` are now passing tests, and the
  module grew coverage for the special-vs-lexical decision, unwinding on a
  non-local exit, the `*PACKAGE*` mirror, and LET*'s two repairs.
  **Discovered, diagnosed and deliberately not fixed here:** a special variable
  has **two homes that never reconcile** — `DEFVAR`/`SETQ` maintain a lexical
  binding in the global environment, `SYMBOL-VALUE`/`BOUNDP`/`PROGV` and every
  dynamic binding use the value cell, and `eval` checks the lexical chain first,
  so `(defvar *x* 1)` leaves `(boundp '*x*)` NIL and `(let ((*x* 2)) *x*)` reads
  1. Consolidating the binder is what isolated it: the frame's dynamic bindings
  are provably correct through `SYMBOL-VALUE`, so the residual wrong answer is
  entirely the global lexical binding's. See [§5](#5-known-temporary-deviations)
  and the 4 `xfail`s in `TestTheGlobalValueCellDefect`; it is M2's next slice,
  and the fix has to move `SETQ` and `eval`'s lookup order with it.
- **2026-08-14** — **C7: there is one printer, and output goes where the
  language says.** `fclpy/printer.py` — previously a complete printer that
  *nothing under `fclpy/` imported* — is now the only one, and every Lisp-visible
  printed representation comes from it: `PRIN1`, `PRINC`, `PRINT`, `WRITE`, the
  three `*-TO-STRING`s, and FORMAT's `~A`/`~S`. Deleted: `PrinterSettings` and
  its `@cl_function('*PRINT-...*')` accessors, the unreachable
  `_print_with_limits`, the `@cl_function('*STANDARD-OUTPUT*')`-style accessors
  returning raw `sys.stdout`, and `_write_stream_output` (a strictly worse
  duplicate of the new single `write_text` funnel). Three printers → one
  (standing rule 3).
  **The gate mattered more than the printer.** Every output function with no
  stream argument wrote to Python's `print()` rather than to the value of
  `*STANDARD-OUTPUT*`, and every `def-print-test` in `printer/` captures via
  `(with-output-to-string (*standard-output*) (prin1 form))` — so ~440 tests read
  the empty string regardless of the code under test, exactly as the
  string-is-a-vector gate did in the 2026-08-13 entry. `(format t ...)` was the
  same bug with a twist: FORMAT's `t` means `*STANDARD-OUTPUT*` (CLHS 22.3.1),
  not `*TERMINAL-IO*` as it would for a stream designator.
  Landed with it: the control variables are real variables with the ANSI initial
  values from one table (`printer.PRINTER_VARIABLES`, so bootstrap and printer
  cannot disagree — `*PRINT-RIGHT-MARGIN*` and `*PRINT-MISER-WIDTH*` are NIL, not
  80 and 40); `*PRINT-BASE*` 2–36 with upper-case digits and the radix prefix
  before the sign (`#b-1`, `#3r-11`, `10.`); `*PRINT-LEVEL*` applied to
  aggregates only, at `>=`, so an atom is never `#`; ratios as `n/d` and
  complexes as `#C(r i)` instead of Python's `Fraction(1, 2)` and `(1+2j)`;
  vectors as `#(...)` (a Python `list` is a *vector* here, and `str()` printed it
  as a list, so every vector read back as a cons) and arrays as `#2A((0 0) (0 0))`
  instead of `#(ARRAY (2, 2))` — a Python tuple's repr inside claimed Lisp
  syntax; the full CLHS 22.1.3.3.2 `READTABLE-CASE` × `*PRINT-CASE*` matrix;
  WRITE's keyword arguments, which were collected into `**kwargs` and dropped,
  plus `:allow-other-keys`; `PRINT` as newline-object-space rather than the
  reverse; and `FRESH-LINE`/`~&` as actual fresh lines — both had emitted
  unconditionally, `~&` with the comment "we don't track column".
  **Measured: the 25 `printer/` object-printing files 36 → 128 passing of 306,
  +92, 0 newly failing.** `iteration` 409 → 410, no regressions. `pytest` 1457
  passed, 1 pre-existing unrelated failure (`STREAM-ELEMENT-TYPE`), 16 xfailed;
  115 new tests in `tests/test_printer_ansi.py`, and `tests/test_printer.py`'s 7
  non-ANSI assertions corrected with citations (it asserted `PRINC` = `PRIN1` for
  keywords and characters, which pinned the two-representations bug, and `\n`
  escaping inside strings, which CLHS 2.4.5 forbids). Deleted
  `tests/test_printer_control.py`, 273 lines certifying the dead
  `PrinterSettings`/`_print_with_limits` pair — the same pathology as the dead
  `reader.py`'s 177 tests.
  **Discovered, diagnosed, and deliberately not fixed here:** all eight iteration
  forms *assign to* an enclosing variable of the same name instead of binding
  their own, so `(let ((x 99)) (dolist (x '(1 2 3))) x)` is NIL and
  `(do-all-symbols (s) ...)`/`(loop for s = ...)` clobber rt.lsp's own report
  stream parameter — which is why a whole-directory printer run cannot complete.
  See [§5](#5-known-temporary-deviations) and the 13 `xfail`s in
  `tests/test_iteration_variable_binding.py`; it is M2's, and the one-word fix
  regresses the four tests that bind the variable *specially*.
- **2026-08-13** — **A string is a vector, and its elements are characters.**
  Four functions type-tested `isinstance(x, str)`, which is false for every
  `LispString` the reader makes: `EQUAL`/`EQUALP` (so `(equal "abc" "abc")` was
  **NIL**), `TYPEP`'s `STRING` branch, and `CHARACTERP` (which also missed the
  `Character` class *and* returned a raw Python bool, the dangerous direction
  given `is_truthy(False)` is true). `TYPEP` additionally excluded strings from
  `VECTOR`/`ARRAY`, contradicting CLHS 15.1.
  **Why this mattered more than any cluster:** rt.lsp's `equalp-with-case`
  compares via `(typep x 'vector)` and walks elements; with strings not vectors
  it fell through to `EQL`, so *every* string-valued test failed no matter what
  the code under test returned. Fixing `TYPEP` then exposed the second half —
  `AREF`/`LOOP across`/`MAKE-ARRAY` yielded bare length-1 Python strings, so a
  "character" was also a one-element string and therefore a one-element vector,
  and element-wise traversal recursed until the stack died, aborting whole runs.
  String element access now yields `Character` through one shared
  `string_element`. **8 `LOOP.5.*` tests that had been passing via the EQL
  conflation are the proof the old behaviour was wrong, not the fix** — they
  expect `(#\a ...)`; all are passing again for the right reason.
  Also landed: **`WITH-OUTPUT-TO-STRING`/`WITH-INPUT-FROM-STRING`/
  `WITH-OPEN-STREAM` are real macros.** They were `cl_function` stubs that
  returned their last body form unevaluated — and because `cl_function`
  evaluates arguments eagerly, the binding spec `(stream)` was evaluated as a
  call, failing with `Undefined function STREAM`. Every `FORMATTER.*` test in
  the suite is written in terms of `WITH-OUTPUT-TO-STRING`, so this alone gated
  the 638 `FORMATTER` failures. **Three registrations of each existed**
  (`misc_macros.py`, `io_read.py`, `io_write.py`) and the undecorated ones would
  still auto-register via `register_module` because its dedup is by *Python*
  name; two deleted (standing rule 3).
  And **C2's iteration half**: `~{...~}`/`~?` tested `isinstance(arg, (list,
  tuple))` — false for the cons list the directive exists to iterate — so
  `(format nil "~{~A ~}" '(1 2 3))` returned `"(1 2 3) "`. `~^` escaped
  unconditionally instead of testing its CLHS 22.3.9.2 condition, and signalled
  via an in-band `' '` marker callers had to `str.replace` out; it is now a
  control transfer carrying its partial output. `~<...~>` processed only its
  last segment (no justification ever happened); `~A`/`~S` honoured only
  `mincol` with a hardcoded space; `~n[`/`~#[` ignored the prefix parameter and
  stole an argument.
  **Measured, before → after on the same seven files:** `format-a` 0→42/107,
  `format-brace` 0→55/152, `format-circumflex` 0→198/470, `format-justify`
  1→22/59, `format-conditional` 3→28/58, `format-question` 0→10/20, `format-s`
  0→33/87 — **4 → 388 of 953 (0.4% → 40.7%), +384, 0 regressions.**
  `iteration` 409 → 409 and `cons` 817 → 838, no regressions. `pytest` 1347
  passed, 1 pre-existing unrelated failure (`STREAM-ELEMENT-TYPE`); 71 new tests
  across `test_format.py`, `test_equality_strings.py`, `test_string_elements.py`.
  Tooling: `run_ansi.py` now collects the `compile-and-load*` preamble from
  *ancestor* directories, without which **no file under `printer/format/` could
  be targeted at all** (they need `def-format-test` from `printer/load.lsp`, one
  level up) — 2 tests registered before, 953 after; and it runs on a big-stack
  thread with a raised recursion limit, since one level of Lisp recursion costs
  ~15 Python frames and rt.lsp's own list comparison cdr-recurses per element.
- **2026-08-12 (f)** — **C1: LOOP has one iteration engine.** `eval_loop` held a
  scalar `iteration_type` and nine near-duplicate loops selected by it, so the
  last iteration-control clause parsed decided which one ran and discarded the
  rest — the cause of both `Unbound variable: X` for `for x = 7 repeat 5` and
  the non-terminating `repeat 5 for x = 7`. All nine are gone; drivers (FOR/AS,
  and REPEAT, now modelled as an anonymous driver) compose in one loop over
  `all(_driver_has_value(...))`, per CLHS 6.1.2. `for var = form` evaluates at
  bind time so a later clause can depend on an earlier driver, WHILE/UNTIL
  compose and are position-aware, and the eight copy-pasted accumulation parse
  branches became one table — which is how `INTO` came to be implemented at all
  (it had been silently dropped in every one of them). Also landed as part of
  the same mechanism: `INITIALLY`, several accumulation clauses per loop, the
  CLHS 6.1.1.4 rule that a FINALLY value overrides an accumulation, and the
  6.1.2.2 rule that ALWAYS/THEREIS skip the epilogue. Fixed en route: a local
  `import fclpy.lisptype` inside `_init_driver` that turned every
  `LispNotImplementedError` in that function into `UnboundLocalError`, and
  `for x to 5`'s start defaulting to `None` instead of 0.
  **`run_ansi.py iteration` 371 → 409 passing of 843, 0 regressions**, and the
  run itself went from LOOP-dominated to **6 seconds**. 13 new tests in
  `tests/test_loop.py`; `pytest` 1259 passed, 1 pre-existing unrelated failure
  (`STREAM-ELEMENT-TYPE`). Tooling: `run_ansi.py --update-checklist` and
  `ansi_checklist.py --merge` now keep `docs/ansi_checklist.md` current from
  targeted runs, so the checklist no longer needs a 4+ hour run to stop being
  stale.
- **2026-08-12 (d)** — `DO-SYMBOLS`/`DO-EXTERNAL-SYMBOLS`/`DO-ALL-SYMBOLS`
  implicit tagbody + NIL block. All three ran their bodies as a flat `eval` over
  the form list, so `(go foo)` raised an uncaught `GoException` to top level,
  aborting the run at `DO-SYMBOLS.8`. **No new helper was written** — the prior
  plan's claim that "the tagbody half has no shared helper yet" was wrong;
  `_exec_iteration_body` already existed and was already used by
  `DO`/`DO*`/`DOLIST`/`DOTIMES`. The fix is those three functions using the
  mechanisms their siblings already use. 7 new tests in
  `tests/test_do_symbols_family.py`; `pytest` 1246 passed, 1 pre-existing
  unrelated failure (`STREAM-ELEMENT-TYPE`). **ANSI impact unmeasured** — the run
  that would measure it was still in flight.
- **2026-08-12 (e)** — **First complete run in the project's history:
  `COMPLETENESS: OK`, 22036/22036 accounted, 0 missing.** M0's central goal, and
  the first trustworthy scoreboard: 8960 passing (40.7%), ~7.5 hours. It
  reordered the checklist immediately — `FORMAT`/`FORMATTER` took first place at
  1623 failures, ahead of LOOP (450), which the truncated data had ranked #1.
- **2026-08-12 (c)** — LOOP `for var = expr` diagnosed as the cause of ~76% of
  full-run wall time; `scripts/run_ansi.py` built; this document restructured
  around the failure checklist.
- **2026-08-12 (b)** — Audit of the *unit* suite for tests asserting non-ANSI
  behavior. Fixed: `(PROGN)` returning Python `None`; `(VALUES-LIST NIL)`
  returning one value instead of zero; two reader tests. Remainder catalogued in
  §3. Discovered `fclpy/reader.py` is a dead second reader certified by 177 tests.
- **2026-08-12 (a)** — **M8's signaling core.** Handlers now run at the signal
  point *before unwinding* (`state.handler_stack`); `HANDLER-BIND` catches
  nothing; `HANDLER-CASE`/`IGNORE-ERRORS` share the stack; one `build_condition`
  replaced three drifted designator constructors; `SIGNAL` became SIGNAL; handler
  type matching delegates to `TYPEP`; condition lattice completed to CLHS Fig 9-1.
  `conditions/` 92 → 116 passing, zero regressions. Unblocked measurement of
  ~79% of the suite: `accounted` 4687 → 8971, `passed` 2920 → 4514.
  Also consolidated three drifted loop-watchdog copies into one `LoopWatchdog`.
- **2026-08-11** — M1 steps 1–2: canonical CL symbol table; deleted the blanket
  `except Exception: pass` at `lispenv.py:513`.
- **2026-08-09** — M0's measurement-corruption bugs: `funcall`'s missing
  non-local-exit re-raise (Finding K); `WARN` routing through `format_fn`;
  `LOOP`'s implicit NIL block (which this document had twice claimed was already
  done); FORMAT argument cursor; the `COMPLETENESS:` assertion, which is what
  made every later truncation visible instead of silent.
