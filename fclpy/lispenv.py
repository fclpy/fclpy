
import fclpy.lisptype
import fclpy.state as state


# Initialize the central environment object in state if not present
if state.current_environment is None:
    state.current_environment = fclpy.lisptype.Environment()

# Backward-compatible module-level name used across the codebase
current_environment = state.current_environment


def setup_standard_environment():
    """Set up the standard environment with proper Lisp function name mappings."""
    # Use centralized state flags and environment
    if state.functions_loaded:
        return state.current_environment
    # Ensure an Environment object exists on state (tests may reset it to None)
    if state.current_environment is None:
        state.current_environment = fclpy.lisptype.Environment()

    # Import here to avoid circular imports
    import fclpy.lispfunc as lispfunc
    # Prefer registry-driven population when available
    try:
        from fclpy.lispfunc import registry as _registry
    except Exception:
        _registry = None

    # If registry is available, populate from it (but do not return early)
    # Some registry entries may be partial; keep fallback mapping to ensure full coverage.
    if _registry:
        for lisp_name, meta in _registry.function_registry.items():
            py_name = meta.get('py_name')
            if not py_name:
                continue
            python_func = getattr(lispfunc, py_name, None)
            if python_func:
                symbol = fclpy.lisptype.LispSymbol(lisp_name)
                # only add if not already present
                if state.current_environment.find_func(symbol) is None:
                    state.current_environment.add_function(symbol, python_func)
        # register specials into environment as markers handled by evaluator
        for lisp_name, meta in _registry.special_registry.items():
            py_name = meta.get('py_name')
            python_func = getattr(lispfunc, py_name, None)
            symbol = fclpy.lisptype.LispSymbol(lisp_name)
            if state.current_environment.find_func(symbol) is None:
                state.current_environment.add_function(
                    symbol, python_func or (lambda *a: f"SPECIAL:{lisp_name}"))

    # Fallback: use the large hand-maintained mapping (unchanged)
    function_mappings = {
        # Core list operations (now registered via decorators in core.py)
        # Arithmetic operator symbols and numeric predicates (now registered via decorators in math.py)

        # List accessors (CAR/CDR combinations) registered via decorators in core.py

        # Predicates
        'ATOM': lispfunc.atom,
        'CONSP': lispfunc.consp,
        'LISTP': lispfunc.listp,
        'NULL': lispfunc.null,
        'SYMBOLP': lispfunc.symbolp,
        'STRINGP': lispfunc.stringp,
        'FUNCTIONP': lispfunc.functionp,
        'COMPILED-FUNCTION-P': lispfunc.compiled_function_p,
        'HASH-TABLE-P': lispfunc.hash_table_p,
        'KEYWORDP': lispfunc.keywordp,
        'ALPHA-CHAR-P': lispfunc.alpha_char_p,
        'ALPHANUMERICP': lispfunc.alphanumericp,
        'GRAPHIC-CHAR-P': lispfunc.graphic_char_p,
        'INPUT-STREAM-P': lispfunc.input_stream_p,
        'INTERACTIVE-STREAM-P': lispfunc.interactive_stream_p,
        'ADJUSTABLE-ARRAY-P': lispfunc.adjustable_array_p,
        'ARRAY-HAS-FILL-POINTER-P': lispfunc.array_has_fill_pointer_p,
        'FBOUNDP': lispfunc.fboundp,

        # Equality and comparison
        'EQ': lispfunc.eq,
        'EQUAL': lispfunc.equal,

        # Mathematical and float functions are now registered via decorators in lispfunc/math.py

        # Sequence operations (most moved to registry in sequences.py)
        'BUTLAST': lispfunc.butlast,

        # Array operations
        'ARRAY-DIMENSION': lispfunc.array_dimension,
        'ADJUST-ARRAY': lispfunc.adjust_array,

        # Hash table operations
        'HASH-TABLE-COUNT': lispfunc.hash_table_count,
        'HASH-TABLE-SIZE': lispfunc.hash_table_size,
        'HASH-TABLE-TEST': lispfunc.hash_table_test,
        'HASH-TABLE-REHASH-SIZE': lispfunc.hash_table_rehash_size,
        'HASH-TABLE-REHASH-THRESHOLD': lispfunc.hash_table_rehash_threshold,

        # String/Character operations
        'MERGE-PATHNAMES': lispfunc.merge_pathnames,
        'HOST-NAMESTRING': lispfunc.host_namestring,
        'FILE-NAMESTRING': lispfunc.file_namestring,

        # Property lists
        'GET': lispfunc.get,
        'RPLACA': lispfunc.rplaca,
        'RPLACD': lispfunc.rplacd,

        # Package and symbol operations
        'INTERN': lispfunc.intern,
        'FIND-SYMBOL': lispfunc.find_symbol,
        'FIND-PACKAGE': lispfunc.find_package,
        'FIND-ALL-SYMBOLS': lispfunc.find_all_symbols,
        'EXPORT': lispfunc.export_symbol,
        'IMPORT': lispfunc.import_symbol,
        'IN-PACKAGE': lispfunc.in_package,
        'GENTEMP': lispfunc.gentemp,
        'APROPOS': lispfunc.apropos,
        'APROPOS-LIST': lispfunc.apropos_list,

        # Function operations
        'FDEFINITION': lispfunc.fdefinition,
        'FMAKUNBOUND': lispfunc.fmakunbound,
        'FUNCTION-KEYWORDS': lispfunc.function_keywords,
        'FUNCTION-LAMBDA-EXPRESSION': lispfunc.function_lambda_expression,

        # File operations
        'COMPILE-FILE-PATHNAME': lispfunc.compile_file_pathname,
        'DELETE-FILE': lispfunc.delete_file,
        'FILE-AUTHOR': lispfunc.file_author,
        'FILE-STRING-LENGTH': lispfunc.file_string_length,
        'FILE-WRITE-DATE': lispfunc.file_write_date,
        'PROBE-FILE': lispfunc.probe_file,
        'RENAME-FILE': lispfunc.rename_file,

        # Stream and format operations are registered via decorators in io.py

        # Time operations
        'GET-DECODED-TIME': lispfunc.get_decoded_time,
        'GET-INTERNAL-REAL-TIME': lispfunc.get_internal_real_time,
        'GET-INTERNAL-RUN-TIME': lispfunc.get_internal_run_time,
        'INTERNAL-TIME-UNITS-PER-SECOND': lispfunc.internal_time_units_per_second,

        # Macro/readtable operations are registered via decorators in io.py

        # CLOS operations
        'FIND-CLASS': lispfunc.find_class,
        'FIND-METHOD': lispfunc.find_method,
        'ADD-METHOD': lispfunc.add_method,
        'ALLOCATE-INSTANCE': lispfunc.allocate_instance,
        'INITIALIZE-INSTANCE': lispfunc.initialize_instance,
        'UPDATE-INSTANCE-FOR-DIFFERENT-CLASS': lispfunc.update_instance_for_different_class,
        'UPDATE-INSTANCE-FOR-REDEFINED-CLASS': lispfunc.update_instance_for_redefined_class,
        'SLOT-MISSING': lispfunc.slot_missing,
        'INVALID-METHOD-ERROR': lispfunc.invalid_method_error,

        # Conditions and restarts
        'INVOKE-DEBUGGER': lispfunc.invoke_debugger,
        'INVOKE-RESTART': lispfunc.invoke_restart,
        'INVOKE-RESTART-INTERACTIVELY': lispfunc.invoke_restart_interactively,
        'ABORT': lispfunc.abort,

        # Constants and type operations (non-math remain here)
        'LAMBDA-LIST-KEYWORDS': lispfunc.lambda_list_keywords,
        'LAMBDA-PARAMETERS-LIMIT': lispfunc.lambda_parameters_limit,

        # Utilities
        'IDENTITY': lispfunc.identity,
        'COPY-TREE': lispfunc.copy_tree,
        'INSPECT': lispfunc.inspect,
        'INCF': lispfunc.incf,

        # Load form operations
        'MAKE-LOAD-FORM': lispfunc.make_load_form,
        'MAKE-LOAD-FORM-SAVING-SLOTS': lispfunc.make_load_form_saving_slots,

        # Pretty printing
        'PPRINT-FILL': lispfunc.pprint_fill,

        # Special symbols and constants
        # LIST* registered via decorator in sequences

        # Types and constants
        'KEYWORD': lispfunc.keyword,
        'INTEGER': lispfunc.integer,
        'FIXNUM': lispfunc.fixnum,
        'DOUBLE-FLOAT': lispfunc.double_float,
        'SINGLE-FLOAT': lispfunc.single_float,
        'SHORT-FLOAT': lispfunc.short_float,
        'EXTENDED-CHAR': lispfunc.extended_char,
        'HASH-TABLE': lispfunc.hash_table,
        'GENERIC-FUNCTION': lispfunc.generic_function,
        'FILE-STREAM': lispfunc.file_stream,
        'FILE-ERROR': lispfunc.file_error,
        'END-OF-FILE': lispfunc.end_of_file,
        'FLOATING-POINT-INEXACT': lispfunc.floating_point_inexact,
        'FLOATING-POINT-INVALID-OPERATION': lispfunc.floating_point_invalid_operation,
        'FLOATING-POINT-OVERFLOW': lispfunc.floating_point_overflow,
        'FLOATING-POINT-UNDERFLOW': lispfunc.floating_point_underflow,
        'ARITHMETIC-ERROR-OPERANDS': lispfunc.arithmetic_error_operands,
        'ARITHMETIC-ERROR-OPERATION': lispfunc.arithmetic_error_operation,
        'FILE-ERROR-PATHNAME': lispfunc.file_error_pathname,

        # I/O and utilities
        'PRINT': lispfunc._s_print_,
        'READ': lispfunc.read,
        'ERROR': lispfunc.error,
        'GENSYM': lispfunc.gensym,

        # Special forms are handled by eval, but we can include them for completeness
        'EVAL': lispfunc.eval,
        'APPLY': lispfunc.apply,

        # Special forms (these are handled specially in eval)
        'IF': lambda *args: "SPECIAL_FORM_IF",
        'COND': lambda *args: "SPECIAL_FORM_COND",
        'DEFUN': lambda *args: "SPECIAL_FORM_DEFUN",
        'SETQ': lambda *args: "SPECIAL_FORM_SETQ",
        'DEFVAR': lambda *args: "SPECIAL_FORM_DEFVAR",
        'LET': lambda *args: "SPECIAL_FORM_LET",
        'WHEN': lambda *args: "SPECIAL_FORM_WHEN",
        'FLET': lispfunc.flet,
        'LABELS': lispfunc.labels,
        'HANDLER-BIND': lispfunc.handler_bind,
        'HANDLER-CASE': lispfunc.handler_case,
        'WITH-OPEN-FILE': lispfunc.with_open_file,
        'LOOP-FINISH': lispfunc.loop_finish,
        'INLINE': lispfunc.inline_decl,
        'IGNORE': lispfunc.ignore,
        'IGNORABLE': lispfunc.ignorable,

        # Essential ANSI Lisp functions
        # Math functions moved to registry via decorators
        'ENDP': lispfunc.endp,
        'CHAR': lispfunc.char,
        'CHAR-CODE': lispfunc.char_code,
        'CODE-CHAR': lispfunc.code_char,
        'CHARACTER': lispfunc.character,
        'CHAR-UPCASE': lispfunc.char_upcase,
        'CHAR-DOWNCASE': lispfunc.char_downcase,
        'UPPER-CASE-P': lispfunc.upper_case_p,
        'LOWER-CASE-P': lispfunc.lower_case_p,
        'DIGIT-CHAR-P': lispfunc.digit_char_p,
        'STANDARD-CHAR-P': lispfunc.standard_char_p,
        'STRING': lispfunc.string,
        'STRING-UPCASE': lispfunc.string_upcase,
        'STRING-DOWNCASE': lispfunc.string_downcase,
        'STRING-CAPITALIZE': lispfunc.string_capitalize,
        'UNLESS': lispfunc.unless,
        'PROG1': lispfunc.prog1,
        'PROG2': lispfunc.prog2,
        'TYPEP': lispfunc.typep,
        'TYPE-OF': lispfunc.type_of,
        'SUBTYPEP': lispfunc.subtypep,
        'CHARACTERP': lispfunc.characterp,
        'PACKAGEP': lispfunc.packagep,
        'PATHNAMEP': lispfunc.pathnamep,
        'READTABLEP': lispfunc.readtablep,
        'STREAMP': lispfunc.streamp,
        'PRINC': lispfunc.princ,
        'PRIN1': lispfunc.prin1,
        'TERPRI': lispfunc.terpri,
        'SET': lispfunc.set,
        'BOUNDP': lispfunc.boundp,
        'MAKUNBOUND': lispfunc.makunbound,
        'FUNCALL': lispfunc.funcall,
        'CONSTANTLY': lispfunc.constantly,
        'COMPLEMENT': lispfunc.complement,
        'VALUES': lispfunc.values,
        'VALUES-LIST': lispfunc.values_list,
        'RANDOM': lispfunc.random,
        'MAKE-RANDOM-STATE': lispfunc.make_random_state,
        'BLOCK': lispfunc.block,
        'RETURN-FROM': lispfunc.return_from,
        'CATCH': lispfunc.catch,
        'THROW': lispfunc.throw,
        'TAGBODY': lispfunc.tagbody,
        'GO': lispfunc.go,
        'UNWIND-PROTECT': lispfunc.unwind_protect,

        # Additional essential ANSI Lisp functions
        'AND': lispfunc.and_fn,
        'OR': lispfunc.or_fn,
        'PROG': lispfunc.prog,
        'PROGN': lispfunc.progn,
        'WHEN': lispfunc.when_fn,
        'UNLESS': lispfunc.unless_fn,
        'CASE': lispfunc.case_fn,
        'COND': lispfunc.cond_fn,

        # Array operations
        'ARRAY-ELEMENT-TYPE': lispfunc.array_element_type,
        'ARRAY-RANK': lispfunc.array_rank,
        'ARRAY-TOTAL-SIZE': lispfunc.array_total_size,
        'ARRAY-IN-BOUNDS-P': lispfunc.array_in_bounds_p,
        'ARRAY-DISPLACEMENT': lispfunc.array_displacement,

        # Sequence operations
        # Registered via decorators in sequences module

        # Bit operations
        # Registered via decorators in math/sequences modules

        # Character comparison functions
        'CHAR=': lispfunc.char_equal,
        'CHAR/=': lispfunc.char_not_equal,
        'CHAR<': lispfunc.char_less,
        'CHAR>': lispfunc.char_greater,
        'CHAR<=': lispfunc.char_less_equal,
        'CHAR>=': lispfunc.char_greater_equal,
        'CHAR-EQUAL': lispfunc.char_equal_ignore_case,
        'CHAR-NOT-EQUAL': lispfunc.char_not_equal_ignore_case,
        'CHAR-LESSP': lispfunc.char_lessp,
        'CHAR-GREATERP': lispfunc.char_greaterp,
        'CHAR-NOT-GREATERP': lispfunc.char_not_greaterp,
        'CHAR-NOT-LESSP': lispfunc.char_not_lessp,
        'BOTH-CASE-P': lispfunc.both_case_p,
        'CHAR-INT': lispfunc.char_int,
        'INT-CHAR': lispfunc.int_char,
        'CHAR-NAME': lispfunc.char_name,
        'NAME-CHAR': lispfunc.name_char,
        'DIGIT-CHAR': lispfunc.digit_char,

        # String operations
        'SCHAR': lispfunc.schar,
        'STRING=': lispfunc.string_equal_fn,
        'STRING/=': lispfunc.string_not_equal,
        'STRING<': lispfunc.string_less,
        'STRING>': lispfunc.string_greater,
        'STRING<=': lispfunc.string_less_equal,
        'STRING>=': lispfunc.string_greater_equal,
        'STRING-EQUAL': lispfunc.string_equal_ignore_case,
        'STRING-NOT-EQUAL': lispfunc.string_not_equal_ignore_case,
        'STRING-LESSP': lispfunc.string_lessp,
        'STRING-GREATERP': lispfunc.string_greaterp,
        'STRING-NOT-GREATERP': lispfunc.string_not_greaterp,
        'STRING-NOT-LESSP': lispfunc.string_not_lessp,
        'NSTRING-UPCASE': lispfunc.nstring_upcase,
        'NSTRING-DOWNCASE': lispfunc.nstring_downcase,
        'NSTRING-CAPITALIZE': lispfunc.nstring_capitalize,
        'STRING-TRIM': lispfunc.string_trim,
        'STRING-LEFT-TRIM': lispfunc.string_left_trim,
        'STRING-RIGHT-TRIM': lispfunc.string_right_trim,
        'PARSE-INTEGER': lispfunc.parse_integer,

        # Hash table operations
        'MAKE-HASH-TABLE': lispfunc.make_hash_table,
        'GETHASH': lispfunc.gethash,
        'REMHASH': lispfunc.remhash,
        'MAPHASH': lispfunc.maphash,
        'CLRHASH': lispfunc.clrhash,
        'SXHASH': lispfunc.sxhash,

        # Property list operations
        'GETF': lispfunc.getf,
        'GET-PROPERTIES': lispfunc.get_properties,
        'PUTPROP': lispfunc.putprop,
        'REMPROP': lispfunc.remprop,
        'SYMBOL-PLIST': lispfunc.symbol_plist,
        'REMF': lispfunc.remf,

        # More list operations
        # Registered via decorators in sequences module

        # Set operations (destructive versions)
        # Registered via decorators in sequences module

        # Tree operations (destructive versions)
        # Registered via decorators in sequences module

        # I/O, pathname, and stream operations are registered via decorators in io.py

        # Stream operations
        'OPEN': lispfunc.open_fn,
        'CLOSE': lispfunc.close_fn,
        'OUTPUT-STREAM-P': lispfunc.output_stream_p,
        'OPEN-STREAM-P': lispfunc.open_stream_p,
        'STREAM-ELEMENT-TYPE': lispfunc.stream_element_type,
        'MAKE-STRING-INPUT-STREAM': lispfunc.make_string_input_stream,
        'MAKE-STRING-OUTPUT-STREAM': lispfunc.make_string_output_stream,
        'MAKE-BROADCAST-STREAM': lispfunc.make_broadcast_stream,
        'MAKE-CONCATENATED-STREAM': lispfunc.make_concatenated_stream,
        'MAKE-ECHO-STREAM': lispfunc.make_echo_stream,
        'MAKE-SYNONYM-STREAM': lispfunc.make_synonym_stream,
        'MAKE-TWO-WAY-STREAM': lispfunc.make_two_way_stream,

        # Time operations
        'GET-UNIVERSAL-TIME': lispfunc.get_universal_time,
        'DECODE-UNIVERSAL-TIME': lispfunc.decode_universal_time,
        'ENCODE-UNIVERSAL-TIME': lispfunc.encode_universal_time,
        'SLEEP': lispfunc.sleep_fn,

        # Multiple values
        'MULTIPLE-VALUE-BIND': lispfunc.multiple_value_bind,
        'MULTIPLE-VALUE-CALL': lispfunc.multiple_value_call,
        'MULTIPLE-VALUE-LIST': lispfunc.multiple_value_list,
        'MULTIPLE-VALUE-PROG1': lispfunc.multiple_value_prog1,
        'MULTIPLE-VALUE-SETQ': lispfunc.multiple_value_setq,
        'NTH-VALUE': lispfunc.nth_value,

        # Symbol operations
        'SYMBOL-NAME': lispfunc.symbol_name,
        'SYMBOL-PACKAGE': lispfunc.symbol_package,
        'SYMBOL-VALUE': lispfunc.symbol_value,
        'SYMBOL-FUNCTION': lispfunc.symbol_function,
        'MAKE-SYMBOL': lispfunc.make_symbol,
        'COPY-SYMBOL': lispfunc.copy_symbol,

        # Package operations
        'MAKE-PACKAGE': lispfunc.make_package,
        'PACKAGE-NAME': lispfunc.package_name,
        'PACKAGE-NICKNAMES': lispfunc.package_nicknames,
        'RENAME-PACKAGE': lispfunc.rename_package,
        'PACKAGE-USE-LIST': lispfunc.package_use_list,
        'PACKAGE-USED-BY-LIST': lispfunc.package_used_by_list,
        'PACKAGE-SHADOWING-SYMBOLS': lispfunc.package_shadowing_symbols,
        'LIST-ALL-PACKAGES': lispfunc.list_all_packages,
        'UNINTERN': lispfunc.unintern,
        'UNEXPORT': lispfunc.unexport,
        'SHADOWING-IMPORT': lispfunc.shadowing_import,
        'SHADOW': lispfunc.shadow,
        'USE-PACKAGE': lispfunc.use_package,
        'UNUSE-PACKAGE': lispfunc.unuse_package,

        # Macro operations
        'MACROEXPAND': lispfunc.macroexpand,
        'MACROEXPAND-1': lispfunc.macroexpand_1,

        # Constants and limits
        'ARRAY-DIMENSION-LIMIT': lispfunc.array_dimension_limit,
        'ARRAY-RANK-LIMIT': lispfunc.array_rank_limit,
        'ARRAY-TOTAL-SIZE-LIMIT': lispfunc.array_total_size_limit,
        'CALL-ARGUMENTS-LIMIT': lispfunc.call_arguments_limit,
        'MULTIPLE-VALUES-LIMIT': lispfunc.multiple_values_limit,
        'CHAR-CODE-LIMIT': lispfunc.char_code_limit,

        # Essential predicates
        'CONSTANTP': lispfunc.constantp,
        'SPECIAL-OPERATOR-P': lispfunc.special_operator_p,
        'MACRO-FUNCTION': lispfunc.macro_function,

        # Critical missing functions for 75% coverage
        # Control flow
        'DO': lispfunc.do_fn,
        'DOLIST': lispfunc.dolist,
        'DOTIMES': lispfunc.dotimes,
        'LOOP': lispfunc.loop_fn,
        'EVAL-WHEN': lispfunc.eval_when,
        'LOAD': lispfunc.load_fn,

        # Essential predicates and comparisons
        'EQUALP': lispfunc.equalp,
        'NOT': lispfunc.not_fn,
        'EQL': lispfunc.eql,
        'EQUAL': lispfunc.equal_fn,

        # Symbol and package operations
        'DO-SYMBOLS': lispfunc.do_symbols,
        'DO-EXTERNAL-SYMBOLS': lispfunc.do_external_symbols,
        'DO-ALL-SYMBOLS': lispfunc.do_all_symbols,
        'DOCUMENTATION': lispfunc.documentation,

        # Mathematical constants now registered via decorators in lispfunc/math.py

        # Essential sequence operations
        'NOTEVERY': lispfunc.notevery,
        'NOTANY': lispfunc.notany,
        'MAPL': lispfunc.mapl,
        'MAPLIST': lispfunc.maplist,
        'APPLY': lispfunc.apply_fn,

        # File and stream operations
        'FILE-POSITION': lispfunc.file_position,
        'FILE-LENGTH': lispfunc.file_length,
        'FRESH-LINE': lispfunc.fresh_line,
        'FINISH-OUTPUT': lispfunc.finish_output,
        'FORCE-OUTPUT': lispfunc.force_output,

        # More string operations
        'STRING': lispfunc.string_fn,
        'CHAR': lispfunc.char_fn,

        # More array operations
        # Registered via decorators in sequences module
        'FILL-POINTER': lispfunc.fill_pointer,

        # More type predicates
        'SIMPLE-STRING-P': lispfunc.simple_string_p,
        'RANDOM-STATE-P': lispfunc.random_state_p,

        # Essential arithmetic operations
        # Registered via decorators in math module

        # Logical operations on integers
        # Registered via decorators in math module

        # More list operations
        # Registered via decorators in sequences module

        # More control constructs
        'TYPECASE': lispfunc.typecase,
        'ETYPECASE': lispfunc.etypecase,
        'CTYPECASE': lispfunc.ctypecase,

        # Compiler and evaluation
        'COMPILE': lispfunc.compile_fn,
        'COMPILE-FILE': lispfunc.compile_file,
        'EVAL': lispfunc.eval_fn,

        # Additional critical ANSI functions
        # Format and pretty printing
        'COPY-PPRINT-DISPATCH': lispfunc.copy_pprint_dispatch,
        'PPRINT': lispfunc.pprint,
        'PPRINT-DISPATCH': lispfunc.pprint_dispatch,
        'PPRINT-EXIT-IF-LIST-EXHAUSTED': lispfunc.pprint_exit_if_list_exhausted,
        'PPRINT-INDENT': lispfunc.pprint_indent,
        'PPRINT-LINEAR': lispfunc.pprint_linear,
        'PPRINT-LOGICAL-BLOCK': lispfunc.pprint_logical_block,
        'PPRINT-NEWLINE': lispfunc.pprint_newline,
        'PPRINT-POP': lispfunc.pprint_pop,
        'PPRINT-TAB': lispfunc.pprint_tab,

        # Generalized assignment
        'DECF': lispfunc.decf,
        'PSETF': lispfunc.psetf,
        'SETF': lispfunc.setf,
        'SHIFTF': lispfunc.shiftf,
        'ROTATEF': lispfunc.rotatef,

        # Package system
        'PACKAGE-ERROR-PACKAGE': lispfunc.package_error_package,
        'WITH-PACKAGE-ITERATOR': lispfunc.with_package_iterator,
        'EXPORT': lispfunc.export_fn,
        'IMPORT': lispfunc.import_fn,

        # Essential missing functions
        'ARRAY-ROW-MAJOR-INDEX': lispfunc.array_row_major_index,
        'BREAK': lispfunc.break_fn,
        'CCASE': lispfunc.ccase,
        'CHAR-BITS-LIMIT': lispfunc.char_bits_limit,
        'CHAR-FONT-LIMIT': lispfunc.char_font_limit,
        'COMPLEX': lispfunc.complex_fn,
        'COMPUTE-RESTARTS': lispfunc.compute_restarts,
        'CONTINUE': lispfunc.continue_fn,
        'DECLAIM': lispfunc.declaim,
        'DECLARE': lispfunc.declare,
        'DEFCLASS': lispfunc.defclass,
        'DEFCONSTANT': lispfunc.defconstant,
        'DEFGENERIC': lispfunc.defgeneric,
        'DEFINE-CONDITION': lispfunc.define_condition,
        'DEFINE-METHOD-COMBINATION': lispfunc.define_method_combination,
        'DEFINE-MODIFY-MACRO': lispfunc.define_modify_macro,
        'DEFINE-SETF-EXPANDER': lispfunc.define_setf_expander,
        'DEFMETHOD': lispfunc.defmethod,
        'DEFPACKAGE': lispfunc.defpackage,
        'DEFSETF': lispfunc.defsetf,
        'DEFSTRUCT': lispfunc.defstruct,
        'DEFTYPE': lispfunc.deftype,
        'DESTRUCTURING-BIND': lispfunc.destructuring_bind,
        'DIRECTORY': lispfunc.directory,
        'ECHO-STREAM-INPUT-STREAM': lispfunc.echo_stream_input_stream,
        'ECHO-STREAM-OUTPUT-STREAM': lispfunc.echo_stream_output_stream,
        'ECHO-STREAM-P': lispfunc.echo_stream_p,
        'ECASE': lispfunc.ecase,
        'ERROR': lispfunc.error_fn,
        'FIND-RESTART': lispfunc.find_restart,
        'GET-SETF-EXPANSION': lispfunc.get_setf_expansion,
        'IGNORE-ERRORS': lispfunc.ignore_errors,
        'LAMBDA': lispfunc.lambda_fn,
        'LOAD-TIME-VALUE': lispfunc.load_time_value,
        'LOCALLY': lispfunc.locally,
        'MAKE-CONDITION': lispfunc.make_condition,
        'MUFFLE-WARNING': lispfunc.muffle_warning,
        'PROCLAIM': lispfunc.proclaim,
        'PROVIDE': lispfunc.provide,
        'REQUIRE': lispfunc.require,
        'RESTART-BIND': lispfunc.restart_bind,
        'RESTART-CASE': lispfunc.restart_case,
        'RESTART-NAME': lispfunc.restart_name,
        'SIGNAL': lispfunc.signal_fn,
        'STORE-VALUE': lispfunc.store_value,
        'THE': lispfunc.the,
        'TIME': lispfunc.time_fn,
        'TRACE': lispfunc.trace_fn,
        'UNTRACE': lispfunc.untrace,
        'USE-VALUE': lispfunc.use_value,
        'WARN': lispfunc.warn_fn,
        'WITH-CONDITION-RESTARTS': lispfunc.with_condition_restarts,
        'WITH-SIMPLE-RESTART': lispfunc.with_simple_restart,

        # Additional ANSI Common Lisp functions
        'LISP-IMPLEMENTATION-TYPE': lispfunc.lisp_implementation_type,
        'LISP-IMPLEMENTATION-VERSION': lispfunc.lisp_implementation_version,
        'DESCRIBE': lispfunc.describe,
        'ED': lispfunc.ed,
        'DRIBBLE': lispfunc.dribble,
        'DISASSEMBLE': lispfunc.disassemble,
        'CERROR': lispfunc.cerror,
        'DEFMACRO': lispfunc.defmacro_fn,
        'DEFPARAMETER': lispfunc.defparameter,
        'CLASS-OF': lispfunc.class_of,
        'CLASS-NAME': lispfunc.class_name,
        'CHANGE-CLASS': lispfunc.change_class,
        'BUILT-IN-CLASS': lispfunc.built_in_class,
        'CALL-METHOD': lispfunc.call_method,
        'CALL-NEXT-METHOD': lispfunc.call_next_method,
        'COMPUTE-APPLICABLE-METHODS': lispfunc.compute_applicable_methods,
        'ENSURE-GENERIC-FUNCTION': lispfunc.ensure_generic_function,
        'GENERIC-FUNCTION-LAMBDA-LIST': lispfunc.generic_function_lambda_list,
        'GENERIC-FUNCTION-METHODS': lispfunc.generic_function_methods,
        'GENERIC-FUNCTION-NAME': lispfunc.generic_function_name,
        'COMPILER-MACRO-FUNCTION': lispfunc.compiler_macro_function,
        'DEFINE-COMPILER-MACRO': lispfunc.define_compiler_macro,
        'DYNAMIC-EXTENT': lispfunc.dynamic_extent,
        'FTYPE': lispfunc.ftype,
        'FUNCTION': lispfunc.function_fn,
        'BROADCAST-STREAM-P': lispfunc.broadcast_stream_p,
        'BROADCAST-STREAM-STREAMS': lispfunc.broadcast_stream_streams,
        'CONCATENATED-STREAM-P': lispfunc.concatenated_stream_p,
        'CONCATENATED-STREAM-STREAMS': lispfunc.concatenated_stream_streams,
        'FILE-STREAM-P': lispfunc.file_stream_p,
        # COPY-READTABLE, readtable, and pprint helpers registered via decorators in io.py
        'ENSURE-DIRECTORIES-EXIST': lispfunc.ensure_directories_exist,
        # Float rounding functions registered via decorators in math module

        # Complete ANSI Common Lisp function coverage
        'MAKE-INSTANCE': lispfunc.make_instance,
        'MAKE-METHOD': lispfunc.make_method,
        'METHOD-COMBINATION-ERROR': lispfunc.method_combination_error,
        'METHOD-FUNCTION': lispfunc.method_function,
        'METHOD-GENERIC-FUNCTION': lispfunc.method_generic_function,
        'METHOD-SPECIALIZERS': lispfunc.method_specializers,
        'NEXT-METHOD-P': lispfunc.next_method_p,
        'NO-APPLICABLE-METHOD': lispfunc.no_applicable_method,
        'NO-NEXT-METHOD': lispfunc.no_next_method,
        'REINITIALIZE-INSTANCE': lispfunc.reinitialize_instance,
        'REMOVE-METHOD': lispfunc.remove_method,
        'SHARED-INITIALIZE': lispfunc.shared_initialize,
        'SLOT-BOUNDP': lispfunc.slot_boundp,
        'SLOT-EXISTS-P': lispfunc.slot_exists_p,
        'SLOT-MAKUNBOUND': lispfunc.slot_makunbound,
        'SLOT-UNBOUND': lispfunc.slot_unbound,
        'SLOT-VALUE': lispfunc.slot_value,
        'STANDARD-CLASS': lispfunc.standard_class,
        'STANDARD-OBJECT': lispfunc.standard_object,
        'STRUCTURE-CLASS': lispfunc.structure_class,
        'STRUCTURE-OBJECT': lispfunc.structure_object,
        'PPRINT-TABULAR': lispfunc.pprint_tabular,
        'READ-CHAR-NO-HANG': lispfunc.read_char_no_hang,
        'READ-DELIMITED-LIST': lispfunc.read_delimited_list,
        'READ-FROM-STRING': lispfunc.read_from_string,
        'READ-PRESERVING-WHITESPACE': lispfunc.read_preserving_whitespace,
        'STRING-STREAM-P': lispfunc.string_stream_p,
        'SYNONYM-STREAM-P': lispfunc.synonym_stream_p,
        'SYNONYM-STREAM-SYMBOL': lispfunc.synonym_stream_symbol,
        'TWO-WAY-STREAM-INPUT-STREAM': lispfunc.two_way_stream_input_stream,
        'TWO-WAY-STREAM-OUTPUT-STREAM': lispfunc.two_way_stream_output_stream,
        'TWO-WAY-STREAM-P': lispfunc.two_way_stream_p,
        # Float constants registered via decorators in math module
        # Readtable/macro character and pprint dispatch functions are registered via decorators in io.py
        'PROGV': lispfunc.progv,
        'PSETQ': lispfunc.psetq,
        'QUOTE': lispfunc.quote_fn,
        'RATIONAL': lispfunc.rational,
        'RATIONALIZE': lispfunc.rationalize,
        'MAP-INTO': lispfunc.map_into,
        'MAPCON': lispfunc.mapcon,
        'ROW-MAJOR-AREF': lispfunc.row_major_aref,
        'SOFTWARE-TYPE': lispfunc.software_type,
        'SOFTWARE-VERSION': lispfunc.software_version,
        'ROOM': lispfunc.room,
        'STEP': lispfunc.step,
        'USER-HOMEDIR-PATHNAME': lispfunc.user_homedir_pathname,
        'OCTETS-TO-STRING': lispfunc.octets_to_string,
        'STRING-TO-OCTETS': lispfunc.string_to_octets,
        'TYPE': lispfunc.type,
        'UPGRADED-ARRAY-ELEMENT-TYPE': lispfunc.upgraded_array_element_type,
        'UPGRADED-COMPLEX-PART-TYPE': lispfunc.upgraded_complex_part_type,
        'OPTIMIZE': lispfunc.optimize,
        'SPECIAL': lispfunc.special,
        'NIL': lispfunc.nil_symbol,
        'T': lispfunc.t_symbol,
        'WITH-ACCESSORS': lispfunc.with_accessors,
        'WITH-COMPILATION-UNIT': lispfunc.with_compilation_unit,
        'WITH-INPUT-FROM-STRING': lispfunc.with_input_from_string,
        'WITH-OPEN-STREAM': lispfunc.with_open_stream,
        'WITH-OUTPUT-TO-STRING': lispfunc.with_output_to_string,
        'WITH-PPRINT-LOGICAL-BLOCK': lispfunc.with_pprint_logical_block,
        'WITH-SLOTS': lispfunc.with_slots,
        'WITH-STANDARD-IO-SYNTAX': lispfunc.with_standard_io_syntax,

        # Environment and implementation functions
        'MACHINE-TYPE': lispfunc.machine_type,
        'MACHINE-INSTANCE': lispfunc.machine_instance,
        'MACHINE-VERSION': lispfunc.machine_version,
        'LONG-SITE-NAME': lispfunc.long_site_name,
        'SHORT-SITE-NAME': lispfunc.short_site_name,
        'LOAD-LOGICAL-PATHNAME-TRANSLATIONS': lispfunc.load_logical_pathname_translations,
        'LOGICAL-PATHNAME-TRANSLATIONS': lispfunc.logical_pathname_translations,

        # Floating point constants (keep normalized variants not in decorators)
        'LEAST-NEGATIVE-LONG-FLOAT': lispfunc.least_negative_long_float,
        'LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT': lispfunc.least_negative_normalized_double_float,
        'LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT': lispfunc.least_negative_normalized_long_float,
        'LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT': lispfunc.least_negative_normalized_short_float,
        'LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT': lispfunc.least_negative_normalized_single_float,
        'LEAST-NEGATIVE-SHORT-FLOAT': lispfunc.least_negative_short_float,
        'LEAST-NEGATIVE-SINGLE-FLOAT': lispfunc.least_negative_single_float,
        'LEAST-POSITIVE-LONG-FLOAT': lispfunc.least_positive_long_float,
        'LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT': lispfunc.least_positive_normalized_double_float,
        'LEAST-POSITIVE-NORMALIZED-LONG-FLOAT': lispfunc.least_positive_normalized_long_float,
        'LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT': lispfunc.least_positive_normalized_short_float,
        'LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT': lispfunc.least_positive_normalized_single_float,
        'LEAST-POSITIVE-SHORT-FLOAT': lispfunc.least_positive_short_float,
        'LEAST-POSITIVE-SINGLE-FLOAT': lispfunc.least_positive_single_float,
        'LONG-FLOAT-EPSILON': lispfunc.long_float_epsilon,
        'LONG-FLOAT-NEGATIVE-EPSILON': lispfunc.long_float_negative_epsilon,

        # Additional CLOS and utility functions
        'METHOD-LAMBDA-LIST': lispfunc.method_lambda_list,
        'METHOD-QUALIFIERS': lispfunc.method_qualifiers,
        'MACROLET': lispfunc.macrolet,
        'SYMBOL-MACROLET': lispfunc.symbol_macrolet,
        'NOTINLINE': lispfunc.notinline,
        'INLINE': lispfunc.inline,
    }

    # Add all the mapped functions to the environment
    for lisp_name, python_func in function_mappings.items():
        symbol = fclpy.lisptype.LispSymbol(lisp_name)
        state.current_environment.add_function(symbol, python_func)

    state.functions_loaded = True
    return state.current_environment
