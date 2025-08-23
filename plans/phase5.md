# Phase 5 – Data Structures and I/O

**CRITICAL**: Run `pipenv run pytest -q` after every task.

## Step-By-Step Tasks (Do These In Order)

### Task 1: Create unified sequence system
- [ ] **Goal**: Common interface for working with lists, vectors, strings
	- [ ] **Step 1.1**: Define sequence interface with iterate(seq, start, end) function
	- [ ] **Step 1.2**: Add key/test handling wrapper for comparisons
	- [ ] **Step 1.3**: Support lists, vectors, and strings with this interface
	- [ ] **Step 1.4**: Make it extensible for new sequence types
	- [ ] **Step 1.5**: Write tests: slicing with :start and :end parameters
	- [ ] **Step 1.6**: Run `pipenv run pytest -q`

### Task 2: Rewrite sequence functions to use common protocol
- [ ] **Goal**: Make FIND, POSITION, REMOVE, MAP, REDUCE work consistently
	- [ ] **Step 2.1**: Write tests for current behavior of these functions BEFORE changing them
	- [ ] **Step 2.2**: Replace implementations to use new sequence protocol
	- [ ] **Step 2.3**: Make sure keyword arguments like :key and :test still work
	- [ ] **Step 2.4**: Run regression tests to ensure nothing broke
	- [ ] **Step 2.5**: Run `pipenv run pytest -q`

### Task 3: Add adjustable vectors
- [ ] **Goal**: Vectors that can grow and shrink
	- [ ] **Step 3.1**: Create vector structure that supports resizing
	- [ ] **Step 3.2**: Add fill-pointer support (tracks actual length vs capacity)
	- [ ] **Step 3.3**: Implement VECTOR-PUSH (add element if room)
	- [ ] **Step 3.4**: Implement VECTOR-PUSH-EXTEND (add element, growing if needed)  
	- [ ] **Step 3.5**: Write tests: push operations, boundary conditions
	- [ ] **Step 3.6**: Run `pipenv run pytest -q`

### Task 4: Add basic multi-dimensional arrays
- [ ] **Goal**: Support arrays with more than one dimension (optional/basic)
	- [ ] **Step 4.1**: Design minimal shape storage for array dimensions
	- [ ] **Step 4.2**: Implement row-major index calculation for multi-dim access
	- [ ] **Step 4.3**: Implement subset of MAKE-ARRAY function  
	- [ ] **Step 4.4**: Implement AREF function for accessing array elements
	- [ ] **Step 4.5**: Write basic tests for 2D arrays
	- [ ] **Step 4.6**: Document advanced features not implemented (displacement, etc.)
	- [ ] **Step 4.7**: Run `pipenv run pytest -q`

### Task 5: Add file and stream I/O
- [ ] **Goal**: Basic file reading and writing
	- [ ] **Step 5.1**: Create abstract stream class
	- [ ] **Step 5.2**: Create subclasses: file-stream and string-stream
	- [ ] **Step 5.3**: Implement OPEN function returning file-stream
	- [ ] **Step 5.4**: Implement READ-CHAR and WRITE-CHAR functions
	- [ ] **Step 5.5**: Implement CLOSE function
	- [ ] **Step 5.6**: Write tests using temporary files and string stream round-trips
	- [ ] **Step 5.7**: Run `pipenv run pytest -q`

### Task 6: Add pathname handling
- [ ] **Goal**: Handle file paths in a structured way
	- [ ] **Step 6.1**: Create pathname parser that splits path into components
	- [ ] **Step 6.2**: Handle: host, device, directory, name, type, version (simplify unsupported parts)
	- [ ] **Step 6.3**: Add accessor functions for each component
	- [ ] **Step 6.4**: Write tests: construct from path string, convert back to string
	- [ ] **Step 6.5**: Test round-trip where no information is lost
	- [ ] **Step 6.6**: Run `pipenv run pytest -q`

### Task 7: Add hash tables
- [ ] **Goal**: Dictionary-like data structure
	- [ ] **Step 7.1**: Implement MAKE-HASH-TABLE with :TEST option (EQ, EQL, EQUAL)
	- [ ] **Step 7.2**: Store Python dict plus test function
	- [ ] **Step 7.3**: Implement GETHASH (should return two values: value and found-flag)
	- [ ] **Step 7.4**: Implement REMHASH, CLRHASH, MAPHASH, HASH-TABLE-COUNT
	- [ ] **Step 7.5**: Add printer support showing `#<HASH-TABLE n>` format
	- [ ] **Step 7.6**: Write tests: insertion, removal, iteration, counting
	- [ ] **Step 7.7**: Document what's not implemented (rehash-size, weak tables, etc.)
	- [ ] **Step 7.8**: Run `pipenv run pytest -q`

## Important Notes
- Build sequence protocol first, then rewrite functions to use it
- Streams and pathnames can be worked on in parallel
- Multi-dimensional arrays are optional - basic support is fine

## How to Know Phase 5 is Done  
✅ All checkboxes above are checked
✅ All tests pass when you run `pipenv run pytest -q`
✅ Sequence functions work consistently across lists/vectors/strings
✅ Basic file I/O works
✅ Hash tables work for basic use cases

## Exit Criteria
Core sequence ops spec subset works; omissions documented.
