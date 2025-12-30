;;; sample-tests.lsp - Third file in the load chain
;;; Tests that functions defined in earlier files are still accessible

(in-package :common-lisp-user)

;; Use the function defined in rt-minimal.lsp
(record-file-load 'sample-tests)

;; Verify all files were loaded by checking the list
;; At this point *test-loaded-files* should have all three
