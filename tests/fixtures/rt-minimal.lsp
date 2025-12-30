;;; rt-minimal.lsp - Minimal regression test framework for testing LOAD
;;; Defines a test variable and function in COMMON-LISP-USER

(in-package :common-lisp-user)

(defvar *test-loaded-files* nil "List of loaded test files")

(defun record-file-load (name)
  "Record that a file was loaded."
  (setf *test-loaded-files* (cons name *test-loaded-files*))
  name)

;; Record this file was loaded
(record-file-load 'rt-minimal)
