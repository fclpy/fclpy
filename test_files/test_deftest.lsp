;;; Minimal test file that uses DEFTEST
;;; This file should be loaded AFTER rt.lsp and cl-test-package.lsp

(in-package :cl-test)

;;; Simple test using DEFTEST macro
(deftest simple-add-test
  (+ 1 2)
  3)

(deftest simple-mul-test
  (* 2 3)
  6)
