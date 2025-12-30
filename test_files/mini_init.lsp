;;; Minimal init file that mimics ansi-test/init.lsp loading sequence
;;; This loads rt.lsp, then cl-test-package.lsp, then the test file

(load "../ansi-test/rt.lsp")
(load "../ansi-test/cl-test-package.lsp")
(load "test_files/test_deftest.lsp")
