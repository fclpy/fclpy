;;; load-chain.lsp - Tests nested LOAD calls
;;; This mimics how init.lsp loads rt.lsp, then cl-test-package.lsp, then test files

(load "tests/fixtures/rt-minimal.lsp")
(load "tests/fixtures/test-package.lsp")
(load "tests/fixtures/sample-tests.lsp")
