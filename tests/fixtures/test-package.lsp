;;; test-package.lsp - Second file in the load chain
;;; Tests that functions defined in previous files are accessible

(in-package :common-lisp-user)

;; Use the function defined in rt-minimal.lsp
(record-file-load 'test-package)
