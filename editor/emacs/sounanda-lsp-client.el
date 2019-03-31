;;; sounanda-lsp-client.el --- LSP client configurations for sounanda-lang. -*- lexical-binding: t; -*-

;; Copyright (C) 2019- yutopp

;; Author: yutopp

;;; Code:

(require 'lsp-mode)
(require 'sounanda-mode)

(defgroup sounanda-lsp nil
  "Sounanda."
  :group 'lsp-mode
  :tag "Sounanda")

(defvar sounanda-lsp-server-bin "sounanda-lsp-server")

(add-to-list 'lsp-language-id-configuration '(sounanda-mode . "sounanda"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection sounanda-lsp-server-bin)
                  :major-modes '(sounanda-mode)
                  :server-id 'sounanda-ls))

(provide 'sounanda-lsp-client)

;;; sounanda-lsp-client.el ends here
