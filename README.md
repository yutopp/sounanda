# Sounanda

> A sample project for a frontend of Sounanda programming language

同人誌:[進捗大陸05](https://shinchoku-tairiku.github.io/web/#shinchoku_tairiku_05)(技術書典6参加予定)の内容の一部のサンプルプロジェクトです．

このリポジトリは，自作言語 `Sounanda` のフロントエンド実装であり，具体的には以下の内容が含まれます．

- Syntax checker
- Type checker
- LSP server

## 使い方

現状，`Sounanda`言語はLinux上のEmacsでのみ動作確認を行っています．

### Emacsでの設定

このリポジトリを`~/repo/sounanda`にcloneした場合，以下のような設定を`init.el`に書き足すことで`Sounanda`言語の補完を行える準備が整います．

```elisp
;; Sounanda
(setq sounanda-base (expand-file-name "~/repo/sounanda"))

(add-to-list 'load-path (concat (file-name-as-directory sounanda-base) "editor/emacs/"))
(require 'sounanda-mode)

(setq sounanda-lsp-server-bin (concat (file-name-as-directory sounanda-base)
                                      "_build/install/default/bin/sounanda-lsp-server"))
(require 'sounanda-lsp-client)

(setq auto-mode-alist (cons '("\\.sounanda\\w?" . sounanda-mode) auto-mode-alist))

(add-hook 'sounanda-mode-hook #'lsp)
```

設定を行った後，cloneしたディレクトリ(例:`~/repo/sounanda`) で `dune build && emacs examples/test.sounanda &` を実行してください．

## ライセンス

NYSLとします．
