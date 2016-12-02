;;;;
;; Clojure
;;;;

;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; mandatory lambda symbol :)
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
           ;; (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))
            (clj-refactor-mode 1)))

(add-hook 'clojure-mode-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))


;;;;
;; Cider
;;;;

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

(setq cider-lein-command "/usr/local/bin/lein")
(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)



;; Use clojure mode for other extensions
(dolist (pattern '("\\.edn$"
                   "\\.boot$"
                   "lein-env"))
  (add-to-list 'auto-mode-alist (cons pattern 'clojure-mode)))

;; clj-refactor
(cljr-add-keybindings-with-prefix "C-c C-m")

(eval-after-load 'clj-refactor-mode
  (define-key clj-refactor-map (kbd "C-c '") 'hydra-cljr-cljr-menu/body))

(setq cljr-warn-on-eval nil)
