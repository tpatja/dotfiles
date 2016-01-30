;; Python

(add-to-list 'auto-mode-alist '("\\.py\\'" .  python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
