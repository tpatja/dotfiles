;; Python

(add-to-list 'auto-mode-alist '("\\.py\\'" .  python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)
            (local-set-key (kbd "C-M-<right>") 'hs-show-block)
            (local-set-key (kbd "C-M-<left>")  'hs-hide-block)
            (local-set-key (kbd "C-M-<up>")    'hs-hide-all)
            (local-set-key (kbd "C-M-<down>")  'hs-show-all)
            (elpy-mode t)))

(require 'pydoc-info)

(info-lookup-add-help
 :mode 'python-mode
 :parse-rule 'pydoc-info-python-symbol-at-point
 :doc-spec
 '(("(python)Index" pydoc-info-lookup-transform-entry)
   ("(sphinx)Index" pydoc-info-lookup-transform-entry)))

(elpy-enable)

(eval-after-load "elpy-mode"
  '(progn
    (define-key elpy-mode-map (kbd "M-<right>") nil)
    (define-key elpy-mode-map (kbd "M-<left>") nil)))
