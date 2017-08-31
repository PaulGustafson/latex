;; (require 'package)
;; (add-to-list
;;   'package-archives
;;   '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)

;; -*- emacs-lisp -*-
(unless package-archive-contents    ;; Refresh the packages descriptions
  (package-refresh-contents))
(setq package-load-list '(all))     ;; List of packages to load
(unless (package-installed-p 'org)  ;; Make sure the Org package is
  (package-install 'org))           ;; installed, install it if not
(package-initialize)                ;; Initialize & Install Package
;; (setq org-...)                   ;; Your custom settings


(defun my-randomize-region (beg end)
  "Randomize lines in region from BEG to END."
  (interactive "*r")
  (let ((lines (split-string
		(delete-and-extract-region beg end) "\n")))
    (when (string-equal "" (car (last lines 1)))
      (setq lines (butlast lines 1)))
    (apply 'insert
	   (mapcar 'cdr
		   (sort (mapcar (lambda (x) (cons (random) (concat x "\n"))) lines)
			 (lambda (a b) (< (car a) (car b))))))))

;; cubical type theory
;(add-to-list 'load-path "/home/a/.emacs.d/cl-lib/")
;(require 'cl-lib)
;; (load-file "/home/a/dev/cubicaltt/cubicaltt.el")
;; (autoload 'cubicaltt-mode "cubicaltt" "cubical editing mode" t)
;; (setq auto-mode-alist (append auto-mode-alist '(("\\.ctt$" . cubicaltt-mode))))

(autoload 'cubicaltt-mode "cubicaltt" "cubical editing mode" t)
(setq auto-mode-alist (append auto-mode-alist '(("\\.ctt$" . cubicaltt-mode))))


(add-to-list 'load-path "/home/a/.emacs.d/misc/")
(require 'fill-column-indicator)

(setq latex-run-command "pdflatex")


;(global-set-key (kbd "C-h") 'query-replace)

;(add-to-list 'load-path "/home/a/.emacs.d/elpa/org-20161224")
;(require 'org-loaddefs)

;;ETAGS
(setq tags-revert-without-query 1)


;; share system clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; python
(add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)
(add-hook 'python-mode-hook (lambda ()
                                    (guess-style-guess-tab-width)))

;; visual
(setq column-number-mode t)
(invert-face 'default)

;; haskell
(add-hook 'haskell-mode-hook 'intero-mode)

;; (eval-after-load 'haskell-mode '(progn
;;   (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;;   (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
;; (eval-after-load 'haskell-cabal '(progn
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
;; (eval-after-load 'haskell-mode
;;           '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

;; (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
;;   (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
;;   (add-to-list 'exec-path my-cabal-path))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 ;; '(haskell-mode-hook (quote (turn-on-haskell-doc turn-on-haskell-indentation)))
 ;; '(haskell-process-auto-import-loaded-modules t)
 ;; '(haskell-process-log t)
 ;; '(haskell-process-suggest-remove-import-lines t)
 ;; '(haskell-tags-on-save t)
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((eval let ((unimath-topdir (expand-file-name (locate-dominating-file buffer-file-name "UniMath")))) (setq fill-column 100) (make-local-variable (quote coq-use-project-file)) (setq coq-use-project-file nil) (make-local-variable (quote coq-prog-args)) (setq coq-prog-args (\` ("-emacs" "-indices-matter" "-type-in-type" "-Q" (\, (concat unimath-topdir "UniMath")) "UniMath"))) (make-local-variable (quote coq-prog-name)) (setq coq-prog-name (concat unimath-topdir "sub/coq/bin/coqtop")) (make-local-variable (quote before-save-hook)) (add-hook (quote before-save-hook) (quote delete-trailing-whitespace)) (modify-syntax-entry 39 "w") (modify-syntax-entry 95 "w") (if (not (memq (quote agda-input) features)) (load (concat unimath-topdir "emacs/agda/agda-input"))) (if (not (member (quote ("chimney" "╝")) agda-input-user-translations)) (progn (setq agda-input-user-translations (cons (quote ("chimney" "╝")) agda-input-user-translations)) (agda-input-setup))) (set-input-method "Agda")) (eval let ((unimath-topdir (expand-file-name (locate-dominating-file buffer-file-name "UniMath")))) (make-local-variable (quote coq-use-project-file)) (setq coq-use-project-file nil) (make-local-variable (quote coq-prog-args)) (setq coq-prog-args (\` ("-emacs" "-indices-matter" "-type-in-type" "-Q" (\, (concat unimath-topdir "UniMath")) "UniMath"))) (make-local-variable (quote coq-prog-name)) (setq coq-prog-name (concat unimath-topdir "sub/coq/bin/coqtop")) (make-local-variable (quote before-save-hook)) (add-hook (quote before-save-hook) (quote delete-trailing-whitespace)) (modify-syntax-entry 39 "w") (modify-syntax-entry 95 "w") (if (not (memq (quote agda-input) features)) (load (concat unimath-topdir "emacs/agda/agda-input"))) (set-input-method "Agda")))))
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
