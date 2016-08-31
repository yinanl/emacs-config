;------------------------;
;;; Python Programming ;;;
;------------------------;

; config python-mode
(setq py-install-directory "~/.emacs.d/python-mode.el-6.1.3")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
; don't split windows
(setq py-split-windows-on-execute-p t)

; try to automagically figure out indentation
(setq py-smart-indentation t)

(add-hook 'python-mode-hook
	  (lambda ()
		    (setq-default indent-tabs-mode t)
		    (setq-default tab-width 4)
		    (setq-default py-indent-tabs-mode t)
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;(setq py-shell-local-path "/usr/local/Cellar/python3/3.4.3_2/Frameworks/Python.framework/Versions/3.4/bin/python3")
;(setq py-use-local-default nil)

;; config eply
(package-initialize)
(elpy-enable)


;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  ;'(python-shell-interpreter "python3")
;;  '(python-shell-interpreter "ipython"))
;; (custom-set-faces)

(require 'ein)

(provide 'python-settings)
