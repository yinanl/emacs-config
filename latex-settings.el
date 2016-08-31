;-----------;
;;; LaTeX ;;;
;-----------;

;; set environment path and execution path for tex binaries
;; "/usr/texbin" is a symbolic link of "/usr/local/texlive/2013/bin/x86_64-darwin" in this mac
(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/usr/local/bin") exec-path))

;; set load path for .el or .elc files
;(add-to-list 'load-path "/Applications/Emacs.app/Contents/Resources/site-lisp")
;(add-to-list 'load-path "/usr/local/Cellar/emacs/24.5/share/emacs/site-lisp")
(add-to-list 'load-path "/Users/yinan/.emacs.d/elpa/auctex-11.89.3")
(add-to-list 'load-path "/Users/yinan/.emacs.d/elpa/async-20160513.128")
;(load "tex-site.el" nil t t)
;(load "auctex.el" nil t t)
;(load "preview.el" nil t t)
;(load "preview-latex.el" nil t t)

;; basic settings
(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil) ;; in case use \include or \input
(setq reftex-plug-into-AUCTeX t) 

(require 'tex)
;(setq-default TeX-engine 'xetex)
(setq TeX-PDF-mode t)

(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
     		'("XeLaTeX" "xelatex -interaction=nonstopmode %s"
		  TeX-run-command t t :help "Run xelatex") t))

;; mode settings
(add-hook 'TeX-mode-hook 'visual-line-mode)
(add-hook 'TeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
; reftex setting
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUTeX t)

;;make latexmk available via C-c C-c
;;Note: SyncTeX is setup via ~/.latexmkrc
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

(setq TeX-view-program-selection'((output-pdf "PDF Viewer")))

;; used for forward search
(setq TeX-view-program-list
      '(("PDF Viewer""/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;;(setq TeX-source-correlate-mode 'SyncTeX)
;;(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(server-start) ;; used for backward search


(provide 'latex-settings)
