;;; General Settings ;;;

;; path where settings files are kept

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/settings")

;; remap the key alt and fn
(setq mac-option-modifier nil)
(setq mac-function-modifier 'meta)

;; to use Melpa
(require 'package)
(add-to-list 'package-archives
'("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
'("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; tell emacs where to read abbrev
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

(setq debug-on-error t)


;; (set-default 'truncate-lines t)

;; Line and Column mode: display current line and column number in mode line
(setq line-number-mode t)
(setq column-number-mode t)
(add-hook 'prog-mode-hook 'linum-mode) ;It could slow down emacs, only view line numbers in the buffer in a programme mode.

;; == spelling check: specify the path to aspell == ;;
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "en_US")


;; == Ido mode (Emacs completion) == ;;
(require 'ido)
(ido-mode t)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; colour theme : adwaita, deeper-blue, dichromacy, light-blue, manoj-dark, misterioso, tango, tango-dark, tsdh-dark, tsdh-light, wheatgrass, whiteboard, wombat
(load-theme 'misterioso)


;; == company mode == ;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backend 'company-c-headers)
(defun my/python-mode-hook ()
(add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)


;; == magit ==;;
(setq magit-refresh-status-buffer nil)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)


;; == flycheck == ;;
(use-package flycheck
:ensure t
:pin melpa-stable
:init (global-flycheck-mode))
(add-hook 'after-init-hook #'global-flycheck-mode) ;; permanently enable flycheck
;; (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))


;; == open files in external app == ;;
(defun xah-open-in-external-app ()
"Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-01-26"
(interactive)
(let* (
(-file-list
(if (string-equal major-mode "dired-mode")
(dired-get-marked-files)
(list (buffer-file-name))))
(-do-it-p (if (<= (length -file-list) 5)
t
(y-or-n-p "Open more than 5 files? "))))

(when -do-it-p
(cond
((string-equal system-type "windows-nt")
(mapc
(lambda (fPath)
(w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) -file-list))
((string-equal system-type "darwin")
(mapc
(lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  -file-list))
((string-equal system-type "gnu/linux")
(mapc
(lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) -file-list))))))

(global-set-key (kbd "C-x C-o") 'xah-open-in-external-app)


;; == Python mode == ;;
(package-initialize)
(elpy-enable)
(pyenv-mode)
(setq python-shell-interpreter "python"
Python-shell-interpreter-args "-i")
;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i --simple-prompt")
(setq elpy-rpc-virtualenv-path "~/.pyenv/versions/3.6.8")
(defalias 'workon 'pyvenv-workon)
(setenv "WORKON_HOME" "~/.pyenv/versions/")
(setenv "PYTHONPATH" (concat (getenv "PYTHONPATH") ":/opt/X11/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/opt/X11/bin"))
;;(setq elpy-rpc-python-command "python")
(add-hook 'python-mode-hook
(lambda ()
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq-default py-indent-tabs-mode t)
(add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(setq elpy-rpc-backend "jedi")
(with-eval-after-load 'python
(defun python-shell-completion-native-try ()
"Return non-nil if can trigger native completion."
(let ((python-shell-completion-native-enable t)
(python-shell-completion-native-output-timeout
python-shell-completion-native-try-output-timeout))
(python-shell-completion-native-get-completions
(get-buffer-process (current-buffer))
nil "_"))))


;; == Projectile == ;;
(use-package projectile
:ensure t
:pin melpa-stable
:config
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1))
(setq projectile-project-search-path '("~/Desktop/" "~/Google Drive/my_impls/"))

;; == treemacs == ;;
(use-package treemacs
:ensure t
:defer t
:init
(with-eval-after-load 'winum
(define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
:config
(progn
(setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
treemacs-deferred-git-apply-delay      0.5
treemacs-directory-name-transformer    #'identity
treemacs-display-in-side-window        t
treemacs-eldoc-display                 t
treemacs-file-event-delay              5000
treemacs-file-extension-regex          treemacs-last-period-regex-value
treemacs-file-follow-delay             0.2
treemacs-file-name-transformer         #'identity
treemacs-follow-after-init             t
treemacs-git-command-pipe              ""
treemacs-goto-tag-strategy             'refetch-index
treemacs-indentation                   2
treemacs-indentation-string            " "
treemacs-is-never-other-window         nil
treemacs-max-git-entries               5000
treemacs-missing-project-action        'ask
treemacs-move-forward-on-expand        nil
treemacs-no-png-images                 nil
treemacs-no-delete-other-windows       t
treemacs-project-follow-cleanup        nil
treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
treemacs-position                      'left
treemacs-recenter-distance             0.1
treemacs-recenter-after-file-follow    nil
treemacs-recenter-after-tag-follow     nil
treemacs-recenter-after-project-jump   'always
treemacs-recenter-after-project-expand 'on-distance
treemacs-show-cursor                   nil
treemacs-show-hidden-files             t
treemacs-silent-filewatch              nil
treemacs-silent-refresh                nil
treemacs-sorting                       'alphabetic-asc
treemacs-space-between-root-nodes      t
treemacs-tag-follow-cleanup            t
treemacs-tag-follow-delay              1.5
treemacs-user-mode-line-format         nil
treemacs-user-header-line-format       nil
treemacs-width                         35)

;; The default width and height of the icons is 22 pixels. If you are
;; using a Hi-DPI display, uncomment this to double the icon size.
;;(treemacs-resize-icons 44)

(treemacs-follow-mode t)
(treemacs-filewatch-mode t)
(treemacs-fringe-indicator-mode t)
(pcase (cons (not (null (executable-find "git")))
(not (null treemacs-python-executable)))
(`(t . t)
(treemacs-git-mode 'deferred))
(`(t . _)
(treemacs-git-mode 'simple))))
:bind
(:map global-map
("M-0"       . treemacs-select-window)
("C-x t 1"   . treemacs-delete-other-windows)
("C-x t t"   . treemacs)
("C-x t B"   . treemacs-bookmark)
("C-x t C-t" . treemacs-find-file)
("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-magit
:after treemacs magit
:ensure t)

(use-package treemacs-projectile
:after treemacs projectile
:ensure t)


;; Latex mode
(require 'latex-settings)

;; cplusplus mode
(require 'cplusplus-settings)
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;; org mode
(require 'org-settings)

;; deft mode, a mode for quickly browsing, filtering, and
;; editing directories of plain text notes.
(use-package deft
:bind ("<f8>" . deft)
:commands (deft)
:config (setq deft-directory "~/Dropbox/notes"
deft-extensions '("txt" "tex" "org" "md")
deft-recursive t
deft-use-filename-as-title t))

;; ;; markdown mode
;; (use-package markdown-mode
;;   :ensure t
;;   :commands (markdown-mode gfm-mode)
;;   :mode (("README\\.md\\'" . gfm-mode))
;;   :init (setq markdown-command "/usr/local/bin/multimarkdown"))



;; ;;----------------------;;
;; ;;;;     plugins      ;;;;
;; ;;----------------------;;


(custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
'(org-agenda-files
(quote
("~/Desktop/notes/c++/rvalues_movesemantics.org" "~/Dropbox/[yinan]_drafts/tasks.org")))
'(package-selected-packages
(quote
(deft exec-path-from-shell flycheck websocket use-package treemacs-projectile treemacs-magit swiper sr-speedbar skewer-mode request python-mode pyenv-mode org magit-popup ggtags flx-ido find-file-in-project elpy company-jedi company-c-headers company-auctex cl-generic auto-complete)))
'(safe-local-variable-values (quote ((TeX-command-extra-options . "-shell-escape")))))
(custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
)

