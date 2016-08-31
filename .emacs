;;----------------------;;
;;;; General Settings ;;;;
;;----------------------;;

;; path where settings files are kept
(add-to-list 'load-path "~/.emacs.d/settings")

;; remap the key alt and fn
(setq mac-option-modifier nil)
(setq mac-function-modifier 'meta)

;; to use Melpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; tell emacs where to read abbrev
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

(setq debug-on-error t)




;;----------------------;;
;;;;   Minor Modes    ;;;;
;;----------------------;;

;; Line and Column mode: display current line and column number in mode line
(setq line-number-mode t)
(setq column-number-mode t)
(add-hook 'prog-mode-hook 'linum-mode) ;It could slow down emacs, only view line numbers in the buffer in a programme mode.

;; spelling check: specify the path to aspell
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "en_US")


;; Ido mode
(require 'ido)
(ido-mode t)

;; colour theme : adwaita, deeper-blue, dichromacy, light-blue, manoj-dark, misterioso, tango, tango-dark, tsdh-dark, tsdh-light, wheatgrass, whiteboard, wombat
(load-theme 'misterioso)


;; Auto-complete-mode
(require 'auto-complete-settings)


;; open files in external app
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



;;----------------------;;
;;;;   Major Modes    ;;;;
;;----------------------;;

;; Python mode
(require 'python-settings)


;; Latex mode
(require 'latex-settings)


;; cplusplus mode
(require 'cplusplus-settings)


;;----------------------;;
;;;;     plugins      ;;;;
;;----------------------;;

;; path to where plugins are kept
;;(setq plugin-path "~/.emacs.d/el-get/")

;; Yasnippet: a template system for Emacs (https://github.com/capitaomorte/yasnippet)
(add-to-list 'load-path
                "~/.emacs.d/elpa/yasnippet-20160629.1922")
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((company-clang-arguments "-I/usr/local/Cellar/boost/1.55.0_2/include/" "-I/usr/local/include" "-I/Applications/MATLAB_R2015b.app/extern/include/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
