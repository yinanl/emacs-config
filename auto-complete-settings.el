;-------------------;
;;; Auto-Complete ;;;
;-------------------;



;;(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20160416.604")
(add-to-list 'load-path "~/.emacs.d/elpa/popup-20160531.425")
(require 'auto-complete) 
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-1.3.1/ac-dict")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20160416.604/dict")
(require 'auto-complete-config) 
(ac-config-default)
(global-auto-complete-mode t)


(provide 'auto-complete-settings)
