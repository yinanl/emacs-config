;; org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-hide-leading-stars t)
(setq org-adapt-indentation nil)

;(setq org-startup-truncated nil)
;(add-hook 'org-mode-hook 'toggle-word-wrap)

(add-hook 'org-mode-hook #'(lambda ()

                             ;; make the lines in the buffer wrap around the edges of the screen.
                             
                             ;; to press C-c q  or fill-paragraph ever again!
                             (visual-line-mode)
                             (org-indent-mode)))

;; org-tree-slides
(global-set-key (kbd "<f8>") 'org-tree-slide-mode)
(global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)


;; publish
;; (setq org-publish-project-alist
;;       '(("org-notes"
;;          :base-directory "~/org/"
;;          :publishing-directory "~/public_html/"
;;          :publishing-function org-twbs-publish-to-html
;;          :with-sub-superscript nil
;;          )))


(provide 'org-settings)
