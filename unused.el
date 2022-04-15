
(use-package! org-roam
  :init
  ;; `org-roam-directory' is set to "~/org/roam" by doom by default
  (setq org-roam-db-gc-threshold most-positive-fixnum
        +org-roam-open-buffer-on-find-file nil)
  ;; Set up templates
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n\n")
           :unnarrowed t)
          ("i" "immediate" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n\n")
           :unnarrowed t
           :immediate-finish t)
          ("f" "fleeting" plain "%?"
           :if-new (file+head "fleeting/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n\n")
           :unnarrowed t)
          ("F" "flaschenpost" plain "%?"
           :if-new (file+head "flaschenpost/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n\n")
           :unnarrowed t)
          ))

  :config
  (setq org-roam-completion-everywhere nil)
  ;; No need for timestamp after all
  ;; Timestamp
  ;; (setq time-stamp-active t
  ;;       time-stamp-start "#\\+last_modified:[ \t]*"
  ;;       time-stamp-end "$"
  ;;       time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
  ;; (add-hook 'before-save-hook 'time-stamp nil)
  (map!
   :g "<f19>" #'org-roam-node-find
   :g "<C-f19>" #'org-roam-node-insert
   (:map org-mode-map
    :localleader
    :prefix ("m" . "org-roam")
    "a" #'org-roam-alias-add
    "A" #'org-roam-alias-delete
    "t" #'org-roam-tag-add
    "T" #'org-roam-tag-remove
    "R" #'org-roam-db-build-cache))

  (defun org-hide-properties ()
    "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
        (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov_this 'display "")
          (overlay-put ov_this 'hidden-prop-drawer t))))
    (put 'org-toggle-properties-hide-state 'state 'hidden))

  (defun org-show-properties ()
    "Show all org-mode property drawers hidden by org-hide-properties."
    (interactive)
    (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
    (put 'org-toggle-properties-hide-state 'state 'shown))

  (defun org-toggle-properties ()
    "Toggle visibility of property drawers."
    (interactive)
    (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
        (org-show-properties)
      (org-hide-properties)))

  ;; Redefine this org-roam function to hide properties.
  ;; I'm sure there's a better solution
  (defun org-roam-buffer-persistent-redisplay ()
    "Recompute contents of the persistent `org-roam-buffer'.
Has no effect when there's no `org-roam-node-at-point'."
    (when-let ((node (org-roam-node-at-point)))
      (unless (equal node org-roam-buffer-current-node)
        (setq org-roam-buffer-current-node node
              org-roam-buffer-current-directory org-roam-directory)
        (with-current-buffer (get-buffer-create org-roam-buffer)
          (org-roam-buffer-render-contents)
          ;; Hide properties
          (org-hide-properties)
          ;; Show only titles of backlinks
          (magit-section-show-level-2)
          (add-hook 'kill-buffer-hook #'org-roam-buffer--persistent-cleanup-h nil t)))))
  )


;; (use-package! org-roam-timestamps
;;   :after org-roam
;;   :config (org-roam-timestamps-mode))

;; ;; NOTE: Just testing
;; (use-package! nroam
;;   :after org-roam
;;   :config
;;   (add-hook 'org-mode-hook #'nroam-setup-maybe))


;; BUG: Seems to slow down everything, in particular problematic
;; with dailies.  Not using for now.
;; ;; Pretty note graphs
;; (use-package! org-roam-server
;;   ;; Load this pacakge after org-roam is called.
;;   :after-call org-roam
;;   :config
;;   ;; This is to fix a bug with Doom:
;;   ;; https://github.com/org-roam/org-roam-server/issues/115
;;   (defun org-roam-server-open ()
;;     "Ensure the server is active, then open the roam graph."
;;     (interactive)
;;     (smartparens-global-mode -1)
;;     (org-roam-server-mode 1)
;;     (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))
;;     (smartparens-global-mode 1))
;;   (map!
;;    (:leader :prefix ("r" . "roam")
;;     :desc "Start server for web graph" "G" #'org-roam-server-open)))

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :init
  (setq orb-templates
        '(("b" "bib-note" plain #'org-roam-capture--get-point
           "%?"
           :file-name "bib/${citekey}"
           :head "#+TITLE: ${title}\n#+ROAM_ALIAS: \"${=key=}\"\n#+ROAM_KEY: ${ref}\n\n"
           :unnarrowed t))))


(use-package! org-ref
  :after (org org-roam)
  :init
  (setq org-ref-notes-directory (concat org-roam-directory "bib/")
        ;; Why nil here?
        org-ref-show-broken-links nil
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
        ;; Use `org-roam-bibtex' to edit files
        ;; TODO Make this conditional
        org-ref-notes-function 'orb-edit-notes))
