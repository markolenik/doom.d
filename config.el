;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Mark Olenik"
      user-mail-address "mark.olenik@gmail.com")

;; (setq doom-theme 'doom-molokai)

;; Set font
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 10.5))

(setq confirm-kill-emacs nil)

(scroll-bar-mode 1)

(setq display-line-numbers-type nil)


;; Setup some readline keys etc
(map! :ie "C-h" #'delete-backward-char
      :ie "C-d" #'delete-forward-char
      "C-S-h" #'help-command
      "M-u" #'universal-argument)


;; Convenient keys
(map! "s-`" #'other-window
      "s-n" #'+default/new-buffer
      "s-N" #'make-frame
      "s-h" #'windmove-left
      "s-j" #'windmove-down
      "s-k" #'windmove-up
      "s-l" #'windmove-right
      "s-1" #'doom/window-maximize-vertically
      "s-0" #'delete-window
      "s-a" #'mark-whole-buffer
      (:after evil
       "s-s" #'+evil-window-split-a
       "s-d" #'+evil-window-vsplit-a)
      :after helm "s-f" #'helm-occur)


(use-package! which-key
  :init
  (setq which-key-idle-delay 0.3))


(use-package! evil
  :init
  (setq evil-ex-search-persistent-highlight nil
        evil-ex-search-highlight-all nil
        evil-want-fine-undo t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-respect-visual-line-mode t)
  :config
  (map! "M-o" #'+evil/insert-newline-below
        "M-O" #'+evil/insert-newline-above
        :n "ga" #'evil-switch-to-windows-last-buffer))


(use-package! helm
  :init
  (setq helm-move-to-line-cycle-in-source t)
  :config
  (map!
   "<f13>" #'helm-mini
   "<f14>" #'helm-find-files
   "C-<f14>" #'helm-recentf
   "M-y" #'helm-show-kill-ring
   "M-m" #'helm-imenu
   (:leader
    :n "ss" #'helm-occur)
   (:map helm-map
    "C-h" #'delete-backward-char
    "M-O" #'helm-ff-run-open-file-with-default-tool
    "M-j" #'helm-next-source
    "M-k" #'helm-previous-source)
   (:map helm-find-files-map
    "C-l" #'helm-execute-persistent-action)))


;; Sane company defaults
(use-package! company
  :config
  (map!
   (:map company-active-map
    "C-h" #'delete-backward-char
    "C-S-h" #'company-show-doc-buffer)))



;; I prefer this over `evil-nerd-commenter'
(use-package! evil-commentary
  :config
  (evil-commentary-mode)
  (map! "M-/" #'evil-commentary-line
        "C-M-/" #'evil-commentary-yank-line
        :nv "gc" #'evil-commentary))


;; Perform operations on surroundings (brackets, quotes etc).
(use-package! evil-surround
  :config
  (evil-add-to-alist
   'evil-surround-pairs-alist
   ;; Get rid of useless spaces.
   ?\( '("(" . ")")
   ?\[ '("[" . "]")
   ?\{ '("{" . "}")
   ?\) '("(" . ")")
   ?\] '("[" . "]")
   ?\} '("{" . "}")
   ;; By default < should insert angle brackets, not tag.
   ;; For tag insertion use "t".
   ?\< '("<" . ">")))


(use-package! text-mode
  :config
  ;; Don't highlight or number lines in `text-mode'
  (remove-hook! 'text-mode-hook #'(hl-line-mode display-line-numbers-mode)))



;; Tranpose window frames
(use-package! transpose-frame
  :config
  (map! :leader
        :prefix "w"
        "t" #'transpose-frame :desc "Transpose frame"))


;; Note tacking and searching
(use-package! deft
  :init
  (setq deft-directory "~/notes"
        deft-recursive t
        deft-file-naming-rules '((noslash . "_") (nospace . "_")
                                 (case-fn . downcase)))
  :config
  (map! "C-<f15>" #'deft
        "<f15>" #'deft-find-file))


;; org-mode
(use-package! org
  ;; `org-num-mode' shows nubmered headings
  :hook (org-mode . org-num-mode)
  :init
  (setq org-directory "~/notes/"
        org-log-done 'time
        org-log-into-drawer t))

;; (use-package! org-journal
;;   )
;;


(use-package! evil-org
  :config
  (map! :map evil-org-mode-map
        ;; Free readline bindings
        :ie "C-h" nil
        :ie "C-d" nil
        :ien "C-S-h" nil))


(use-package! org-roam
  :init
  (setq org-roam-directory "~/notes"
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t)
  ;; Set up templates
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n\n"
           :unnarrowed t)
          ("f" "fleeting" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_TAGS: fleeting\n\n"
           :unnarrowed t)
          ;; ("b" "bib" plain (function org-roam-capture--get-point)
          ;;  "%?"
          ;;  :file-name "bib/${slug}"
          ;;  :head
          ;;  )
          )
        org-roam-capture-immediate-template
        '("d" "default" plain (function org-roam-capture--get-point)
          "%?"
          :file-name "${slug}"
          :head "#+TITLE: ${title}\n\n"
          :unnarrowed t
          :immediate-finish t)
        ;; roam-ref protocol: Create new note linked to a website reference.
        ;; roam-ref is triggered by clicking on a brower bookmarklet.
        org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "website/${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n
 - source :: ${ref}\n\n"
           :unnarrowed t)))
  :config
  ;; TODO There should be no need to do this. What goes wrong in doom?
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev))
  ;; (map! )
  )
