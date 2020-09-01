;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Mark Olenik"
      user-mail-address "mark.olenik@gmail.com")

(setq doom-theme 'doom-vibrant)
(custom-set-faces!
  `(show-paren-match :foreground ,(doom-color 'bg)
                     :background ,(doom-color 'magenta) :weight bold))


;; Set font
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 10.5))

(setq confirm-kill-emacs nil)

(scroll-bar-mode 1)

(setq display-line-numbers-type nil)


;; Setup some readline keys etc
(map! :ige "C-h" #'delete-backward-char
      :ige "C-d" #'delete-forward-char
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
        :n "ga" #'evil-switch-to-windows-last-buffer
        :n "0" #'doom/backward-to-bol-or-indent))


(use-package! evil-snipe
  :init
  (setq evil-snipe-spillover-scope 'visible))


(use-package! helm
  :init
  (setq helm-move-to-line-cycle-in-source t)
  :config
  (map! "<f13>" #'helm-mini
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
         "C-l" #'helm-execute-persistent-action)
        (:map helm-read-file-map
         "C-l" #'helm-execute-persistent-action)))


(use-package! helm-bibtex
  :after helm
  :defer t
  :init
  (setq helm-bibtex-full-frame t
        bibtex-completion-bibliography "~/bib/bibliography.bib"
        bibtex-completion-library-path "~/bib"
        bibtex-completion-notes-path "~/notes/bib"
        bibtex-completion-pdf-field nil
        bibtex-completion-find-additional-pdfs t
        bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "xdg-open" nil 0 nil fpath))
        bibtex-completion-notes-template-multiple-files
        (concat
         "#+TITLE: ${title}\n"
         "#+ROAM_ALIAS: ${=key=}\n"
         "#+ROAM_KEY: cite:${=key=}\n\n")))


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


(use-package! org
  ;; `org-num-mode' shows nubmered headings
  ;; :hook (org-mode . org-num-mode)
  :init
  (setq org-directory "~/notes/"
        org-log-done 'time
        org-log-into-drawer t
        org-id-link-to-org-use-id t
        ;; Setting this to `t' is necessary in order to be able to link to
        ;; IDs across different files. If `t', Emacs creates a file, .orgids
        ;; in my case, with lists all the files and their respective heading
        ;; IDs. If no such file is found, Emacs will go through all files
        ;; and try to generate it, which might take time.
        org-id-track-globally t))

(use-package! org-journal
  :init
  (setq org-journal-file-header "#+TITLE: %d %B %Y\n\n"
        org-journal-file-type 'weekly
        org-journal-date-format "%A, %d %B %Y"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-enable-agenda-integration t)
  :config
  (map! :map calendar-mode-map :leader
        :n "j m" 'org-journal-mark-entries
        :n "j r" 'org-journal-read-entry
        :n "j d" 'org-journal-display-entry
        :n "] j" 'org-journal-next-entry
        :n "[ j" 'org-journal-previous-entry
        :n "j n" 'org-journal-new-date-entry
        :n "j s w" 'org-journal-search-calendar-week
        :n "j s m" 'org-journal-search-calendar-month
        :n "j s y" 'org-journal-search-calendar-year))


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
        +org-roam-open-buffer-on-find-file nil)
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
           :file-name "web/${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n
 - source :: ${ref}\n\n"
           :unnarrowed t)))
  :config
  (map!
   (:map org-roam-backlinks-mode-map
    :desc "Close backlinks buffer" :n "q" #'org-roam-buffer-deactivate)
   (:desc "Switch to roam buffer" :g "C-<f13>" #'org-roam-switch-to-buffer
    :desc "Roam find file"        :g "<f14>" #'org-roam-find-file)))


(use-package! latex
  :init
  (setq +latex-viewers '(evince)
        TeX-save-query nil             ; Don't ask to save before compile.
        TeX-command-default "LatexMk"
        TeX-engine 'luatex)
  ;; Sane paragraph definition
  (setq-hook! 'LaTeX-mode-hook
    paragraph-start "\f\\|[ 	]*$"
    paragraph-separate "[ 	\f]*$"))


(use-package! python
  :init
  (setq jupyter-repl-echo-eval-p nil
        jupyter-eval-use-overlays t)
  :preface
  (defun mark/jupyter-connect-repl ()
    "Connect to Jupyter Kernel with kernel file suggestion and without
opening REPL buffer."
    (interactive)
    (let* ((path (shell-command-to-string "jupyter --runtime-dir"))
           (file-name (nth 0 (split-string path))))
      (jupyter-connect-repl (read-file-name "Connection file: "
                                            (concat file-name "/"))
      nil t nil nil))))




;; NOTE Not using it, due to interferences with TAB funtionality. Easier to call `org-roam-insert-immediate'.
;; ;; BUG
;; ;; You shouldn't have to change company-backends manually. There's something wrong
;; ;; in `modules/lang/org/config.el'. I think they overwrite the backend.
;; (use-package! company-org-roam
;;   :when (featurep! :completion company)
;;   :after org-roam
;;   :config
;;   (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))
