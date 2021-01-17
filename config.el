;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; User config
(setq user-full-name "Mark Olenik"
      user-mail-address "mark.olenik@gmail.com")

;; (setq ispell-personal-dictionary "~/.aspell.en.pws")
;; (ispell-change-dictionary "en")

(setq global-hl-line-modes nil)

;; Set doom looks
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 10.5)
      darkokai-mode-line-padding 1
      doom-theme 'darkokai
      ;; doom-theme 'molokai
      ;; doom-theme 'eclipse
      ;; doom-theme 'leuven
      ;; doom-theme 'github
      ;; doom-theme 'doom-opera-light
      ;; doom-theme 'doom-vibrant
      ;; doom-theme 'doom-monokai-pro
      ;; doom-theme 'doom-molokai
      ;; doom-theme 'doom-monokai-classic
      ;; doom-theme 'doom-monokai-spectrum
      ;; doom-theme 'doom-laserwave
      ;; doom-theme 'doom-city-lights
      ;; doom-theme 'doom-ephemeral
      display-line-numbers-type nil)

(custom-set-faces!
  '((font-lock-function-name-face font-lock-keyword-face) :weight bold))
(scroll-bar-mode 1)

;; Some common options
(setq delete-by-moving-to-trash t
      confirm-kill-emacs nil
      windmove-wrap-around t)

;; Make help window bigger
(set-popup-rules!
 '(("^\\*[Hh]elp" :size 20 :select t)))



;; Setup some readline keys etc
(map! :ie "C-h" #'backward-delete-char-untabify
      :ie "C-k" #'kill-line
      :g "C-S-h" #'help-command
      :g "M-u" #'universal-argument)

;; Convenient keys
(map! :g "s-`" #'other-window
      :g "s-n" #'+default/new-buffer
      :g "s-N" #'make-frame-command
      :g "s-h" #'windmove-left
      :g "s-j" #'windmove-down
      :g "s-k" #'windmove-up
      :g "s-l" #'windmove-right
      :g "s-1" #'doom/window-maximize-vertically
      :g "s-0" #'delete-window
      :g "s-a" #'mark-whole-buffer
      (:after evil
       :g "s-s" #'+evil-window-split-a
       :g "s-d" #'+evil-window-vsplit-a))


(use-package! el-patch)


(use-package! which-key
  :init
  (setq which-key-idle-delay 0.3))


(use-package! evil
  :init
  (setq evil-ex-search-persistent-highlight nil
        evil-ex-search-highlight-all nil
        evil-want-fine-undo t
        evil-split-window-below t
        evil-vsplit-window-right t)
  :config
  ;; Don't use outline evil bindings, since they change default `M-h'
  (add-to-list '+evil-collection-disabled-list 'outline)
  (map!
   :g "M-o" #'+evil/insert-newline-below
   :g "M-O" #'+evil/insert-newline-above
   :n "ga" #'evil-switch-to-windows-last-buffer
   :n "0" #'doom/backward-to-bol-or-indent
  ;; Free readline bindings
   :i "C-d" #'delete-char))


(use-package! evil-snipe
  :after evil
  :init
  (setq evil-snipe-spillover-scope 'visible))


;; I prefer this over `evil-nerd-commenter'
(use-package! evil-commentary
  :after evil
  :config
  (evil-commentary-mode)
  (map! :g "M-/" #'evil-commentary-line
        :g "C-M-/" #'evil-commentary-yank-line
        :nv "gc" #'evil-commentary))


;; Perform operations on surroundings (brackets, quotes etc).
(use-package! evil-surround
  :after evil
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


(use-package! ivy
  :config
  (map! :g "M-y" #'counsel-yank-pop
        ;; :g "<f13>" #'+ivy/switch-workspace-buffer))
        :g "<f13>" #'ivy-switch-buffer
  (:map ivy-minibuffer-map
   "C-h" #'backward-delete-char-untabify)))


;; Sane company defaults
(use-package! company
  :config
  (set-company-backend! '(text-mode prog-mode)
    'company-files)
  (map!
    :i "C-l" #'+company/complete
   (:map company-active-map
    :g "C-l" #'company-complete-common
    :g "C-h" #'backward-delete-char-untabify
    :g "C-S-h" #'company-show-doc-buffer)))


(use-package! text-mode
  :init
  (setq-hook! 'text-mode-hook fill-column 90)
  :config
  (remove-hook! 'text-mode-hook #'(+fill-column-enable-h))
  (add-hook! 'text-mode-hook #'visual-fill-column-mode))


;; Tranpose window frames
(use-package! transpose-frame
  :config
  (map! :leader
        :prefix "w"
        "t" #'transpose-frame :desc "Transpose frame"))


;; TODO Set `org-tags-alist' to have common tags in org-roam
;; Not sure if setting here best solution though, would be better if
;; tags would be read from all files, see org-agenda ...
;; TODO efficient org-agenda?
(use-package! org
  ;; `org-num-mode' shows nubmered headings
  ;; :hook (org-mode . org-num-mode)
  :hook (org-mode . org-fragtog-mode)
  :init
  (setq org-log-done 'time
        ;; org-id-link-to-org-use-id t
        org-log-done nil
        org-adapt-indentation nil
        org-startup-folded 't

        ;; IDs across different files. If `t', Emacs creates a file, .orgids
        ;; in my case, with lists all the files and their respective heading
        ;; IDs. If no such file is found, Emacs will go through all files
        ;; and try to generate it, which might take time.
        ;; org-id-track-globally t
        org-hide-emphasis-markers t
        org-startup-with-latex-preview t
        ;; Add mathtools to latex packages.
        org-latex-packages-alist '(("" "mathtools" t)
                                   ("" "bm" t)
                                   ("" "amssymb" t))
        ;; org-attach should attach to relative dir, not absolute.
        ;; Otherwise hard to version control attachments.
        org-attach-id-dir ".attach/"
        org-tag-alist '(("QUESTION" . ?q) ; Unanswered question
                        ("NOTE" . ?n)     ; Turn this heading into a note
                        ("IDEA" . ?i)))   ; Some idea
  :config
  (setq org-startup-indented nil)
  (when (featurep! :lang org +pretty)
    (setq org-superstar-remove-leading-stars t))
  (map! :map org-mode-map
        :in "M-h" nil)

  ;; See bug with latex previews in org
  ;; https://github.com/hlissner/emacs-solaire-mode/issues/24
  (defun +org-update-latex-preview-background-color (&rest _)
    (setq-default
     org-format-latex-options
     (plist-put org-format-latex-options
                :background
                (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                    'default)
                                :background nil t))))
  (advice-add #'load-theme :after #'+org-update-latex-preview-background-color)

  )


(use-package! evil-org
  :after (evil org)
  :config
  (map! :map evil-org-mode-map
        ;; Free readline bindings
        :ie "C-h" nil
        :ie "C-d" nil
        :ien "C-S-h" nil
        :i "C-k" nil
        :i "C-j" nil
        :in "M-h" nil))


;; TODO Don't attach text files when dragndrop.
(use-package! org-download
  :after org
  :init
  (setq org-download-image-org-width 400
        org-download-heading-lvl nil))


;; TODO Create tag command (with ivy support)
;; TODO org-roam and org-agenda?
;; NOTE In doom the variable `+org-roam-open-buffer-on-find-file' controls whether
;; a backlinks buffer open automatically or not.
;; TODO Why does automatic file rename not work on title change?
;; BUG duplicate ids everywhere, but I can only find always one id
;; For now I've disabled logging of duplicate ids, but that's not a longterm
;; solution. Google. I'm sure someone else has encountered this problem.
(use-package! org-roam
  ;; `org-roam-directory' is set to "~/org/roam" by doom by default
  :init
  (setq org-roam-db-gc-threshold most-positive-fixnum
        org-roam-tag-sources '(prop last-directory)
        +org-roam-open-buffer-on-find-file nil)
  ;; Set up templates
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam-capture--get-point)
           "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)
          ("a" "action" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_TAGS: \"action\"\n"
           :unnarrowed t))
        org-roam-capture-immediate-template
        '("d" "default" plain (function org-roam-capture--get-point)
          "%?"
          :file-name "%<%Y%m%d%H%M%S>-${slug}"
          :head "#+TITLE: ${title}\n"
          :unnarrowed t
          :immediate-finish t)
        ;; roam-ref protocol: Create new note linked to a website reference.
        ;; roam-ref is triggered by clicking on a brower bookmarklet.
        org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "web/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n\n"
           :unnarrowed t))
        org-roam-dailies-capture-templates
        '(("d" "daily" plain #'org-roam-capture--get-point
           ""
           :file-name "dailies/%<%Y%m%d>"
           :head "#+TITLE: %<%A, %d %B %Y>"
           :immediate-finish t)))
  :config
  (setq org-agenda-files (list (concat org-roam-directory "dailies")))
  (map!
   :g "C-<f13>" #'org-roam-switch-to-buffer
   :g "<f14>" #'org-roam-find-file-immediate
   :g "<C-f14>" #'org-roam-find-file
   :g "<menu>" #'org-roam-insert-immediate
   :g "<C-menu>" #'org-roam-insert
   :g "<f9>" #'org-roam-db-build-cache
   (:leader :prefix ("r" . "roam")
    :desc "Switch to buffer"              "b" #'org-roam-switch-to-buffer
    :desc "Org Roam Capture"              "c" #'org-roam-capture
    :desc "Find file"                     "f" #'org-roam-find-file
    :desc "Show graph"                    "g" #'org-roam-graph
    :desc "Insert"                        "i" #'org-roam-insert
    :desc "Insert (skipping org-capture)" "I" #'org-roam-insert-immediate
    :desc "Org Roam"                      "r" #'org-roam
    :desc "Rebuild db cache"              "R" #'org-roam-db-build-cache
    :desc "Jump to index"                 "TAB" #'org-roam-jump-to-index
    (:prefix ("d" . "by date")
     :desc "Arbitrary date" "d" #'org-roam-dailies-date
     :desc "Today"          "t" #'org-roam-dailies-find-today
     :desc "Tomorrow"       "m" #'org-roam-dailies-tomorrow
     :desc "Yesterday"      "y" #'org-roam-dailies-yesterday))
   (:map org-roam-backlinks-mode-map
    :desc "Close backlinks buffer" :n "q" #'org-roam-buffer-deactivate)))



;; Pretty note graphs
(use-package! org-roam-server
  ;; Load this pacakge after org-roam is called.
  :after-call org-roam
  :config
  ;; This is to fix a bug with Doom:
  ;; https://github.com/org-roam/org-roam-server/issues/115
  (defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (smartparens-global-mode -1)
    (org-roam-server-mode 1)
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))
    (smartparens-global-mode 1))
  (map!
   (:leader :prefix ("r" . "roam")
    :desc "Start server for web graph" "G" #'org-roam-server-open)))


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
        org-ref-default-bibliography '("~/bib/bibliography.bib")
        org-ref-pdf-directory "~/bib"
        ;; Why nil here?
        org-ref-show-broken-links nil
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
        ;; Use `org-roam-bibtex' to edit files
        ;; TODO Make this conditional
        org-ref-notes-function 'orb-edit-notes))


(use-package! latex
  :init
  (setq +latex-viewers '(evince)
        TeX-save-query nil             ; Don't ask to save before compile.
        TeX-command-default "LatexMk"
        TeX-engine 'luatex)
  ;; Sane paragraph definition
  (setq-hook! 'LaTeX-mode-hook
    paragraph-start "\f\\|[ 	]*$"
    paragraph-separate "[ 	\f]*$")
  (add-hook! 'TeX-mode-hook #'hl-todo-mode))



(use-package! cdlatex
  :init
  (setq cdlatex-simplify-sub-super-scripts nil
        cdlatex-sub-super-scripts-outside-math-mode nil)
  (setq cdlatex-env-alist
        '(
          ("equation*" "\\begin{equation*}\n?\n\\end{equation*}" nil)
          ("beq*" "\\begin{equation*}\\boxed{\n?\n}\\end{equation*}" nil)
          )))


(use-package! bibtex
  :init
  (setq bibtex-maintain-sorted-entries t
        bibtex-align-at-equal-sign t
        bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator ""
        bibtex-autokey-titlewords 0
        bibtex-autokey-titlewords-stretch 0)
  :config
  ;; Set type of formatting performed by `bibtex-clean-entry'
  (add-to-list 'bibtex-entry-format 'unify-case)
  (add-to-list 'bibtex-entry-format 'whitespace)
  (add-to-list 'bibtex-entry-format 'realign)
  (add-to-list 'bibtex-entry-format 'sort-fields)
  (bibtex-set-dialect 'BibTeX)
  (map! :map bibtex-mode-map
        :localleader
        "desc" "Clean entry" :n "c" #'bibtex-clean-entry
        "desc" "Reformat"    :n "r" #'bibtex-reformat))


(use-package! ivy-bibtex
  :after (ivy org-roam)
  :defer t
  :init
  (setq bibtex-completion-bibliography "~/bib/bibliography.bib"
        bibtex-completion-library-path "~/bib"
        bibtex-completion-notes-path (concat org-roam-directory "bib/")
        bibtex-completion-pdf-field nil
        bibtex-completion-find-additional-pdfs t
        bibtex-completion-pdf-open-function
        (lambda (fpath) (call-process "xdg-open" nil 0 nil fpath))
        bibtex-completion-notes-template-multiple-files
        (concat
         "#+TITLE: ${title}\n"
         "#+ROAM_ALIAS: \"${=key=}\"\n"
         "#+ROAM_KEY: cite:${=key=}\n\n")))


;; Note tacking and searching
(use-package! deft
  :init
  (setq deft-directory org-directory
        deft-recursive t
        deft-extensions '("org" "md" "text" "txt")
        deft-file-naming-rules '((noslash . "_") (nospace . "_")
                                 (case-fn . downcase))
        deft-file-limit 40)
  :config
  (map! :g "<f15>" #'deft
        (:map deft-mode-map
         :desc "Close Deft buffer" :n "q" #'kill-this-buffer
         :i "C-h" #'deft-filter-decrement
         :i "C-w" #'deft-filter-decrement-word)))



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
                            nil t nil nil)))

  (defun mark/jupyter-connect-repl-most-recent ()
    "Connect to most recent Jupyter Kernel."
    (interactive)
    (let* ((path (shell-command-to-string "jupyter --runtime-dir"))
           (file-name (nth 0 (split-string path))))
      (jupyter-connect-repl
        (nth 0 (mapcar #'car
                       (sort
                        (directory-files-and-attributes file-name t ".json$")
                             #'(lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y)))))))
        nil t nil nil))))



;;(use-package! ranger
;;  :init
;;  (setq ranger-deer-show-details nil))


(use-package! git-gutter
  :init
  ;; Pointless to use git-gutter in org-mode, since I use visual lines anyway.
  (setq git-gutter:disabled-modes '(org-mode image-mode latex-mode)))


(use-package! smartparens
  :config
  (sp-with-modes 'org-mode
    (sp-local-pair "$" "$")
    (sp-local-pair "\\[" "\\]")))


;; ;; TODO Don't open new windows in new workspace, open in either
;; ;; a given workspace, or just the last used workspace.
;; (when (featurep! :ui workspaces)
;;   (map! :gn "<C-tab>" #'+workspace/switch-right
;;         :gn "<C-iso-lefttab>" #'+workspace/switch-left
;;         :gn "C-S-t" #'+workspace/new
;;         :gn "C-S-w" #'+workspace/delete
;;         :gn "C-S-r" #'+workspace/re))


(use-package! super-save
  :init
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 180)
  (super-save-mode 1))


;; Overly complicated solution to have `flycheck-mode' not turn on by default,
;; only when you toggle it with `SPC t f'.
(use-package! eglot
  :init
  ;; NOTE: Not sure if this is the right place to set this variable.
  (setq flycheck-global-modes nil)
  :config
  (when (featurep! :checkers syntax)
    (after! flycheck
      ;; I turn off flycheck after it is turned on by `+lsp-eglot-prefer-flycheck-h',
      ;; which is why the function has to be appended to the hook with `:append'.
      (add-hook! 'eglot--managed-mode-hook :append
        (defun mark/turn-off-flycheck ()
          (flycheck-mode -1))))))
