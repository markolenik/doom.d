;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; User config
(setq user-full-name "Mark Olenik"
      user-mail-address "mark.olenik@gmail.com")

;; (setq ispell-personal-dictionary "~/.aspell.en.pws")
;; (ispell-change-dictionary "en")

(setq global-hl-line-modes nil)

(scroll-bar-mode 1)

;; Set doom looks
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 12)
      ;; doom-theme 'spolsky
      doom-theme 'darkokai
      darkokai-mode-line-padding 1
      ;; doom-theme 'monokai
      ;; doom-theme 'molokai
      ;; doom-theme 'eclipse
      ;; doom-theme 'leuven
      ;; doom-theme 'github
      ;; doom-theme 'doom-one
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

(if (string-equal (system-name) "office")
    (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 14))
  (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 12)))

;; Some common options
(setq delete-by-moving-to-trash t
      confirm-kill-emacs nil
      windmove-wrap-around t)

(setq doom-localleader-key "\\")

;; Make help window bigger
(set-popup-rules!
 '(("^\\*[Hh]elp" :size 20 :select t)))

;; Optimise tramp
(setq tramp-chunksize 2000
      tramp-copy-size-limit nil)

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


;; Setup some readline keys etc
(map! :ie "C-h" #'backward-delete-char-untabify
      :ie "C-k" #'kill-line
      :g "C-S-h" #'help-command
      :g "M-u" #'universal-argument)

(map! :map minibuffer-local-map
      "C-h" #'backward-delete-char)

(map! :gin "C-RET" nil
      :gin "<C-return>" nil)

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
   :i "C-d" #'delete-char
   :map evil-ex-completion-map
   "C-h" #'delete-backward-char
   "C-d" #'delete-char))


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
        ;; f14 should be RALT
        :g "<f14>" #'ivy-switch-buffer
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


(use-package! org
  ;; `org-num-mode' shows nubmered headings
  ;; :hook (org-mode . org-num-mode)
  :hook ((org-mode . org-fragtog-mode)
         (org-mode . org-autolist-mode)
         (org-mode . +org-pretty-mode))
  ;; :custom-face
  ;; (org-level-1 ((t (:inherit outline-1 :height 1.0))))
  ;; (org-level-2 ((t (:inherit outline-2 :height 1.0))))
  ;; (org-level-3 ((t (:inherit outline-3 :height 1.0))))
  ;; (org-level-4 ((t (:inherit outline-4 :height 1.0))))
  ;; (org-level-5 ((t (:inherit outline-5 :height 1.0))))
  :init
  (setq org-log-done 'time
        ;; org-id-link-to-org-use-id t
        org-log-done nil
        ;; org-startup-folded 't
        org-pretty-entities-include-sub-superscripts nil
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
                                        ; Turn this heading into a note,
                                        ; or do some other improvement.
                        ("REFACTOR" ?r)
                        ("IDEA" . ?i))
        org-edit-src-persistent-message nil
        org-edit-src-turn-on-auto-save t)
  :config
  (setq org-list-demote-modify-bullet
        '(("+" . "*") ("-" . "+") ("*" . "+") ("1." . "a."))
        org-indent-indentation-per-level 1
        org-src-window-setup 'current-window
        org-startup-indented t)
  ;; NOTE: Not sure if that's correct.
  ;; TODO: Double check
  ;; see https://emacs.stackexchange.com/questions/3397/how-to-replace-an-element-of-an-alist/3402
  (add-to-list 'org-blank-before-new-entry
               '(plain-list-item . nil))
  (when (featurep! :lang org +pretty)
    (setq org-superstar-remove-leading-stars t))
  (add-hook! 'org-mode-hook
    (defun adjust-latex-image-scale ()
      (if (> (length (display-monitor-attributes-list)) 1)
          (plist-put org-format-latex-options :scale 1.5)
        (plist-put org-format-latex-options :scale 2.5))
      ))
  (map! :map org-mode-map
        :ie "C-l" nil))


(use-package! evil-org
  :after (evil org)
  :config
  (map! :map evil-org-mode-map
        ;; Free readline bindings and co
        :ie "C-h" nil
        :ie "C-d" nil
        :ien "C-S-h" nil
        :i "C-k" nil
        :i "C-j" nil
        :i "C-l" nil
        :in "M-h" nil
        :in "M-l" nil
        :ien "C-M-h" #'org-metaleft
        :ien "C-M-l" #'org-metaright))


;; TODO Don't attach text files when dragndrop.
(use-package! org-download
  :after org
  :init
  (setq org-download-image-org-width 400
        org-download-heading-lvl nil))


(use-package! org-roam
  :init
  ;; `org-roam-directory' is set to "~/org/roam" by doom by default
  (setq org-roam-db-gc-threshold most-positive-fixnum
        org-roam-tag-sources '(prop last-directory)
        org-roam-graph-exclude-matcher '("dailies")
        +org-roam-open-buffer-on-find-file nil
        )
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
        '(("d" "daily" entry
           #'org-roam-capture--get-point
           "%?"
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+TITLE: %<%A, %d %B %Y>"
           :immediate-finish t))
        )
  :config
  (setq
   ;; I think agenda slows things down...
   ;; org-agenda-files (list (concat org-roam-directory "dailies"))
   org-roam-completion-everywhere nil)
  (map!
   :g "C-<f14>" #'org-roam-switch-to-buffer
   :g "<f13>" #'org-roam-find-file-immediate
   :g "<C-f13>" #'org-roam-insert-immediate
   (:prefix "<f9>"
    :desc "Switch to buffer"              "b" #'org-roam-switch-to-buffer
    :desc "Org Roam Capture"              "c" #'org-roam-capture
    :desc "Find file"                     "f" #'org-roam-find-file
    :desc "Show graph"                    "g" #'org-roam-graph
    :desc "Insert"                        "i" #'org-roam-insert
    :desc "Insert (skipping org-capture)" "I" #'org-roam-insert-immediate
    :desc "Org Roam"                      "r" #'org-roam
    :desc "Org Roam"                      "<f9>" #'org-roam
    :desc "Rebuild db cache"              "R" #'org-roam-db-build-cache
    :desc "Jump to index"                 "TAB" #'org-roam-jump-to-index
    (:prefix ("d" . "by date")
     :desc "Find previous note" "b" #'org-roam-dailies-find-previous-note
     :desc "Find date"          "d" #'org-roam-dailies-find-date
     :desc "Find next note"     "f" #'org-roam-dailies-find-next-note
     :desc "Find tomorrow"      "m" #'org-roam-dailies-find-tomorrow
     :desc "Capture today"      "n" #'org-roam-dailies-capture-today
     :desc "Find today"         "t" #'org-roam-dailies-find-today
     :desc "Capture Date"       "v" #'org-roam-dailies-capture-date
     :desc "Find yesterday"     "y" #'org-roam-dailies-find-yesterday
     :desc "Find directory"     "." #'org-roam-dailies-find-directory))
   (:map org-roam-backlinks-mode-map
    :desc "Close backlinks buffer" :n "q" #'org-roam-buffer-deactivate)))


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
  (map! (:map deft-mode-map
         :desc "Close Deft buffer" :n "q" #'kill-this-buffer
         :i "C-h" #'deft-filter-decrement
         :i "C-w" #'deft-filter-decrement-word)))



(use-package! python
  ;; For some reason company triggers a file transfer in tramp and ipython,
  ;; which makes completion slow.  Have to trigger completion manually for now.
  ;; (setq-hook! 'inferior-python-mode-hook company-idle-delay nil)
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


(use-package! code-cells
  :hook (python-mode . code-cells-mode)
  :config
  (map!
   (:map code-cells-mode-map
    :g "M-p" #'code-cells-backward-cell
    :g "M-n" #'code-cells-forward-cell
    :g "C-c C-SPC" #'cells-mark-cell
    ;; TODO Can't add description here, maybe because `code-cells-command' a
    ;; macro?
    :g "C-c C-w" (code-cells-command #'kill-region :use-region)
    :g "C-c M-w" (code-cells-command #'kill-ring-save :user-region
                                     :pulse)
    ;; TODO A simple `:after' statement would be more elegant, but for some
    ;; reason `:after jupyter' doesn't seem to work...
    (:when (featurep! :lang org +jupyter)
     :g "<C-return>" (code-cells-command #'jupyter-eval-region
                                         :use-region :pulse))
    )))


(when (featurep! :lang org +jupyter)
  (use-package! jupyter
    :init
    ;; NOTE It may make sense to change `jupyter-repl-echo-eval-p' variable
    ;; in context depending on function that calls it.
    ;; TODO Jupyter should always send output stuff to *jupyter-result*, and
    ;; never to *jupyter-output*, confusing to have both.
    (setq jupyter-pop-up-frame t
          jupyter-repl-echo-eval-p nil
          jupyter-eval-use-overlays t
          jupyter-eval-short-result-max-lines 0)
    :config
    ;; NOTE: Most of these functions should could be implemented more easily
    ;; with macros (probably).
    ;; TODO At the moment the command is printed in REPL as well, change that.
    ;; ... maybe change max-line-num or sth?

    (defun mark/jupyter-send-var-at-point ()
      "Send variable under cursor."
      (interactive)
      (jupyter-eval-string (thing-at-point 'symbol)))

    (defun mark/jupyter-send-var-or-region ()
      (interactive)
      (if (not (use-region-p))
          (mark/jupyter-send-var-at-point)
        (let ((beg (region-beginning))
              (end (region-end)))
          (if (> (evil-count-lines beg end) 1)
              (jupyter-eval-region)
            (jupyter-eval-string-command (buffer-substring beg end)))))
      (evil-force-normal-state))

    (defun mark/jupyter-get-var-call (var fun)
      "Return string where VAR is called with FUN."
      (format "print(\"%s(%s): \" + str(%s(%s)))" fun var fun var))

    (defun mark/jupyter-call-point-with (fun)
      "Call thing-at-point with FUN."
      (let ((var (thing-at-point 'symbol)))
        (jupyter-eval-string-command (mark/jupyter-get-var-call var fun))))

    (defun mark/jupyter-call-region-with (fun)
      "Call single line region with FUN."
      (let ((var (buffer-substring (mark) (point))))
        (jupyter-eval-string-command (mark/jupyter-get-var-call var fun))))

    (defun mark/jupyter-call-point-or-region-with (fun)
      "Call selected variable or variable under point with FUN."
      (if (use-region-p)
          (mark/jupyter-call-region-with fun)
        (mark/jupyter-call-point-with fun)))

    (defun mark/jupyter-send-len ()
      "Send length of var or region under point."
      (interactive)
      (mark/jupyter-call-point-or-region-with "len"))

    (defun mark/jupyter-send-max ()
      "Send length of var under or region point."
      (interactive)
      (mark/jupyter-call-point-or-region-with "max"))

    (defun mark/jupyter-send-min ()
      "Send length of var under or region point."
      (interactive)
      (mark/jupyter-call-point-or-region-with "min"))

    (defun mark/jupyter-send-shape ()
      "Send share of var under point or region. Works only in pylab atm."
      (interactive)
      (mark/jupyter-call-point-or-region-with "shape"))

    (map!
     (:map python-mode-map
      :localleader
      :n "\\" #'jupyter-connect-repl
      :n "v" #'mark/jupyter-send-var-or-region
      :n "m" #'mark/jupyter-send-min
      :n "M" #'mark/jupyter-send-max
      :n "s" #'mark/jupyter-send-shape
      :n "y" #'mark/jupyter-send-type
      :n "l" #'mark/jupyter-send-len
      :n "z" #'jupyter-repl-pop-to-buffer
      ))

    ))


(use-package! git-gutter
  :init
  ;; Pointless to use git-gutter in org-mode, since I use visual lines anyway.
  (setq git-gutter:disabled-modes '(org-mode image-mode latex-mode)))


(use-package! smartparens
  :preface
  (defun mark/open-block-org-mode (id action context)
    (when (eq action 'insert)
      (newline)
      (newline)
      (indent-according-to-mode)
      (previous-line)
      (indent-according-to-mode)))
  :config
  (sp-with-modes 'org-mode
    (sp-local-pair "$" "$")
    (sp-local-pair "\\[" "\\]"
                   :post-handlers '(:add mark/open-block-org-mode))))


(use-package! super-save
  :init
  (setq super-save-auto-save-when-idle nil
        super-save-idle-duration 180)
  (super-save-mode 1))


;; NOTE: Quickfix to disable flycheck for now.
;; TODO: Need better solution in future, i.e. toggle flycheck etc.  Atm can't
;; run flycheck manually either
(use-package! flycheck
  :init
  (setq-hook! 'python-mode-hook flycheck-disabled-checkers '(lsp)))


;; TODO Add key to `magit-git-push' without without entering magit
;; (use-package! magit)


;; ;; TODO Don't open new windows in new workspace, open in either
;; ;; a given workspace, or just the last used workspace.
;; (when (featurep! :ui workspaces)
;;   (map! :gn "<C-tab>" #'+workspace/switch-right
;;         :gn "<C-iso-lefttab>" #'+workspace/switch-left
;;         :gn "C-S-t" #'+workspace/new
;;         :gn "C-S-w" #'+workspace/delete
;;         :gn "C-S-r" #'+workspace/re))

;; This is just so that I can work on samba files without these annoyting popups
(use-package! modtime-skip-mode
  :hook ((org-mode . modtime-skip-mode)
         (python-mode . modtime-skip-mode)))
;; BUG Shit aint switchin off!
;; Also disable `auto-save-mode', causes hangups with samba
;; (auto-save-mode -1)
(setq auto-save-default nil)
