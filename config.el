;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; User config
(setq user-full-name "Mark Olenik"
      user-mail-address "mark.olenik@gmail.com")

;; (setq ispell-personal-dictionary "~/.aspell.en.pws")
(ispell-change-dictionary "en_GB" t)



;; Set doom looks
(setq ;; doom-theme 'spolsky
 ;; doom-theme 'darkokai
 ;; darkokai-mode-line-padding 1
 ;; doom-theme 'monokai
 ;; doom-theme 'molokai
 doom-theme 'doom-xcode
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
 )

(setq! +modeline-height 20)

;; ----------------------------------------------------------
;; Mac stuff
;;
(setq mac-pass-command-to-system t
      ;; That's my long awaited hyper.
      mac-right-control-modifier 'hyper)
;; (map! :g "s-`" #'other-frame
;; :g "s-~" (lambda () (interactive) (other-frame -1)))
;; This is a hack to enable proper cmd-TAB switching in mac,
;; the menubar doesn't actually appear this way.
;; (menu-bar-mode -1)
(map! :gnv "<C-tab>" #'mac-next-tab-or-toggle-tab-bar)
(defun mark/make-new-frame ()
  (interactive)
  (make-frame-command)
  (mac-move-tab-to-new-frame))

;; ----------------------------------------------------------

;; Some common options
(scroll-bar-mode 1)
(setq display-line-numbers-type nil)
(setq delete-by-moving-to-trash t
      confirm-kill-emacs nil
      windmove-wrap-around t)

(setq doom-localleader-key "\\")

;; Disable evil-snipe for now
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
;; Disable highlight line
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)
;; Don't add comments to new line
(setq +default-want-RET-continue-comments nil)

;; Make help window bigger
(set-popup-rules!
  '(("^\\*[Hh]elp" :size 20 :select t)))

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
(map! :g "s-n" #'make-frame-command
      :g "s-t" #'make-frame-command
      ;; Delete frame and don't ask to confirm
      :g "s-w" (lambda () (interactive) (delete-frame nil t))
      :g "H-h" #'windmove-left
      :g "H-j" #'windmove-down
      :g "H-k" #'windmove-up
      :g "H-l" #'windmove-right
      ;; BUG: Why this no work??!! \('_')/
      ;; :g "<H-left>" #'windmove-left
      ;; :g "<H-down>" #'windmove-down
      ;; :g "<H-up>" #'windmove-up
      ;; :g "<H-right>" #'windmove-right
      :g "H-\\" #'doom/window-maximize-vertically
      :g "H-0" #'delete-window
      :g "s-p" #'counsel-find-file
      ;; :g "<C-tab>" #'mac-next-tab-or-toggle-tab-bar
      ;; :g "<C-S-tab>" #'mac-previous-tab-or-toggle-tab-bar
      ;; :g "s-t" #'mac-PDF-to-string
      :g "<f19>" #'counsel-find-file
      (:after evil
       :g "H-s" #'+evil-window-split-a
       :g "H-d" #'+evil-window-vsplit-a))


;; Bind C-m to RET, see 3b0f23792
;;(define-key key-translation-map [?\C-m] [return])
(define-key key-translation-map (kbd "RET") nil)


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
        :g "<f18>" #'ivy-switch-buffer
        (:map ivy-minibuffer-map
         "C-h" #'backward-delete-char-untabify)))


;; Sane company defaults
(use-package! company
  :init
  (setq company-selection-wrap-around t)
  :config
  ;; TODO wtf is this?
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
  :hook ((org-mode . org-fragtog-mode)
         (org-mode . org-autolist-mode)
         (org-mode . +org-pretty-mode))
  :init
  (setq org-log-done 'time
        ;; org-id-link-to-org-use-id t
        org-log-done nil
        org-startup-folded 'content
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
        org-startup-indented nil)
  ;; NOTE: Not sure if that's correct.
  ;; TODO: Double check
  ;; see https://emacs.stackexchange.com/questions/3397/how-to-replace-an-element-of-an-alist/3402
  (add-to-list 'org-blank-before-new-entry
               '(plain-list-item . nil))
  (when (featurep! :lang org +pretty)
    (setq org-superstar-remove-leading-stars t))
  (add-hook! 'org-mode-hook
    (defun adjust-latex-image-scale ()
      (plist-put org-format-latex-options :scale 1.4)))
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
        +org-roam-open-buffer-on-find-file nil)
  ;; Set up templates
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)
          ("i" "immediate" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t
           :immediate-finish t)
          ("f" "fleeting" plain "%?"
           :if-new (file+head "fleeting/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)))

  :config
  (setq org-roam-completion-everywhere nil)
  ;; Timestamp
  (setq time-stamp-active t
        time-stamp-start "#\\+last_modified:[ \t]*"
        time-stamp-end "$"
        time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
  (add-hook 'before-save-hook 'time-stamp nil)
  (map!
   :g "<f17>" #'org-roam-node-find
   :g "<C-f17>" #'org-roam-node-insert
   (:map org-mode-map
    :localleader
    :prefix ("m" . "org-roam")
    "a" #'org-roam-alias-add
    "A" #'org-roam-alias-delete
    "t" #'org-roam-tag-add
    "T" #'org-roam-tag-remove
    "R" #'org-roam-db-build-cache)))


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


;; (use-package! org-roam-bibtex
;;   :after org-roam
;;   :hook (org-roam-mode . org-roam-bibtex-mode)
;;   :init
;;   (setq orb-templates
;;         '(("b" "bib-note" plain #'org-roam-capture--get-point
;;            "%?"
;;            :file-name "bib/${citekey}"
;;            :head "#+TITLE: ${title}\n#+ROAM_ALIAS: \"${=key=}\"\n#+ROAM_KEY: ${ref}\n\n"
;;            :unnarrowed t))))


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
  (setq
   TeX-save-query nil             ; Don't ask to save before compile.
   TeX-engine 'luatex
   TeX-electric-sub-and-superscript nil)
  ;; Don't preview figures
  (setq preview-default-option-list
        '("displaymath" "textmath" "sections" "footnotes" "showlabels"))
  ;; Sane paragraph definition
  (setq-hook! 'LaTeX-mode-hook
    paragraph-start "\f\\|[ 	]*$"
    paragraph-separate "[ 	\f]*$")
  (add-hook! 'TeX-mode-hook #'hl-todo-mode)
  (add-hook! 'TeX-mode-hook #'TeX-fold-mode)
  ;; NOTE tex-fold is to show \textbf as bold etc
  ;; https://emacs.stackexchange.com/questions/14113/how-to-render-latex-expressions-not-necessarily-formula-inside-buffer
  ;; TODO Try the same with quotes (``'')!
  ;; TODO Need to do this in a hook or sth
  ;; :config
  ;; (add-to-list 'TeX-fold-macro-spec-list
  ;;              '("[c]" ("cite" "citep")))
  )



(use-package! cdlatex
  :init
  (setq cdlatex-simplify-sub-super-scripts t
        cdlatex-sub-super-scripts-outside-math-mode t)
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
        (lambda (fpath) (call-process "open" nil 0 nil fpath))
        bibtex-completion-notes-template-multiple-files
        (concat
         "#+TITLE: ${title}\n"
         "#+ROAM_ALIAS: \"${=key=}\"\n"
         "#+ROAM_KEY: cite:${=key=}\n\n")))


;; ;; Note tacking and searching
;; (use-package! deft
;;   :init
;;   (setq deft-directory org-directory
;;         deft-recursive t
;;         deft-extensions '("org" "md" "text" "txt")
;;         deft-file-naming-rules '((noslash . "_") (nospace . "_")
;;                                  (case-fn . downcase))
;;         deft-file-limit 40)
;;   :config
;;   (map! (:map deft-mode-map
;;          :desc "Close Deft buffer" :n "q" #'kill-this-buffer
;;          :i "C-h" #'deft-filter-decrement
;;          :i "C-w" #'deft-filter-decrement-word)))

(use-package! notdeft
  :init
  (setq notdeft-directories '("~/org/roam")
        notdeft-allow-org-property-drawers t)
  :config
  (setq notdeft-xapian-program "/Users/mark/.emacs.d/.local/straight/repos/notdeft/xapian/notdeft-xapian")
  (map!
   ((:map notdeft-mode-map
     :n "q" #'notdeft-quit))))


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
    ;; But it setting it to t seems bugged atm, need to find out why.
    ;; TODO Jupyter should always send output stuff to *jupyter-result*, and
    ;; never to *jupyter-output*, confusing to have both.
    (setq jupyter-pop-up-frame t
          jupyter-repl-echo-eval-p t
          jupyter-eval-use-overlays t
          jupyter-eval-short-result-max-lines 0)
    ;; For finding the kernel
    (setq mark/jupyter-kernel-dir
          "/sshx:tpmark:/home/mark/.local/share/jupyter/runtime/")
    :config
    ;; NOTE: Most of these functions should could be implemented more easily
    ;; with macros (probably).
    ;; TODO At the moment the command is printed in REPL as well, change that.
    ;; ... maybe change max-line-num or sth?

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
      ;; FIXME
      (let* ((file-name (nth 0 (split-string mark/jupyter-kernel-dir))))
        (jupyter-connect-repl
         (nth 0 (mapcar #'car
                        (sort
                         (directory-files-and-attributes file-name t ".json$")
                         #'(lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y)))))))
         nil t nil nil)
        ;; Import numpy's shape when connecting.
        (jupyter-eval-string "from numpy import shape as np_shape")
        ))

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
      (format "%s(%s)" fun var))

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
      (mark/jupyter-call-point-or-region-with "np_shape"))

    (map!
     (:map python-mode-map
      :localleader
      :n "\\" #'mark/jupyter-connect-repl-most-recent
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
;; (use-package! flycheck
;;   :init
;;   (setq-hook! 'python-mode-hook flycheck-disabled-checkers '(lsp)))


;; TODO Add key to `magit-git-push' without without entering magit
;; (use-package! magit)

;; This is just so that I can work on samba files without these annoyting popups
(use-package! modtime-skip-mode
  :hook ((org-mode . modtime-skip-mode)
         (python-mode . modtime-skip-mode)))
;; BUG Shit aint switchin off!
;; Also disable `auto-save-mode', causes hangups with samba
;; (auto-save-mode -1)
(setq auto-save-default nil)

(when (featurep! :tools debugger +lsp)
  (use-package! dap-mode
    :init
    (setq dap-python-debugger 'debugpy)
    :config
    ;; NOTE: Bug with dap + poetry.  This fixes it, but would be good to find a
    ;; https://www.reddit.com/r/emacs/comments/k5dsar/emacs_ide_for_python_setting_up_the_debugger_with/
    (defun dap-python--pyenv-executable-find (command)
      (with-venv (executable-find "python")))
    ))


;; TODO This is still crap, FIXME
(when (featurep! :tools lsp)
  (map!
   (:map lsp-mode-map
    :leader
    :n "cR" #'lsp-ui-peek-find-references)))

(map! :leader
      (:prefix "t"
       :desc "Highlight line"  "h" #'hl-line-mode))
