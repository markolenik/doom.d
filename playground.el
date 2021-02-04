;; This is just code for experimenting around.


;; NOTE Not using it, due to interferences with TAB funtionality. Easier to call `org-roam-insert-immediate'.
;; ;; BUG
;; ;; You shouldn't have to change company-backends manually. There's something wrong
;; ;; in `modules/lang/org/config.el'. I think they overwrite the backend.
;; (use-package! company-org-roam
;;   :when (featurep! :completion company)
;;   :after org-roam
;;   :config
;;   (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))




(defun mark/+spell-correct-wrapper ()
  "Correct spelling error in a dwim fashion based on universal argument.
ADD SOME EXPLANATION HERE...
"
  (interactive)
  (let ((lexical-binding t))
    ;; (unless (memq 'spell-fu-incorrect-face (face-at-point nil t))
    ;;   (user-error "%S is correct" (thing-at-point 'word t)))
    (ispell-set-spellchecker-params)
    (save-current-buffer
      (ispell-accept-buffer-local-defs))
    (call-interactively #'ispell-word)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'word)
      (let ((word (thing-at-point 'word t))
            (orig-pt (point))
            poss ispell-filter)
        (ispell-send-string "%\n")
        (ispell-send-string (concat "^" word "\n"))
        (while (progn (accept-process-output ispell-process)
                      (not (string= "" (car ispell-filter)))))
        ;; Remove leading empty element
        (setq ispell-filter (cdr ispell-filter))
        ;; ispell process should return something after word is sent. Tag word as
        ;; valid (i.e., skip) otherwise
        (unless ispell-filter
          (setq ispell-filter '(*)))
        (when (consp ispell-filter)
          (setq poss (ispell-parse-output (car ispell-filter))))
        (cond
         ;; ((or (eq poss t) (stringp poss))
         ;;  ;; don't correct word
         ;;  (progn
         ;;  (message "%s is correct" (funcall ispell-format-word-function word))
         ;;  (cond
         ;;   ((eq poss t) (message "poss is t"))
         ;;   ((string poss) (message poss)))
         ;;  )
         ;;  t)
         ((null poss)
          ;; ispell error
          (error "Ispell: error in Ispell process"))
         (t
          ;; The word is incorrect, we have to propose a replacement.
          (setq res (nth 0 (nth 2 poss)))
          ;; (setq res (funcall +spell-correct-interface (nth 2 poss) word))
          ;; Some interfaces actually eat 'C-g' so it's impossible to stop rapid
          ;; mode. So when interface returns nil we treat it as a stop.
          (unless res (setq res (cons 'break word)))
          (cond
           ((stringp res)
            (+spell--correct res poss word orig-pt start end))
           ((let ((cmd (car res))
                  (wrd (cdr res)))
              (unless (or (eq cmd 'skip)
                          (eq cmd 'break)
                          (eq cmd 'stop))
                (+spell--correct cmd poss wrd orig-pt start end)
                (unless (string-equal wrd word)
                  (+spell--correct wrd poss word orig-pt start end))))))
          (ispell-pdict-save t)))))))




;; Add suffix to notes
;; (cd "~/org/roam/private")
(defun mark/note-add-date (note)
  "Add date of last file modification as suffix to file name"
  (let ((last-modified (format-time-string "%Y%m%d%H%M%S"
                                           (file-attribute-modification-time
                                            (file-attributes note)))))
    (rename-file note (format "%s-%s" last-modified (file-name-nondirectory note)))
    ))




;; (let* ((notes-dir "~/org/")
;;        (notes (org-roam--directory-files-recursively notes-dir ".*\.org$")))
;;          (mapcar #'mark/note-add-date notes))



(defun mark/remove-unused-properties ()
  (interactive)
  )


  ;; python-shell stuff
  (defun mark/python-send-var-at-point ()
    "Send variable under cursor."
    (interactive)
    (python-shell-send-string (thing-at-point 'symbol)))

  (defun mark/python-send-line ()
    (interactive)
    (python-shell-send-string
     (buffer-substring (line-beginning-position) (line-end-position))))

  (defun mark/python-send-var-or-region ()
    (interactive)
    (if (not (use-region-p))
        (mark/python-send-var-at-point)
      (let ((beg (region-beginning))
            (end (region-end)))
        (if (> (evil-count-lines beg end) 1)
            (python-shell-send-region)
          (python-shell-send-string (buffer-substring beg end)))))
    (evil-force-normal-state))


  (defun mark/python-get-var-call (var fun)
    "Return string where VAR is called with FUN."
    (format "print(\"%s(%s): \" + str(%s(%s)))" fun var fun var))

  (defun mark/python-call-point-with (fun)
    "Call ting-at-point with FUN."
    (let ((var (thing-at-point 'symbol)))
      (python-shell-send-string (mark/python-get-var-call var fun))))

  (defun mark/python-call-region-with (fun)
    "Call single line region with FUN."
    (let ((var (buffer-substring (mark) (point))))
      (python-shell-send-string (mark/python-get-var-call var fun))))

  (defun mark/python-call-point-or-region-with (fun)
    "Call selected variable or variable under point with FUN."
    (if (use-region-p)
        (mark/python-call-region-with fun)
      (mark/python-call-point-with fun)))

  (defun mark/python-send-len ()
    "Send length of var or region under point."
    (interactive)
    (mark/python-call-point-or-region-with "len"))

  (defun mark/python-send-max ()
    "Send length of var under or region point."
    (interactive)
    (mark/python-call-point-or-region-with "max"))

  (defun mark/python-send-min ()
    "Send length of var under or region point."
    (interactive)
    (mark/python-call-point-or-region-with "min"))

  (defun mark/python-send-shape ()
    "Send share of var under point or region. Works only in pylab atm."
    (interactive)
    (mark/python-call-point-or-region-with "shape"))

  (defun mark/python-send-type ()
    "Send type of var under point or region."
    (interactive)
    (mark/python-call-point-or-region-with "type"))
  :config
  (map!
   (:map python-mode-map
    :localleader
    :n "\\" #'run-python
    :n "v" #'mark/python-send-var-or-region
    :n "m" #'mark/python-send-min
    :n "M" #'mark/python-send-max
    :n "s" #'mark/python-send-shape
    :n "y" #'mark/python-send-type
    :n "l" #'mark/python-send-len)))




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
