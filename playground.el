

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


