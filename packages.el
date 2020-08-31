;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Change frame splits
(package! transpose-frame)

;; At the moment I like `evil-commentary' more than
;; `evil-nerd-commenter'. Might change in future if I understand how
;; to replicate all features.
(package! evil-nerd-commenter :disable t)
(package! evil-commentary)
;; `helm-bibtex' is pinned in `modules/tools/biblio/packages.el'
;; Use latest version of `helm-bibtex' instead.
(when (featurep! :completion helm)
  (package! helm-bibtex :pin nil))

(package! jupyter)
