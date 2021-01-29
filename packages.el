;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; (unpin! bibtex-completion helm-bibtex org-roam)
;; NOTE: There's some bug with org-roam, pinning for now.
;; Error (org-roam): Failed to parse aliases for buffer:
(unpin! bibtex-completion helm-bibtex)
(disable-packages! evil-nerd-commenter)

;; Change frame splits
(package! transpose-frame)

(package! evil-commentary)
(package! visual-fill-column)
;; (package! jupyter)
;; `python-x' add some additional `python-shell' features
(package! python-x)
;; `python-cell' adds matlab-like cell
(package! python-cell)
(package! org-ref)
(package! org-roam-bibtex)
(package! org-roam-server)
;; Toggle org-mode latex fragment on cursor hover
(package! org-fragtog)

(package! el-patch)

(package! super-save)

(package! eclipse-theme)
(package! darkokai-theme)
(package! molokai-theme)
(package! leuven-theme)
(package! github-theme)
