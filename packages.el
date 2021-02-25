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
(package! org-ref)
;; Jupyter cells for execution
(package! code-cells)
(package! org-roam-bibtex)
(package! org-roam-server)
;; Toggle org-mode latex fragment on cursor hover
(package! org-fragtog)
(package! el-patch)
(package! super-save)
(package! devdocs)

;; Themes
(package! eclipse-theme)
(package! darkokai-theme)
(package! molokai-theme)
(package! monokai-theme)
(package! sublime-themes)
(package! leuven-theme)
(package! github-theme)
