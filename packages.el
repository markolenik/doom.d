;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Themes
(package! eclipse-theme)
(package! darkokai-theme)
(package! molokai-theme)
(package! monokai-theme)
(package! sublime-themes)
(package! leuven-theme)
(package! github-theme)

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
(package! org-roam-bibtex)

;; (package! org-roam-server)
(package! nroam)
;; Toggle org-mode latex fragment on cursor hover
(package! org-fragtog)
;; Return makes new bullet point
(package! org-autolist)
(package! el-patch)
(package! super-save)
(package! devdocs)

;; Don't ask to confirm save for files on smb mounts
(package! modtime-skip-mode)

;; Python
(package! code-cells)  ; Jupyter cells for execution

;; Snippets for docstrings (mostly Python)
(package! yasnippet-radical-snippets
   :recipe (:host github :repo "Xaldew/yasnippet-radical-snippets"
            :files (:defaults "snippets" "yasnippet-radical-snippets.el")))

(package! notdeft :recipe (:host github :repo "hasu/notdeft"))

(package! with-venv)
