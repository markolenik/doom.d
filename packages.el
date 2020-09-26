;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(unpin! bibtex-completion helm-bibtex org-roam)
(disable-packages! company-org-roam evil-nerd-commenter)

;; Change frame splits
(package! transpose-frame)

(package! evil-commentary)
(package! visual-fill-column)
(package! jupyter)
(package! org-ref)
(package! org-roam-bibtex)
(package! org-roam-server)
;; Toggle org-mode latex fragment on cursor hover
(package! org-fragtog)

(package! el-patch)
