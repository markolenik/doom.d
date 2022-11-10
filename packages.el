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
(package! xcode-theme
  :recipe (:host github :repo "juniorxxue/xcode-theme"))

;; Change frame splits
(package! transpose-frame)

(package! evil-commentary)
(package! visual-fill-column)
(package! el-patch)
(package! zmq)
(package! super-save)

;; Python
(package! code-cells)  ; Jupyter cells for execution

;; Snippets for docstrings (mostly Python)
(package! yasnippet-radical-snippets
  :recipe (:host github :repo "Xaldew/yasnippet-radical-snippets"
           :files (:defaults "snippets" "yasnippet-radical-snippets.el")))

(package! xpp-mode
  :recipe (:host github :repo "markolenik/xpp-mode"))


;; Open finder here
(package! reveal-in-osx-finder)
;; Open terminal here
(package! terminal-here)

;; Revert all buffers with some extra goodies
(package! revert-buffer-all)
(package! mac-pseudo-daemon)
