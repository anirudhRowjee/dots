;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Anirudh Rowjee"
      user-mail-address "ani.rowjee@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "IosevkaTerm NFP" :size 14)
      doom-variable-pitch-font (font-spec :family "IosevkaTerm NFP" :size 14))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:


(setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

(setq doom-theme 'doom-tokyo-night)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; change the default lsp server for rust
(after! lsp-rust
  (setq lsp-rust-server 'rust-analyzer))

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))


;; Treesitter
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; DAP
(use-package! dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(require 'dap-python)
(require 'dap-lldb)

(dap-register-debug-template "Rust::GDB Run Configuration"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "GDB::Run"
                                   :gdbpath "rust-gdb"
                                   :target nil
                                   :cwd nil))


(require 'dap-dlv-go)


;; custom keybindings, lessgooooo
(map! :leader
      :desc "split window vertically"
      "|" #'evil-window-vsplit
      )

(map! :leader
      :desc "split window horizontally"
      "-" #'evil-window-split
      )

;; soft wrap line at buffer boundaries
(global-visual-line-mode t)

;; bind file path completion
(map! :leader
      :desc "complete filepath dynamically at this point"
      "e" '(lambda ()
             (interactive)
             (comint-dynamic-complete-as-filename)
             )
      )


;; no transparency.

(custom-set-faces
 '(font-lock-comment-face nil :slant 'italic)
 '(font-lock-function-name-face nil  :slant 'italic)
 '(font-lock-variable-name-face nil  :slant 'italic)
 )
;; TODO https://david.rothlis.net/emacs/customize_colors.html

;; Breadcrums with LSP
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-headerline-breadcrumb-segments '(project file symbols))
(setq lsp-headerline-breadcrumb-icons-enable t)

;; Custom GitGutter Fringes
(define-fringe-bitmap 'git-gutter-fr:added [#b11100000] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:modified [#b11100000] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:deleted
  [#b10000000
   #b11000000
   #b11100000
   #b11110000] nil nil 'bottom)

;; Change the delay for evil-mode to make it more snappy
(setq evil-esc-delay 0.0001)

;; set the idle delay for autocomplete to make it more snappy
(setq company-idle-delay 0.0001)
(setq lsp-idle-delay 0.0001)

;; Snippet to walk a directory tree for python files and include it as org SRC Blocks
(require 'find-lisp)
(defun inject-include-find-file (root_dir regexp)
  (mapc
   (lambda (x)
     (insert "*** " x " \n")
     (insert "#+INCLUDE: " x " src py ")
     (insert "\n")
     )
   (find-lisp-find-files root_dir regexp)))

;; Tell Emacs to use EWW as the Default Browser
(setq browse-url-browser-function 'eww-browse-url)

;; Configure how EWW Opens
(defun eww-display+ (buf _alist)
  (let ((w (or (window-in-direction 'right)
               (window-in-direction 'left)
               (window-in-direction 'below)
               (window-in-direction 'above)
               (split-window-horizontally))))
    (set-window-buffer w buf)
    w))

(push `(,(rx "*eww*")
        (eww-display+))
      display-buffer-alist)

;; Blog Mode!
(load-file "/home/anirudh/.doom.d/blog-mode.el")

;; Remap C-x C-m to M-x
;; Thank you, Steve Yegge!
(global-set-key "\C-x\C-m" 'execute-extended-command) (global-set-key "\C-c\C-m" 'execute-extended-command)

;; New LSP Config to make things a little faster
(setq lsp-modeline-code-actions-segments '(icon))

(setq
 company-minimum-prefix-length 2
 ;; lsp-ui-sideline-enable nil
 company-idle-delay 0.0015 ;; default is 0.2
 ;; lsp-ui-sideline-show-hover nil
 lsp-lens-enable nil
)

;; Modeline / Statusline Config
(setq doom-modeline-time-icon t)
(setq doom-modeline-workspace-name t)
(setq doom-modeline-modal t)
(setq doom-modeline-time t)

;; Org-Mode Bell Sound
(setq org-clock-sound "~/.doom.d/pomo-bell.wav")
(add-to-list 'exec-path (expand-file-name "~/go/bin"))
