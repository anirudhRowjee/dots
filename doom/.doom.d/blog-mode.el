;;; blog-mode.el -*- lexical-binding: t; -*-


;; Custom Blog Mode! :D
;; Inspired by https://willschenk.com/articles/2021/emacs_blogging_mode/
;; set the directory
(setq blog-mode-base-dir "/home/anirudh/portfolio/data/")
(require 'transient)

;; Peek Function - Reads _entite file_ to extract something based on regex
(defun blog-mode-file-peek (pattern file)
    (let ((result (car (process-lines "awk" "-F: " (concat pattern " {print $2}") file))))
      (if result
          (replace-regexp-in-string "\"" "" result)
        "")))

;; Custom peek function - check for WIP
(defun blog-mode-file-peek-wip (pattern file)
    (let ((result (car (process-lines "awk" (concat pattern " {print $0}") file))))
      (if result
          (replace-regexp-in-string "\"" "" result)
          (let ((result "true")))
        "")))

;; Function to extract tags from the post
(defun blog-mode-extract-tags (pattern file)
    (let (
          (result (format "%S" (process-lines "awk" "-F- " (concat pattern " {print $2}")  file)))
          )
      (if result
          (replace-regexp-in-string "\"" "" result)
        "no tags"))
    )

;; Parse an Org File
(defun blog-mode-parse-org (file)
  (let ((title (blog-mode-file-peek "/\\+title/" file))
        (date (blog-mode-file-peek "/\\+date/" file))
        (draft (blog-mode-file-peek "/\\+draft/" file))
        (tags (blog-mode-file-peek "/\\+tags/" file)))
    (list file (vector title draft date tags))))
;; Parse a Markdown File
;; TODO - Figure out how to Parse Tags & WIP Status using AWK
(defun blog-mode-parse-md (file)
  (let ((title (blog-mode-file-peek "/^title/" file))
        (date (blog-mode-file-peek "/^date/" file))
        (draft (blog-mode-file-peek-wip "/( )*WIP/" file))
        (tags (blog-mode-extract-tags "/  - /" file)))
    (list file (vector title draft date tags))))
;; Check if there's a .md or .org file hiding somewhere in a directory
(defun blog-mode-parse-directory (directory)
  (let ((md (concat directory "/index.md"))
        (org (concat directory "/index.org")))
    (if (file-exists-p md)
        (blog-mode-parse-md md)
      (if (file-exists-p org)
          (blog-mode-parse-org org)
        nil))))
;; Delegate a parser based on the file extension
(defun blog-mode-parse (file)
  (if (file-directory-p file)
  (blog-mode-parse-directory file)
    (let ((ex (file-name-extension file)))
      (if (string= ex "md")
          (blog-mode-parse-md file)
        (if (string= ex "org")
            (blog-mode-parse-org file)
          )))))
;; Source all the data from the directory
(defun blog-mode-refresh-data ()
  (setq blog-mode-entries nil)
  (dolist (file (process-lines "find" blog-mode-base-dir  "-maxdepth" "4" "-print"))
    (let ((entry (blog-mode-parse file)))
      (if entry
          (push (blog-mode-parse file) blog-mode-entries)))))
;; Implement the mode itself
(define-derived-mode blog-mode tabulated-list-mode "blog-mode" "Major mode Blog Mode, to edit hugo blogs"
  (setq tabulated-list-format [("Title" 60 t)
                               ("Draft" 5 nil)
                               ("Date"  11 t)
                               ("Tags" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Date" t))
  (use-local-map blog-mode-map)
  (tabulated-list-init-header))

(defun blog-list ()
  (interactive)
  (pop-to-buffer "*Blog Mode*" nil)
  (blog-mode)
  (blog-mode-refresh-data)
  (setq tabulated-list-entries (-non-nil blog-mode-entries))
    (tabulated-list-print t))

;; Implement the popup with transient
(defvar blog-mode-map nil "keymap for blog-mode")

(setq blog-mode-map (make-sparse-keymap))

(define-key blog-mode-map (kbd "?") 'blog-mode-help)
(define-key blog-mode-map (kbd "o") 'blog-mode-open)
(define-key blog-mode-map (kbd "<return>") 'blog-mode-open)
(define-key blog-mode-map (kbd "d") 'blog-mode-drafts)
(define-key blog-mode-map (kbd "a") 'blog-mode-all)
(define-key blog-mode-map (kbd "p") 'blog-mode-published)
(define-key blog-mode-map (kbd "r") 'blog-mode-refresh-all)
(define-key blog-mode-map (kbd "c") 'blog-mode-make-draft)
(define-key blog-mode-map (kbd "s") 'blog-mode-start-hugo)
(define-key blog-mode-map (kbd "RET") 'blog-mode-open)

(transient-define-prefix blog-mode-help ()
  "Help transient for blog mode."
  ["Blog mode help"
   ("o" "Open" blog-mode-open)
   ("d" "Drafts" blog-mode-drafts)
   ("a" "All" blog-mode-all)
   ("p" "Published" blog-mode-published)
   ("r" "Refresh" blog-mode-refresh-all)
   ("c" "Create post" blog-mode-make-draft)
   ("s" "Start hugo" blog-mode-start-hugo)
   ])

;; Actions - Open a file
(defun blog-mode-open ()
  (interactive)
  (find-file (tabulated-list-get-id)))

;; Actions - Fetch all data again
(defun blog-mode-refresh-all ()
  (interactive)
  (progn
    (blog-mode-refresh-data)
    (setq tabulated-list-entries (-non-nil blog-mode-entries))
    (tabulated-list-print t)))

;; Actions - Show all Data
(defun blog-mode-all ()
  (interactive)
  (progn
    (setq tabulated-list-entries (-non-nil blog-mode-entries))
    (tabulated-list-print t)))

;; Actions - show drafts
(defun blog-mode-drafts ()
  (interactive)
  (progn
    (setq tabulated-list-entries
          (-filter (lambda (x)
                     (string= "true"
                              (aref (car (cdr x)) 1))) (-non-nil blog-mode-entries)))
    (tabulated-list-print t)))

(defun blog-mode-published ()
  (interactive)
  (progn
    (setq tabulated-list-entries
          (-filter (lambda (x)
                     (string= ""
                              (aref (car (cdr x)) 1))) blog-mode-entries)))
  (tabulated-list-print t))

(global-set-key (kbd "C-c d") 'blog-list)
