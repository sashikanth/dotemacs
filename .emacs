(package-initialize)

;;; Load exec path from shell for mac
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'exec-path-from-shell)


;;; Marmalade package install

(add-to-list 'package-archives
  '("marmalade" .
    "http://marmalade-repo.org/packages/"))


;;; Melpa package archive2
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(require 'package)


(desktop-save-mode 1)
(server-start)

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
   (color-theme-initialize)
   (color-theme-solarized-dark)))

(set-default-font "Inconsolata-14")

;;; From:https://raw.github.com/rmm5t/dotfiles/master/emacs.d/rmm5t/global.el
;;; Generic emacs settings I cannot live without

;; Use command as the meta key; option key as super
(setq ns-command-modifier 'meta)
(setq ns-option-modifier  'super)

;; Don't show the startup screen
(setq inhibit-startup-message t)
;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight regions and add special behaviors to regions.
;; "C-h d transient" for more info
(setq transient-mark-mode t)
(pending-delete-mode t)

;; Display line and column numbers
(setq line-number-mode    t)
(setq column-number-mode  t)

;;; Line numbers please
(setq linum-format "%d ")
(global-linum-mode t)
;;; With some space after it

;;; Line wrap
(global-visual-line-mode t)

;;; Global tab-width
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; Modeline info
(display-time-mode 1)
;; (display-battery-mode 1)

;; Small fringes
(set-fringe-mode '(1 . 1))

;; Make sure all backup files only live in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Gotta see matching parens
(show-paren-mode t)

;; Don't truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Trailing whitespace is unnecessary
;; (defvar whitespace-cleanup-on-save t)
;; ;; (setq whitespace-cleanup-on-save nil)
;; (add-hook 'before-save-hook
;;     (lambda ()
;;     (if whitespace-cleanup-on-save (whitespace-cleanup))))

;; Trash can support
(setq delete-by-moving-to-trash t)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have

(defun reload-dotemacs-file ()
  "reload your .emacs file without restarting Emacs"
  (interactive)
  (load-file "~/.emacs.d/.emacs") )



;;; Do not prompt for killing buffers that were started with emacsclient
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;; Show server name and file path on window title!
(setq frame-title-format
    (list (format "%s %%S: %%j " (system-name))
  '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))


(defun json-format ()
  (interactive)
  (save-excursion
  (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))


;;;
(defun describe-last-function()
  (interactive)
  (describe-function last-command))

;;;
(require 'rinari)
(rinari-launch)

;;;
(require 's)
(require 'etags-select)

;;; For proper ruby tags support

(load "find-tags")

;; ;; Modify syntax entry
;; (defun hbin-ruby-mode-init ()
;;   (modify-syntax-entry ?? "w")
;;   (modify-syntax-entry ?! "w"))
;; (add-hook 'ruby-mode-hook 'hbin-ruby-mode-init)

;; (defun hbin-rhtml-mode-init ()
;;   (modify-syntax-entry ?? "w")
;;   (modify-syntax-entry ?! "w"))
;; (add-hook 'rhtml-mode-hook 'hbin-rhtml-mode-init)

;;; emacs Window switching shortcuts
(global-set-key [s-left] 'windmove-left)
(global-set-key [s-right] 'windmove-right)
(global-set-key [s-up] 'windmove-up)
(global-set-key [s-down] 'windmove-down)

;;; Indentation
(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;;; Using ibuffer instead of the usual buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")
;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; For string help
(require 'string-utils)

;;; Group by VC dirs
(add-hook 'ibuffer-hook
  (lambda ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic))))
;;; Show VC Status
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 30 30 :left :elide)
              ;" "
              ;(mode 16 16 :left :elide)
              ;" "
              ;(vc-status 16 16 :left)
              ;" "
              ;filename-and-process)))
)))
;              (string-utils-pad filename-and-process (- buffer-width 30) 'right))))
;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;;; Winner mode for undo redo of windo configurations
;; (when (fboundp 'winner-mode)
;;       (winner-mode 1))


(eval-after-load 'ruby-mode
  '(progn
     ;; Libraries
     (require 'flymake)

     ;; Invoke ruby with '-c' to get syntax checking
     (defun flymake-ruby-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "ruby" (list "-c" local-file))))

     (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

     (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)
           flymake-err-line-patterns)

     (add-hook 'ruby-mode-hook 'cabbage-flymake-init)

     (add-hook 'ruby-mode-hook
               (lambda ()
                 (when (and buffer-file-name
                            (file-writable-p
                             (file-name-directory buffer-file-name))
                            (file-writable-p buffer-file-name)
                            (if (fboundp 'tramp-list-remote-buffers)
                                (not (subsetp
                                      (list (current-buffer))
                                      (tramp-list-remote-buffers)))
                              t))
                   (local-set-key (kbd "C-c d")
                                  'flymake-display-err-menu-for-current-line)
                   (flymake-mode t))))))


(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c s v") 'set-variable)

;; Auto pairing parens and quotes
;; enable skeleton-pair insert globally
(setq skeleton-pair t)
;;(setq skeleton-pair-on-word t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\`") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "<") 'skeleton-pair-insert-maybe)

;; And deletion
(defvar skeletons-alist
      '((?\( . ?\))
        (?\' . ?\')
        (?\" . ?\")
        (?[  . ?])
        (?{  . ?})
        (?$  . ?$)))
(defadvice delete-backward-char (before delete-empty-pair activate)
  (if (eq (cdr (assq (char-before) skeletons-alist)) (char-after))
      (and (char-after) (delete-char 1))))
