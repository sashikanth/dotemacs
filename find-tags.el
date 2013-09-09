(require 'thingatpt)

(defun thing-after-point ()
  "Things after point, including current symbol."
  (if (thing-at-point 'symbol)
      (save-excursion
        (let ((from (beginning-of-thing 'symbol))
              (to   (end-of-thing 'line)))
          (and (> to from)
               (buffer-substring-no-properties from to))))))

(defun ruby-thing-at-point ()
  "Get ruby thing at point.
   1. thing at 'current_user'   get current_user;
   2. thing at '!current_user'  get current_user;
   3. thing at 'current_user!'  get current_user!;
   4. thing at 'current_user='  get current_user=;
   5. thing at 'current_user =' get current_user=;
   6. thing at 'current_user ==' get current_user;
   7. thing at 'current_user ||=' get current_user=;
   Otherwise, get `find-tag-default symbol."
  (if (member (symbol-name major-mode)
              '("ruby-mode" "rhtml-mode" "haml-mode" "slim-mode"))
      (let ((symbol (thing-at-point 'symbol))
            (remain (thing-after-point)))
        (if (and symbol remain)
            (let ((sym (s-chop-prefixes '("!!" "!") symbol))
                  (rem (s-chop-prefixes '("!!" "!") remain)))
              (if (s-matches? (concat "^" sym "\\( *\\(||\\)?=[^=]\\)") rem)
                  (concat sym "=")
                sym))
          (find-tag-default)))
    (find-tag-default)))

(defun visit-project-tags ()
  (let ((tags-file (concat (eproject-root) "TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))

(defun hbin-build-ctags ()
  "Build ctags file at the root of current project."
  (interactive)
  (let ((root (eproject-root)))
    (shell-command
     (concat "ctags -e -R --extra=+fq "
             "--exclude=db --exclude=doc --exclude=log --exclude=tmp --exclude=.git --exclude=public "
             "-f " root "TAGS " root)))
  (visit-project-tags)
  (message "TAGS built successfully"))

(defun hbin-etags-find-tag ()
  "Borrow from http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/"
  (interactive)
  (if (file-exists-p (concat (eproject-root) "TAGS"))
      (visit-project-tags)
    (hbin-build-ctags))
  (etags-select-find (ruby-thing-at-point)))

(global-set-key (kbd "M-.") 'hbin-etags-find-tag)
