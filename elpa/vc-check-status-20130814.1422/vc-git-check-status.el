;;; vc-git-check-status.el --- Git checking functions

;; Copyright (C) 2012-2013 Sylvain Rousseau <thisirs at gmail dot com>

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Maintainer: Sylvain Rousseau <thisirs at gmail dot com>
;; URL: https://github.com/thisirs/vc-check-status.git
;; Keywords: vc, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'vc-git)

(defconst vc-git-sym-name
  '((unpushed . "unpushed commits")
    (dirty . "changes or untracked files")
    (dirty-ignore-submodule . "changes or untracked files")
    (changes . "changes")
    (stash . "stashed changes")
    (untracked . "untracked files"))
  "Alist of symbols and corresponding description.")

(defun vc-git-check-dirty-p ()
  "Return t if local repository is dirty."
  (with-temp-buffer
    (vc-git-command t 0 nil "status" "--porcelain")
    (> (buffer-size) 0)))

(defun vc-git-check-dirty-ignore-submodule-p ()
  "Return t if local repository is dirty. Changes in submodules
are ignored."
  (with-temp-buffer
    (vc-git-command t 0 nil "status" "--porcelain" "--ignore-submodules")
    (> (buffer-size) 0)))

(defun vc-git-check-changes-p ()
  "Return t if local repository is changed.
Untracked files are ignored."
  (with-temp-buffer
    (vc-git-command t 0 nil "status" "--porcelain")
    (goto-char 1)
    (flush-lines "^\\?\\?")
    (> (buffer-size) 0)))

(defun vc-git-check-untracked-p ()
  "Return t if local repository has untracked files."
  (with-temp-buffer
    (vc-git-command t 0 nil "status" "--porcelain")
    (goto-char 1)
    (keep-lines "^\\?\\?")
    (> (buffer-size) 0)))

(defun vc-git-check-unpushed-p ()
  "Return t if local repository has some commit on some branch
not pushed yet. It first checks if there is any remote repository
and then lists local commits that are not remote ones."
  (and (with-temp-buffer
         (vc-git-command t 0 nil "remote" "show")
         (> (buffer-size) 0))
       (with-temp-buffer
         (vc-git-command t 0 nil "log" "--branches" "--not" "--remotes" )
         (> (buffer-size) 0))))

(defun vc-git-check-unpushed-current-p ()
  "Return non-nil if local repository has some commit on current
branch not pushed yet."
  (and (with-temp-buffer
         (vc-git-command t 0 nil "remote" "show")
         (> (buffer-size) 0))
       (with-temp-buffer
         (vc-git-command t 0 nil "log" "--not" "--remotes" )
         (> (buffer-size) 0))))

(defun vc-git-check-stash-p ()
  "Return t if local repository has changes stashed."
  (with-temp-buffer
    (eq 0 (vc-git-command t 1 nil "stash" "show"))))

(provide 'vc-git-check-status)

;;; vc-git-check-status.el ends here
