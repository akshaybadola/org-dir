;;; org-directory.el --- Import and synchronize files from the the filesystem to org mode. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Mon Nov 18 02:03:01 IST 2019>
;; Keywords:	files, filesystem, org
;; Version:     0.1
;; Package-Requires: ((org "9.1.9") (f "0.20.0") (bind-key "2.4"))

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;; The code should be considered pre-alpha as a package but the functions are
;; usable


;;; Commentary:
;; None

;;; Code:

(require 'cl)
(require 'org)
(require 'seq)
(require 'subr-x)

;; CHECK: May create confusion with bibkey they should match later
(defvar nonascii-conversion-alist '(("í" . "i")
                                    ("æ" . "ae")
                                    ("ć" . "c")
                                    ("é" . "e")
                                    ("ä" . "ae")
                                    ("è" . "e")
                                    ("à" . "a")
                                    ("á" . "a")
                                    ("ø" . "oe")
                                    ("ë" . "\e")
                                    ("ü" . "ue")
                                    ("ñ" . "n")
                                    ("ņ" . "n")
                                    ("ñ" . "n")
                                    ("å" . "a")
                                    ("ö" . "oe")
                                    ("á" . "a")
                                    ("í" . "i")
                                    ("ó" . "o")
                                    ("ó" . "o")
                                    ("ú" . "u")
                                    ("ú" . "u")
                                    ("ý" . "y")
                                    ("š" . "s")
                                    ("č" . "c")
                                    ("ř" . "r")
                                    ("š" . "s")
                                    ("İ" . "i")
                                    ("ğ" . "g")
                                    ("δ" . "(delta)")
                                    ("ç" . "ch")
                                    ("θ" . "(theta)")
                                    ("μ" . "(mu)")
                                    ("×" . "x")
                                    ("°" . "(degree)")
                                    ("ş" . "sh")
                                    ("γ" . "(gamma)")
                                    ("ɣ" . "(gamma)")
                                    ("º" . "degc")
                                    ("η" . "(eta)")
                                    ("µ" . "(mu)")
                                    ("α" . "(alpha)")
                                    ("β" . "(beta)")
                                    ("ɛ" . "(epsilon)")
                                    ("ⅵ" . "vi")
                                    ("ⅲ" . "iii")
                                    ("ⅴ" . "v")
                                    ("λ" . "(lambda)")
                                    ("π" . "(pi)")
                                    ("∞" . "(infty)")
                                    ("χ" . "(chi)")
                                    ("∼" . "(tilde)")
                                    ("‑" . "-")
                                    (" " . "_")
                                    ("…" . "...")
                                    ("•" . ".")
                                    (" " . " ")
                                    (" " . " ")
                                    (" " . " ")
                                    ("–" . "-")
                                    ("−" . "-")
                                    ("–" . "-")
                                    ("—" . "-")
                                    ("‒" . "-")
                                    ("‘" . "")
                                    ("’" . "")
                                    ("’" . "")
                                    ("“" . "")
                                    ("’" . "")
                                    ("”" . "")))

;; TODO: Option of inserting and updating directories and files recursively
;;       upto some level
(defun org-directory-remove-files-from-heading ())

(defun org-directory-remove-dirs-from-heading ())

(defun org-directory--get-first-path-from-text ()
  "From text corresponding to an org heading, fetch first `file`
  link"
  (save-restriction
    (org-narrow-to-subtree)             ; THIS: is where it has to be fixed
    (car (org-element-map (org-element-parse-buffer) 'link
           (lambda (link)
             (when (string-match-p "^file" (org-element-property :type link)) link))))))

(defun org-directory--get-bounds (parse level)
  (mapcar (lambda (x)
            (when (eq (nth 1 x) level)
              (cons (first x) (last x))))
          (car (org-element-map parse 'plain-list
                 (lambda (x)
                   (org-element-property :structure x))))))

(defun org-directory--get-subtree-parse (buffer)
  (with-current-buffer buffer
    (save-restriction
      (org-narrow-to-subtree)
      (org-element-parse-buffer))))

(defun org-directory--get-first-path-from-bounds (buffer start end)
  (with-current-buffer buffer
    (save-restriction
      (narrow-to-region start end)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (x) (org-element-property :path x)) nil t))))

;; Can easily make this a hash table
(defun org-directory--get-paths-bounds (buf)
  (let* ((level
          (org-element-property :level
                                (car (with-current-buffer buf
                                       (save-restriction
                                         (org-narrow-to-subtree)
                                         (org-element-map (org-element-parse-buffer 'headline) 'headline
                                           (lambda (x) x)))))))
         (parse (org-directory--get-subtree-parse buf))
         (bounds (delq nil (org-directory--get-bounds parse (+ level 1))))
         (paths-bounds (mapcar
                        (lambda (x)
                          (cons (org-directory--get-first-path-from-bounds
                                 (get-buffer buf) (car x) (cadr x))
                                x))
                        bounds)))
    paths-bounds))

;; DONE: Don't insert file(s) if already inserted
;; TODO: Check for FULL file-name-sanity before deletion from buffer
(defun org-directory-insert-files-from-heading ()
  "Puts all the files in the directory there"
  (interactive)
  (save-excursion
    (let* ((path (org-element-property :path (org-directory--get-first-path-from-text)))
           (files-in-path (f-files path))
           (existing-paths-bounds (org-directory--get-paths-bounds (current-buffer)))
           (existing-files (mapcar 'car existing-paths-bounds))
           (additional-files (set-difference files-in-path existing-files :test 'string-equal))
           (deleted-files (set-difference existing-files files-in-path :test 'string-equal)))
      ;; TODO: Here it assumes the regions which will be deleted are sorted
      ;;       Have to add a check for that.
      ;; TODO: Notes if any should be stored in some other file when the
      ;;       entry is deleted
      (message (format "%s" existing-paths-bounds))
      (when deleted-files
        (seq-do (lambda (x)
                  (let ((start (cadr (assoc x existing-paths-bounds)))
                        (end (caddr (assoc x existing-paths-bounds))))
                    (delete-region start end)))
                (reverse deleted-files)))
      (when additional-files
        (when (org-at-heading-p)
          (forward-line))
        (while (not (or (org-at-heading-p) (eobp)))
          (forward-line))
        (open-line 1)
        (while (and (bolp) (eolp))
          (forward-line -1))
        (end-of-line) (newline)
        (indent-relative)
        (insert "- " "[[" (replace-regexp-in-string "\\[\\|\\]" "" (car additional-files)) "]["
                (replace-regexp-in-string "\\[\\|\\]" "" (file-name-nondirectory (car additional-files)))
                "]]" "\n")
        (seq-do (lambda (x)
                  (indent-relative)
                  (when (string-match-p "\\[\\|\\]" x)
                    (rename-file x (replace-regexp-in-string "\\[\\|\\]" "" x)))
                  (insert "- " "[[" (replace-regexp-in-string "\\[\\|\\]" "" x) "]["
                          (file-name-nondirectory
                           (replace-regexp-in-string "\\[\\|\\]" "" x))
                          "]]" "\n"))
                (cdr additional-files))
        (delete-blank-lines)))))

(defun org-directory--edit-heading-helper ()
  (let* ((heading (org-get-heading t t t t))
         (splits (split-string heading "/"))
         (split-length (length splits)))
    (when (> split-length 1)
      (org-edit-headline (car (last splits)))
      (loop for x from 1 to (- split-length 1)
            do (org-demote)))))

;; FIXME: This doesn't check for existing paths like the function for files
;; TODO: For large directories, find may be faster. See `ref-man--files-non-hidden'
(defun org-directory-insert-dirs-from-path (recurse)
  "Imports all the non-hidden directories from a path to the
current org buffer. Recurses directories, with a universal prefix
argument `\\[universal-argument]'"
  (interactive (list (if (equal current-prefix-arg '(4)) t nil)))
  (let ((path (expand-file-name (ido-read-directory-name "Directory: "))))
    (save-excursion
      (let ((dirs (f-directories path
                                 (lambda (x)
                                   (not (string-match-p "/\\." x)))
                                 recurse)))
        (when dirs
          (org-insert-heading)
          (insert (f-base path) "\n")
          (org-indent-line)
          (insert "[[" path "]]" "\n")
          (org-insert-subheading nil)
          (insert (string-remove-prefix "/"
                                        (replace-regexp-in-string path "" (car dirs))) "\n")
          (org-indent-line)
          (insert "[[" (car dirs) "]]" "\n")
          (seq-do (lambda (x)
                    (org-insert-heading nil)
                    (insert (string-remove-prefix "/" (replace-regexp-in-string path "" x)) "\n")
                    (org-indent-line)
                    (insert "[[" x "]]" "\n"))
                  (cdr dirs))
          ;; Now fix hierarchy demotions
          (outline-up-heading 1)
          (loop until (not (org-at-heading-p))
                do
                (outline-next-heading)
                (org-directory--edit-heading-helper)))))))

(defun org-directory-dirs-non-hidden (path)
  (f-entries path (lambda (x)
                    (and (f-directory-p x)
                         (not (string-match-p "/\\." x))))
             t))

;; FIXME: This doesn't check for existing paths like the function for files
;; FIXME: Don't insert if the subdirs are already inserted
;;        1. Narrow to subtree
;;        2. parse all headings
;;        3. if subdir not in headings, insert subdir
;;        4. Else recursively update (optional)
(defun org-directory-insert-subdirs-from-heading (&optional recurse)
  "Inserts all the subdirectories in the path. Does not recurse for now"
  (interactive)
  (save-excursion
    (let* ((path (org-element-property :path (org-directory--get-first-path-from-text)))
           (dirs (f-directories path)))
      (when dirs
        (org-insert-heading-respect-content)
        (org-demote)
        (insert (string-remove-prefix "/" (replace-regexp-in-string path "" (car dirs))) "\n")
        (org-indent-line)
        (insert "[[" (car dirs) "]]" "\n")
        (seq-do (lambda (x)
                  (org-insert-heading nil)
                  (insert (string-remove-prefix "/" (replace-regexp-in-string path "" x)) "\n")
                  (org-indent-line)
                  (insert "[[" x "]]" "\n"))
                (cdr dirs))))))

;; TODO: Incorportate function below, use `nonascii-conversion-alist'
(defun org-directory--sanitize-filename (str)
  (if (string-match-p "\\[\\|\\]" x)
      (replace-regexp-in-string "\\[\\|\\]" "" (sanitize-filename-exp (x)))
    (sanitize-filename-exp(x))))

(provide 'org-directory)

;;; org-directory.el ends here
