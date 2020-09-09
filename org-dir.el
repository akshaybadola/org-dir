;;; org-dir.el --- Import and synchronize files from the the filesystem to org mode. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Mon Nov 18 02:03:01 IST 2019>
;; Keywords:	files, filesystem, org
;; Version:     0.1

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
;; TODO

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)

(defvar org-dir-exclude-regexp
  "~$\\|^\\.")

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
(defun org-dir-remove-files-from-heading ()
  "Placeholder.")

(defun org-dir-remove-dirs-from-heading ()
  "Placeholder.")

(defun org-dir--get-first-path-from-text ()
  "From text corresponding to current org heading, fetch first 'file link."
  (save-restriction
    (org-narrow-to-subtree)             ; THIS: is where it has to be fixed
    (car (org-element-map (org-element-parse-buffer) 'link
           (lambda (link)
             (when (string-match-p "^file" (org-element-property :type link)) link))))))

(defun org-dir--get-bounds (parse level)
  ;; TODO: What does this exactly do?
  (mapcar (lambda (x)
            (when (eq (nth 1 x) level)
              (cons (car x) (last x))))
          (car (org-element-map parse 'plain-list
                 (lambda (x)
                   (org-element-property :structure x))))))

(defun org-dir--get-subtree-parse (buffer)
  ;; TODO: What does this exactly do?
  (with-current-buffer buffer
    (save-restriction
      (org-narrow-to-subtree)
      (org-element-parse-buffer))))

(defun org-dir--get-first-path-from-bounds (buffer start end)
  ;; TODO: What does this exactly do?
  (with-current-buffer buffer
    (save-restriction
      (narrow-to-region start end)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (x) (org-element-property :path x)) nil t))))

;; Can easily make this a hash table
(defun org-dir--get-paths-bounds (buf)
  ;; TODO: What does this exactly do?
  (let* ((level
          (org-element-property :level
                                (car (with-current-buffer buf
                                       (save-restriction
                                         (org-narrow-to-subtree)
                                         (org-element-map (org-element-parse-buffer 'headline) 'headline
                                           (lambda (x) x)))))))
         (parse (org-dir--get-subtree-parse buf))
         (bounds (delq nil (org-dir--get-bounds parse (+ level 1))))
         (paths-bounds (mapcar
                        (lambda (x)
                          (cons (org-dir--get-first-path-from-bounds
                                 (get-buffer buf) (car x) (cadr x))
                                x))
                        bounds)))
    paths-bounds))

;; Should just exclude directly
;; (defun org-dir-exclude-p (file)
;;   "Exclude files matching `org-dir-exclude-regexp'"
;;   (string-match-p org-dir-exclude-regexp))

;; DONE: Don't insert file(s) if already inserted
;; TODO: Check for FULL file-name-sanity before deletion from buffer
(defun org-dir-insert-files-from-heading ()
  "Puts all the files in the directory there."
  (interactive)
  (save-excursion
    (let* ((path (org-element-property :path (org-dir--get-first-path-from-text)))
           (files-in-path (f-files path))
           (existing-paths-bounds (org-dir--get-paths-bounds (current-buffer)))
           (existing-files (mapcar 'car existing-paths-bounds))
           (additional-files (cl-set-difference files-in-path existing-files :test 'string-equal))
           (additional-files (-remove (lambda (x)
                                        (string-match-p org-dir-exclude-regexp x))
                                      additional-files))
           (deleted-files (cl-set-difference existing-files files-in-path :test 'string-equal)))
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

(defun org-dir--demote-heading-helper ()
  "Demote the current heading according to depth from current path."
  (let* ((heading (org-get-heading t t t t))
         (splits (split-string heading "/"))
         (split-length (length splits)))
    (when (> split-length 1)
      (org-edit-headline (car (last splits)))
      (cl-loop for x from 1 to (- split-length 1)
               do (org-demote)))))

;; FIXME: This doesn't check for existing paths like the function for files
;; TODO: For large directories, find may be faster. See `ref-man--files-non-hidden'
(defun org-dir-insert-dirs-from-path (recurse)
  "Import all non-hidden directories from a path to the current org buffer.
With non-nil RECURSE or a universal prefix argument
`\\[universal-argument]', recurse directories also."
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
          (cl-loop until (not (org-at-heading-p))
                   do
                   (outline-next-heading)
                   (org-dir--demote-heading-helper)))))))

(defun org-dir-dirs-non-hidden (path)
  ;; FIXME: This is unused
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
(defun org-dir-insert-subdirs-from-heading (&optional recurse)
  "Insert all the subdirectories in the path.
With optional argument RECURSE, insert recursively.
Does not actually recurse for now."
  (interactive)
  (save-excursion
    (let* ((path (org-element-property :path (org-dir--get-first-path-from-text)))
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
(defun org-dir--sanitize-filename (str)
  "Return a filename with non-ascii characters removed."
  ;; CHECK: Maybe find some other way to get the links.
  (if (string-match-p "\\[\\|\\]" str)
      ;; NOTE: Maybe "exp" in `sanitize-filename-exp' means expression
      (replace-regexp-in-string "\\[\\|\\]" "" (sanitize-filename-exp str))
    (sanitize-filename-exp str)))

(provide 'org-dir)

;;; org-dir.el ends here
