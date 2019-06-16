;;; gen.el --- Generate README from library.bib -*- lexical-binding: t; -*-

;; Copyright (c) 2019 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26"))

;;; Commentary:

;; Generate README from library.bib. Here we keep functions used for generating
;; README.org for paper-reading repo here ->
;; https://github.com/Vernacular-ai/paper-reading
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ht)
(require 'org-ref)
(require 'parsebib)
(require 's)

(defcustom gen-bib-file "./library.bib"
  "BibTeX file to read from")

(defcustom gen-format '(("org"
                         ("article" . "${author}, [[${url}][${title}]], ${journal}, ${volume}(${number}), ${pages} (${year}). (cite:${=key=})")
                         ("inproceedings" . "${author}, [[${url}][${title}]], In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}. (cite:${=key=})")
                         ("book" . "${author}, [[${url}][${title}]] (${year}), ${address}: ${publisher}. (cite:${=key=})")
                         ("phdthesis" . "${author}, [[${url}][${title}]] (Doctoral dissertation) (${year}). ${school}, ${address}. (cite:${=key=})")
                         ("inbook" . "${author}, [[${url}][${title}]], In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}. (cite:${=key=})")
                         ("incollection" . "${author}, [[${url}][${title}]], In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}. (cite:${=key=})")
                         ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}. (cite:${=key=})")
                         ("unpublished" . "${author}, [[${url}][${title}]] (${year}). Unpublished manuscript. (cite:${=key=})")
                         (nil . "${author}, [[${url}][${title}]] (${year}). (cite:${=key=})")))
  "Format used for item in README.org")

(defun gen-parse-keys (text)
  "Return all entry keys from the text."
  (with-temp-buffer
    (insert text)
    (let ((entries (car (parsebib-parse-buffer))))
      ;; Minor verification
      (maphash (lambda (k v)
                 (unless (alist-get "url" v nil nil #'string=)
                   (error (format "No url found for %s" k))))
               entries)
      (ht-keys entries))))

(defun gen-parse-sections ()
  "Read BibTeX keys grouped in sections"
  (let ((sections)
        (prev-section)
        (current-section)
        (section-start))
    (with-current-buffer (find-file-noselect gen-bib-file)
      (goto-char (point-min))
      (while (re-search-forward "^% --- \\(.*\\)" nil t)
        (setq current-section (match-string-no-properties 1))
        (when prev-section
          (push (cons prev-section (gen-parse-keys (buffer-substring-no-properties section-start (point)))) sections))
        (setq section-start (line-beginning-position))
        (setq prev-section current-section))
      (push (cons prev-section (gen-parse-keys (buffer-substring-no-properties section-start (point-max)))) sections))
    (reverse sections)))

(defun gen-format-key (key)
  "Format a key to go as a single README entry."
  (let ((org-ref-default-bibliography gen-bib-file)
        (org-ref-formatted-citation-formats gen-format)
        (org-ref-formatted-citation-backend "org"))
    (format "- %s" (org-ref-format-entry key))))

(defun gen-format-section (section)
  "Format a complete section. Section is a cons coming from
gen-parse-sections function."
  (format "** %s\n%s"
          (car section)
          (s-join "\n\n" (mapcar #'gen-format-key (cdr section)))))

(defun gen-clear-headlines ()
  "Clear headlines from the current org buffer."
  (goto-char (point-min))
  (when (re-search-forward "^*" nil t)
    (delete-region (line-beginning-position) (point-max))))

(defun gen ()
  "Generate entries in README.org"
  (let* ((sections (gen-parse-sections))
         (section-texts (mapcar #'gen-format-section sections)))
    (with-current-buffer (find-file-noselect "README.org")
      (gen-clear-headlines)
      (goto-char (point-max))
      (insert (s-join "\n\n" section-texts))
      (save-buffer))))

(provide 'gen)

;;; gen.el ends here
