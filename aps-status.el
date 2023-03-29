;;; aps-status.el --- Get status of APS manuscript   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'dom)

(defun aps--fetch-status-data (accession-code author-last-name)
  "Get status data of APS manuscripts using ACCESSION-CODE and AUTHOR-LAST-NAME."
  (let* ((url (concat "https://authors.aps.org/Submissions/status?utf8=%E2%9C%93" (format "&accode=%s&author=%s&commit=Submit" accession-code author-last-name))))
    (with-current-buffer (url-retrieve-synchronously url)
      (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	     (status-dom (dom-by-class dom "details"))
	     (correspondence-dom (dom-by-class dom "listing_table"))
	     status-data
	     correspondence-data)
	(setq status-data (cl-loop for d1 in (dom-by-tag status-dom 'td)
				   for d2 in (dom-by-tag status-dom 'th)
				   collect (let ((status (string-trim (car (dom-strings d1))))
						 (status-type (string-trim (car (dom-strings d2)))))
					     (cons status-type status))))
	(setq correspondence-data (cdr (cl-loop for d in (dom-by-tag correspondence-dom 'tr)
						collect (mapcar #'(lambda (a) (car (dom-strings a))) (dom-by-tag d 'td)))))
	`(("status" . ,status-data)
	  ("correspondence" . ,correspondence-data))))))

(defvar aps-status--accession-code-history '()
  "History of accession codes.")

(defvar aps-status--author-last-name-history '()
  "History of author last names.")

(defun aps-status (accession-code author-last-name)
  "Get status of APS manuscripts using ACCESSION-CODE and AUTHOR-LAST-NAME."
  (interactive
   (let ((accession-code (completing-read "Enter accession code: " aps-status--accession-code-history nil nil))
	 (author-last-name (completing-read "Enter author last name: " aps-status--author-last-name-history nil nil)))
     (add-to-list 'aps-status--accession-code-history accession-code)
     (add-to-list 'aps-status--author-last-name-history author-last-name)
     (list accession-code author-last-name)))
  (let ((data (aps--fetch-status-data accession-code author-last-name))
	(buffer (get-buffer-create (format "*aps-status-%s-%s*" accession-code author-last-name))))
    (with-current-buffer buffer
      (let* ((status (cdr (assoc "status" data)))
	     (correspondence (cdr (assoc "correspondence" data))))
	(insert (string-join (cl-loop for d in status
				      collect (format "%s %s" (car d) (cdr d))) "\n"))
	(insert "\nCorrespondence:\n")
	(insert (string-join (cl-loop for d in correspondence
				      collect (string-join d " ")) "\n")))
      (read-only-mode)
      (switch-to-buffer-other-window buffer))))

(provide 'aps-status)
;;; aps-status.el ends here
