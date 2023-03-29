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
;;; M-x aps-status -- To get status of manuscript by entering accession code and author last name
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

(defcustom aps-status--input-history-file (if (file-exists-p (expand-file-name "~/.emacs.d"))
					"~/.emacs.d/aps-status-input-history.csv"
				      "~/.config/emacs/aps-status-input-history.csv")
  "Path to save used accession codes and author last names."
  :type 'string
  :group 'aps-status)

(defun aps-status--get-input-history-alist ()
  "Get the alist of input history."
  (when (file-exists-p aps-status--input-history-file)
    (let* ((hist-string (with-temp-buffer
			  (insert-file-contents-literally aps-status--input-history-file)
			  (split-string (buffer-string) "\n"))))
      (cl-loop for hist in hist-string
		    collect (split-string hist ",")))))

(defun aps-status-save-input-to-history (accession-code author-last-name accession-codes-from-history)
  "Save the ACCESSION-CODE and AUTHOR-LAST-NAME to history file.
Checks against ACCESSION-CODES-FROM-HISTORY to see if it already exists."
  (unless (member accession-code accession-codes-from-history)
    (with-current-buffer (find-file-noselect aps-status--input-history-file)
      (goto-char (point-max))
      (insert (format "%s,%s" accession-code author-last-name))
      (write-file aps-status--input-history-file))))

(defun aps-status (accession-code author-last-name)
  "Get status of APS manuscripts using ACCESSION-CODE and AUTHOR-LAST-NAME."
  (interactive
   (let* ((input-history-alist (aps-status--get-input-history-alist))
	  (accession-codes-from-history (mapcar #'car input-history-alist))
	  (accession-code (completing-read "Enter accession code: " accession-codes-from-history))
	  (author-last-name (read-from-minibuffer "Enter author last name: " (cdr (assoc accession-code input-history-alist)))))
     (aps-status-save-input-to-history accession-code author-last-name accession-codes-from-history)
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
