;;; biblio-zotero.el --- Retrieve BibTeX entries from various sources --*- lexical-binding: t; -*-

;; Copyright (C) 2022 Grzegorz Kowzan

;; Author: Grzegorz Kowzan <grzegorz@kowzan.eu>
;; Maintainer: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; URL: https://github.com/cpitclaudel/biblio.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Retrieve and insert BibTeX records from various sources by using
;; `zotero-insert'. Information is retrieved from an URL by asking a Zotero
;; translation server to extract bibliographic data from the location.
;;
;; This package is part of the more general `biblio' package (which see for more
;; documentation).

(require 'biblio-core)

;;; Code:

(defcustom biblio-zotero-translation-server "http://127.0.0.1:1969"
  "The address of Zotero translation server."
  :group 'biblio
  :type 'string)

(defvar biblio-zotero-debug nil
  "If not nil, save responses from zotero translation server to
`biblio-zotero-debug-buffer'.")

(defconst biblio-zotero-debug-buffer "*biblio-zotero-debug-buffer*")

(defun biblio-zotero-url-retrieve (url callback)
  "Wrapper around `url-retrieve-synchronously' and `url-retrieve'.
Retrieve URL and call CALLBACK in result buffer. See `url-retrieve'.

This does not use `url-queue-retrieve' for asynchronous operation
contrary to `biblio-url-retrieve', because we will need to submit
custom headers to the server, which `url-queue-retrieve' does not
support."
  (message "Fetching %s" url)
  (if biblio-synchronous
      (with-current-buffer (url-retrieve-synchronously url)
	(funcall callback nil))
    (url-retrieve url callback)))

(defun biblio-zotero-generic-callback (callback)
  "Return callback function that ignores errors and logs the response.

Calls CALLBACK if no errors occur and saves the response to
`biblio-zotero-debug-buffer' if `biblio-zotero-debug' is not
nil."
  (biblio-generic-url-callback
   (lambda (&optional errors)
     (unless errors
       (when biblio-zotero-debug
	 (let ((debug-output (buffer-string)))
	   (with-current-buffer (get-buffer-create biblio-zotero-debug-buffer)
	     (insert debug-output "\n"))))
       (funcall callback)))))

(defun biblio-zotero-translate-callback (translate-url callback)
  "Return callback forwarding Zotero json entry to Zotero export service.

Calls CALLBACK in a buffer with a BibTeX entry."
  (lambda ()
    (let ((url-request-method "POST")
	  (url-request-extra-headers '(("Content-Type" . "application/json")))
	  (url-request-data (buffer-string)))
      (biblio-zotero-url-retrieve
       translate-url
       (biblio-zotero-generic-callback
	(lambda ()
	  (set-buffer-multibyte t)
	  (decode-coding-region (point) (point-max) 'utf-8 nil)
	  (funcall callback)))))))

(defun biblio-zotero-call-with-json (url callback)
  "Execute CALLBACK in a buffer with json metadata for URL."
  (let ((url-request-method "POST")
	(url-request-extra-headers '(("Content-Type" . "text/plain")))
	(url-request-data url))
    (biblio-zotero-url-retrieve
     (format "%s/web" biblio-zotero-translation-server)
     (biblio-zotero-generic-callback callback))))

(defun biblio-zotero-call-with-bibtex (url callback &optional export-format)
    "Execute CALLBACK in a buffer with BibTeX entry for URL.

EXPORT-FORMAT is either \"bibtex\" or \"biblatex\"."
    (let* ((export-format (or export-format
			      (downcase (symbol-name (intern-soft bibtex-dialect)))))
	   (translate-url (format "%s/export?format=%s"
				  biblio-zotero-translation-server export-format)))
      (biblio-zotero-call-with-json
       url (biblio-zotero-translate-callback translate-url callback))))

(defun biblio-zotero-forward-json (url forward-to)
  "Pass json metadata for URL to FORWARD-TO."
  (biblio-zotero-call-with-json
   url
   (lambda () (funcall forward-to (buffer-string)))))

(defun biblio-zotero-forward-bibtex (url forward-to)
  "Pass BibTeX entry for URL to FORWARD-TO."
  (biblio-zotero-call-with-bibtex
   url
   (lambda () (funcall forward-to (buffer-string)))))

;;;###autoload
(defun biblio-zotero-insert-bibtex (url)
  "Insert BibTeX entry returned by Zotero translation server for URL."
  (interactive "MURL: ")
  (let ((target-buffer (current-buffer)))
    (biblio-zotero-forward-bibtex
     url
     (lambda (result)
       (with-current-buffer target-buffer
	 (insert (biblio-format-bibtex result biblio-bibtex-use-autokey) "\n\n"))))))

(defalias 'zotero-insert 'biblio-zotero-insert-bibtex)

(provide 'biblio-zotero)
;;; biblio-zotero.el ends here
