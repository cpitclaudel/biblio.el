;;; biblio-iacr.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Lorenzo Martinico

;; Author: Lorenzo Martinico <lzmartinico@gmail.com>
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
;; This file implements a backend for the for the `biblio' package (which see for more
;; documentation).
;;
;;; Code:

(require 'biblio-core)

(defun biblio-iacr--forward-bibtex (metadata forward-to)
  "Forward BibTeX for IACR entry METADATA to FORWARD-TO."
  (funcall forward-to
           (format "@misc{cryptoeprint:%s,
    author = {%s},
    title = {%s},
    howpublished = {%s},
    year = {%s},
    note = {\\url{%s}},
}"
                   (biblio-alist-get 'eprintid metadata)
                   (car (biblio-alist-get 'authors metadata))
                   (biblio-alist-get 'title metadata)
                   (biblio-alist-get 'container metadata)
                   (biblio-alist-get 'year metadata)
                   (biblio-alist-get 'url metadata))))

(defun biblio-iacr--extract-interesting-fields (header body)
  "Prepare a IACR search result, composed of HEADER and BODY, for display.
HEADER contains the technical report id, which encodes the year of publication.
BODY includes the Title and Authors."
  (pcase-let ((`(dt _ _ (a _ ,eprintid) _ ) header)
              (`(dd _ (_ _ ,title) _ (dd _ (_ _ ,authors))) body))
    (list (cons 'year (substring eprintid 0 4))
          (cons 'title title)
          (cons 'authors authors)
          (cons 'container (format "Cryptology ePrint Archive, Report %s" eprintid))
          (cons 'type "eprint")
          (cons 'references nil)
          (cons 'eprintid eprintid)
          (cons 'url (format "https://eprint.iacr.org/%s.pdf" eprintid)))))

(defun biblio-iacr--traverse-results (list)
  "Traverse the IACR html nodes LIST to produce a list of results recursively."
  (let ((entries nil))
    (while list
      (push (biblio-iacr--extract-interesting-fields (car list) (cadr list)) entries )
      (setq list (cddr list)))
    (nreverse entries)))

(defun biblio-iacr--hitp (item tag)
  "Check if ITEM from IACR response has html TAG."
  (eq (car-safe item) tag))

(defun biblio-iacr--parse-search-results ()
  "Extract search results from IACR html response."
  (biblio-decode-url-buffer 'utf-8)
  (biblio-iacr--traverse-results (cddar (seq-filter (lambda (x) (biblio-iacr--hitp x 'dl))
                                                    (cdar (seq-filter (lambda (x) (biblio-iacr--hitp x 'body))
                                                                      (libxml-parse-html-region (point-min) (point-max))))))))
(defun biblio-iacr--url (query)
  "Create a IACR url to look up QUERY."
  (format "https://eprint.iacr.org/eprint-bin/search.pl?anywords=%s" (url-encode-url query)))

(defun biblio-iacr-backend (command &optional arg &rest more)
  "An IACR backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends.el'."
  (pcase command
    (`name "IACR")
    (`prompt "IACR query: ")
    (`url (biblio-iacr--url arg))
    (`parse-buffer (biblio-iacr--parse-search-results))
    (`forward-bibtex (biblio-iacr--forward-bibtex arg (car more)))
    (`register (add-to-list 'biblio-backends #'biblio-iacr-backend))))

;;;###autoload
(add-hook 'biblio-init-hook #'biblio-iacr-backend)

(provide 'biblio-iacr)
;;; biblio-iacr.el ends here
