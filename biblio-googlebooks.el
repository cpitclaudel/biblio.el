;;; biblio-googlebooks.el
;; This code is based on biblio-arxiv.el

(require 'biblio-core)

(defgroup biblio-googlebooks nil
  "Google Books support in biblio.el"
  :group 'biblio)

(defcustom biblio-googlebooks-bibtex-header "Book"
  "Which header to use for BibTeX entries generated from Google Books metadata."
  :group 'biblio
  :type 'string)

(defun biblio-googlebooks--forward-bibtex (metadata forward-to)
  "Forward BibTeX for Google Books entry METADATA to FORWARD-TO."
  (let-alist metadata
    (message "Forwarding bibtex.")
    (funcall forward-to (biblio-googlebooks--download-bibtex (car .references )))))

(defun biblio-googlebooks--download-bibtex (id)
  "Create a BibTeX record from Google Books for ID."
  (message "Downloading BibTex entry for %S." id)
  (shell-command-to-string (format "curl -s \"https://books.google.com/books\?id\=%s\&output\=bibtex\"" id)))

(defun biblio-googlebooks--extract-interesting-fields (item)
  "Prepare a Google Books search result ITEM for display."
  (message "Extracting interesting fields.")
  (let-alist item
    (list (cons 'doi .doi)
          (cons 'year (substring .volumeInfo.publishedDate 0 4))
          (cons 'title .volumeInfo.title)
          (cons 'authors (list "author"))
          (cons 'publisher .volumeInfo.publisher)
          (cons 'container .volumeInfo.printType)
          (cons 'references (list .id "isbn"))
          (cons 'type .volumeInfo.printType)
          (cons 'url .selfLink)
          (cons 'direct-url .selfLink)
          (cons 'open-access-status "access"))))

(defun biblio-googlebooks--parse-search-results ()
  "Extract search results from Google Books response."
  (message "Parsing search results.")
  (biblio-decode-url-buffer 'utf-8)
  (let-alist (json-read)
    (seq-map #'biblio-googlebooks--extract-interesting-fields .items)))

(defun biblio-googlebooks--url (query)
  "Create a Google books url to look up QUERY."
  (message "Querying Google Books.")
  (format "https://www.googleapis.com/books/v1/volumes\?q\=%s"
	  (url-encode-url (string-replace " " "+" query))))

;;;###autoload
(defun biblio-googlebooks-backend (command &optional arg &rest more)
  "A Google Books backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'."
  (pcase command
    (`name "Google Books")
    (`prompt "Google Books query: ")
    (`url (biblio-googlebooks--url arg))
    (`parse-buffer (biblio-googlebooks--parse-search-results))
    (`forward-bibtex (biblio-googlebooks--forward-bibtex arg (car more)))
    (`register (add-to-list 'biblio-backends #'biblio-googlebooks-backend))))

;;;###autoload
(add-hook 'biblio-init-hook #'biblio-googlebooks-backend)

;;;###autoload
(defun biblio-googlebooks-lookup (&optional query)
  "Start a Google Books search for QUERY, prompting if needed."
  (interactive)
  (biblio-lookup #'biblio-googlebooks-backend query))

;;;###autoload
(defalias 'googlebooks-lookup 'biblio-googlebooks-lookup)

(provide 'biblio-googlebooks)
;;; biblio-googlebooks.el ends here
