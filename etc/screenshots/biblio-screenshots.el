;;; biblio-screenshots.el --- Take a screenshot of biblio.el

;;; Commentary:

;;; Code:

(require 'biblio)
(require 'noflet)

(defconst biblio-screenshots--script-dir
  (file-name-directory
   (or (and load-in-progress load-file-name)
       (bound-and-true-p byte-compile-current-file)
       (buffer-file-name)))
  "Full path of this script.")

(defconst biblio-screenshots--fringe-width 8)

(defun biblio-screenshots--url-retrieve-synchronously (url)
  "Return a cached copy of results for URL."
  (with-current-buffer (get-buffer-create " *url*")
    (insert-file-contents-literally
     (expand-file-name
      (pcase url
        ("http://export.arxiv.org/api/query?search_query=higgs%20boson" "arxiv-higgs-boson")
        ("http://dissem.in/api/10.1016/j.physletb.2003.06.057" "dissemin-higgs-boson")
        ("http://doi.org/10.1016/j.physletb.2003.06.057" "doi-higgs-boson")
        (_ (error "Unexpected URL")))
      biblio-screenshots--script-dir))
    (current-buffer)))

(defun biblio-screenshots--make-emacs-pretty ()
  "Prettify Emacs."
  (redisplay t)
  (load-theme 'tango t)
  (set-face-attribute 'default nil :height 105)
  (set-face-attribute 'default nil :foreground "black")
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode)
  (fringe-mode (cons biblio-screenshots--fringe-width biblio-screenshots--fringe-width))
  (blink-cursor-mode -1)
  (setq-default cursor-type nil
                split-width-threshold 100
                mode-line-format '(" " mode-line-buffer-identification))
  (set-frame-size (selected-frame) 125 30))

(defun biblio-screenshots--save-screenshot ()
  "Save screenshot of current frame."
  (let ((fname (expand-file-name "biblio.el.png" biblio-screenshots--script-dir)))
    (process-lines "import" "-window" (frame-parameter nil 'outer-window-id)
                   fname)
    (process-lines "mogrify" "-strip" "-matte"
                   "-bordercolor" (face-attribute 'fringe :background)
                   "-border" (format "0x%d" biblio-screenshots--fringe-width) fname)
    (process-lines "optipng" "-o3" fname))
  (kill-emacs))

(defun biblio-screenshots--prepare ()
  "Prepare a screenshot."
  (biblio-screenshots--make-emacs-pretty)
  (delete-other-windows)
  (noflet ((url-retrieve-synchronously (url &rest _args)
                                       (biblio-screenshots--url-retrieve-synchronously url)))
    (let* ((biblio-synchronous t)
           (biblio-authors-limit 5)
           (source-buffer (get-buffer-create "bosons.bib"))
           (results-buffer (with-current-buffer source-buffer
                             (biblio--lookup-1 #'biblio-arxiv-backend "higgs boson")))
           (dissemin-buffer nil))
      (with-current-buffer source-buffer
        (bibtex-mode))
      (with-current-buffer results-buffer
        (dotimes (_ 2) (biblio--selection-next))
        (hl-line-highlight))
      (with-current-buffer results-buffer
        (setq dissemin-buffer (biblio-dissemin--lookup-record
                               (biblio--selection-metadata-at-point)))
        (biblio--selection-insert))
      (delete-other-windows)
      (let* ((results-window (selected-window))
             (dissemin-window (split-window-horizontally -62))
             (source-window (with-selected-window dissemin-window
                              (split-window-vertically))))
        (set-window-buffer source-window source-buffer)
        (set-window-buffer results-window results-buffer)
        (set-window-buffer dissemin-window dissemin-buffer)
        (with-selected-window source-window
          (goto-char (point-min))
          (setq truncate-lines t))))))

(defun biblio-screenshots--do ()
  "Prepare and take screenshot."
  (biblio-screenshots--prepare)
  (force-window-update)
  (redisplay t)
  (run-with-timer 1 nil #'biblio-screenshots--save-screenshot))

(provide 'biblio-screenshots)
;;; biblio-screenshots.el ends here