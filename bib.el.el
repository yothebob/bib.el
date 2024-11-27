;;; bib.el.el --- An emacs interface for reading the bible  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Brandon Brodrick

;; Author: Brandon Brodrick <bbrodrick@parthenonsoftware.com>
;; Keywords: hypermedia

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

;;; Code:

(defgroup bib.el '() "Bib.El Customization.")
(defvar *bib.el-books* (list))
(defvar *bib.el-file* "~/.emacs.d/bib.el/*Bib.El*")
(defvar bib.el-bookmarks '())
(defvar *bib.el-bookmark-file* "~/.emacs.d/bib.el/ebookmarks")
(defcustom bib.el-version "t_kjv" "Bible table name." :type 'string :options '("t_kjv") :group 'bib.el)
(defcustom bib.el-sqlite-path "~/Downloads/bible_databases/bible-sqlite.db" "Path to db." :type 'string :options '("~/Downloads/bible_databases/bible-sqlite.db") :group 'bib.el)
(defcustom bib.el-buffer-name "*Bib.El*" "Buffer name." :type 'string :options '("*Bib.El*") :group 'bib.el)

(global-set-key (kbd "C-c e b") 'bib.el-open)
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Caladea" :height 190 :weight normal))))
 '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160)))))

(defun bib.el--isearch-to-position (book-ch-v-list)
  "Take BOOK-CH-V-LIST (Book Chapter Verse) list and jump to that position."
  (if (get-buffer bib.el-buffer-name)
      (progn
	(switch-to-buffer bib.el-buffer-name)
	(with-current-buffer bib.el-buffer-name
	  (goto-char 0)
	  (isearch-mode t)
	  (isearch-yank-string (reduce #'(lambda (acc v) (setq acc (concat acc ":" (format "%s" v)))) book-ch-v-list))
	  (isearch-done)
	  (beginning-of-line)))
    (message "Bib.El buffer is not open...")))

(defun bib.el-goto-section ()
  "Go to a section based off input Book, Chapter, Verse."
  (interactive)
  (let ((db (sqlite-open bib.el-sqlite-path))
      (goto-result '())
      (selection '())
      (result "Luke")
      (prompt-query '(("Book: " "select abbreviation from book_info order by \"order\";")
		      ("Chapter: " "select distinct cast(t.c as text) from book_info as bi join t_kjv as t on t.b = bi.\"order\" where bi.abbreviation = '%s';")
		      ("Verse: " "select distinct cast(t.v as text) from book_info as bi join t_kjv as t on t.b = bi.\"order\" where bi.abbreviation = '%s';"))))
  (while (and (not (equal result "")) (> (length prompt-query) (length goto-result)))
    (if (string-match-p "%s" (nth 1 (nth (length goto-result) prompt-query)))
	(setq selection (apply #'append (sqlite-execute db (format (nth 1 (nth (length goto-result) prompt-query)) (nth 0 goto-result)))))
      (setq selection (apply #'append (sqlite-execute db (nth 1 (nth (length goto-result) prompt-query))))))
    (setq result (completing-read
		  (nth 0 (nth (length goto-result) prompt-query))
		  selection))
    (setq goto-result (apply #'append (list goto-result (list result)))))
  (bib.el--isearch-to-position goto-result)))


(defun bib.el-bookmark-position () ;; TODO: Bookmark a position, and be able to select it and jump to spot
  "Save a bookmark for current position for later."
  (interactive)
  (let ((bookmark-description (read-string "Description: ")))
    (push (list 'position 1 'description bookmark-description) bib.el-bookmarks)
    )
  ;; get the line (book chapter verse) from the beginning of cursor line
  ;; get a description
  ;; save to bookmark file
  )

(defun bib.el-jump-to-bookmark () ;; TODO: Bookmark a position, and be able to select it and jump to spot
  "Jump to a bookmark position."
  (interactive)
  (message (cl-reduce #'(lambda (acc v) (setq acc (concat acc ":" (format "%s" (plist-get v 'description))))) bib.el-bookmarks))
  (plist-get (nth 0 bib.el-bookmarks) 'description)
  ;; read in bookmark file
  ;; allow user to select from options
  ;; isearch to section
  )

;; MAYBE a wrapper round dictionary?

;; TODO: custom integration to stream autobook from bible.com
;; TODO: add key_genre_english join, give each book its genre
;; TODO: add a function to get book, chapter and get cross reference links

(defun bib.el-modes ()
  "Enable Bib.El modes on bible buffer."
  (with-current-buffer bib.el-buffer-name
  (org-mode)
  (org-overview)
  (variable-pitch-mode)
  (visual-line-mode)
  (display-line-numbers-mode 0)))

(defun bib.el-open ()
  "Open or create BIB.EL-BUFFER-NAME buffer."
  (interactive)
  (if (get-buffer bib.el-buffer-name)
      (progn
	(switch-to-buffer bib.el-buffer-name)
	(catch 'open-bib.el-return (when t
	(throw 'open-bib.el-return "Opening bib.el buffer...")))))
  (let (db)
    (setq db (sqlite-open bib.el-sqlite-path))
    (setq *bib.el-books*
	  (sqlite-execute db (format "select bi.\"order\", bi.title_short, t.c, t.b, t.v, t.t, bi.abbreviation from book_info as bi join %s as t on t.b = bi.\"order\" order by \"order\";" bib.el-version)))
    (if (file-exists-p *bib.el-file*)
	(progn
	  (find-file *bib.el-file*)
	  (bib.el-modes)
	  (catch 'open-bib.el-return (when t
	    (throw 'open-bib.el-return "Opening bib.el from stashed file...")))))
    (if (= (buffer-size (get-buffer-create bib.el-buffer-name)) 0)
	(progn
	(with-current-buffer bib.el-buffer-name
	  (bib.el--insert-create-new))
	(bib.el-modes)))
    (switch-to-buffer bib.el-buffer-name)))

(defun bib.el--insert-create-new ()
  "Bib.El helper function for inserting into bib.el buffer from scratch."
  (let (cur-book)
    (setq cur-book 0)
    (insert "#+TITLE: Bib.El\n")
    (dolist (c *bib.el-books*)
	(if (not (= cur-book (nth 0 c)))
	    (cond
	     ((= (nth 0 c) 1) (progn
				(insert "* Old Testament \n")
				(insert (format "** %s\n" (nth 1 c)))))
	     ((= (nth 0 c) 40) (progn
				 (insert "* New Testament \n")
				 (insert (format "** %s\n" (nth 1 c)))))
	     
	     (t (insert (format "** %s\n" (nth 1 c))))))
	(insert (format "%s:%s:%s %s\n" (nth 6 c) (nth 2 c) (nth 4 c) (nth 5 c)))
	(setq cur-book (nth 0 c)))))


;; (defun bib.el-get-random-verse()
;;   "Get a random verse message."
;; maybe just from sql?
;;   (interactive)
;;   ((random (count-lines (point-min) (point-max))))
;;   (count-lines (point-min) (point-max))
;;   )


(provide 'bib.el)
;;; bib.el.el ends here
