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
(defvar *bib.el-url-mapping* '(
  ("Ex." "EXO")
  ("Lev." "LEV")
  ("Num." "NUM")
  ("Deut." "DEU")
  ("Josh." "JOS")
  ("Judg." "JDG")
  ("Ruth" "RUT")
  ("1 Sam." "1SA")
  ("2 Sam." "2SA")
  ("1 Kings" "1KI")
  ("2 Kings" "2KI")
  ("1 Chron." "1CH")
  ("2 Chron." "2CH")
  ("Ezra" "EZR")
  ("Neh." "NEH")
  ("Est." "EST")
  ("Job" "JOB")
  ("Ps." "PSA")
  ("Prov." "PRO")
  ("Eccles." "ECC")
  ("Song" "SNG")
  ("Isa." "ISA")
  ("Jer." "JER")
  ("Lam." "LAM")
  ("Ezek." "EZK")
  ("Dan." "DAN")
  ("Hos." "HOS")
  ("Joel" "JOL")
  ("Amos" "AMO")
  ("Obad." "OBA")
  ("Jonah" "JON")
  ("Mic." "MIC")
  ("Nah." "NAM")
  ("Hab." "HAB")
  ("Zeph." "ZEP")
  ("Hag." "HAG")
  ("Zech." "ZEC")
  ("Mal." "MAL")
  ("Matt." "MAT")
  ("Mark" "MRK")
  ("Luke" "LUK")
  ("John" "JHN")
  ("Acts" "ACT")
  ("Rom." "ROM")
  ("1 Cor." "1CO")
  ("2 Cor." "2CO")
  ("Gal." "GAL")
  ("Eph." "EPH")
  ("Phil." "PHP")
  ("Col." "COL")
  ("1 Thess." "1TH")
  ("2 Thess." "2TH")
  ("1 Tim." "1TI")
  ("2 Tim." "2TI")
  ("Titus" "TIT")
  ("Philem." "PHM")
  ("Heb." "HEB")
  ("James" "JAS")
  ("1 Pet." "1PE")
  ("2 Pet." "2PE")
  ("1 John" "1JN")
  ("2 John" "2JN")
  ("3 John" "3JN")
  ("Jude" "JUD")
  ("Rev." "REV")
  ))
(defvar bib.el-bookmarks '())
(defvar *bib.el-bookmark-file* "~/.emacs.d/bib.el/ebookmarks")
(defcustom bib.el-version "t_kjv" "Bible table name." :type 'string :options '("t_kjv") :group 'bib.el)
(defcustom bib.el-sqlite-path "~/.emacs.d/bib.el/bible-sqlite.db" "Path to db." :type 'string :options '("~/.emacs.d/bib.el/bible-sqlite.db") :group 'bib.el)
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


(defun bib.el-save-bookmarks()
  "Save BIB.EL-BOOKMARKS in *BIB.EL-BOOKMARK-FILE*."
  (with-temp-buffer
    (if (file-exists-p *bib.el-bookmark-file*)
	(insert-file-contents *bib.el-bookmark-file*))
    (dolist (bk bib.el-bookmarks)
      (insert (format "%s:::%s\n" (plist-get bk 'position) (plist-get bk 'description))))
    (write-region (point-min) (point-max) *bib.el-bookmark-file*)))



(defun bib.el-read-in-bookmarks(&optional &key override)
  "Populate BIB.EL-BOOKMARKS from *BIB.EL-BOOKMARK-FILE*, if OVERRIDE clear out BIB.EL-BOOKMARKS first."
  (if override (setq bib.el-bookmarks '()))
  (with-temp-buffer
    (if (file-exists-p *bib.el-bookmark-file*)
	(insert-file-contents *bib.el-bookmark-file*))
    (while (not (eobp))
      (let (des pnt raw)
	(setq raw (string-split (buffer-substring-no-properties (point) (progn (forward-line 1) (point))) ":::"))
	(setq pnt (or (nth 0 raw) 0))
	(setq des (or (string-replace "\n" "" (nth 1 raw)) ""))
	(push (list 'position pnt 'description des) bib.el-bookmarks)))))

(defun bib.el-bookmark-position ()
  "Save a bookmark for current position for later."
  (interactive)
  (let ((bookmark-description (read-string "Description: ")))
    (push (list 'position (point) 'description bookmark-description) bib.el-bookmarks))
  (bib.el-save-bookmarks))


(defun bib.el-jump-to-bookmark ()
  "Jump to a bookmark position."
  (interactive)
  (let ((selected-bookmark "") (descriptions (mapcar #'(lambda (a) (plist-get a 'description)) bib.el-bookmarks)))
    (setq selected-bookmark (completing-read "Bookmark:" descriptions))
    (progn
      (switch-to-buffer bib.el-buffer-name)
      (with-current-buffer bib.el-buffer-name
	;; (org-overview)
	(goto-char 0)
	(goto-char (string-to-number (plist-get
		    (nth (cl-position selected-bookmark descriptions :test 'equal) bib.el-bookmarks)
		    'position)))))))

;; MAYBE a wrapper round dictionary?
;; TODO: add a function to get book, chapter and get cross reference links

(defun bib.el-play-audio ()
  "Pull up the selected book/chapter to read from bible.com."
  (interactive)
  (let ((raw-string "")
	(parsed-string "")
	(book-chapter '())
	(website-book "")
      (audio-url ""))
    (setq book-chapter (bib.el-get-book-chapter))
    (setq website-book (nth 1 (nth 0 (cl-remove-if-not #'(lambda (x) (string-equal (nth 0 x) (nth 0 book-chapter))) *bib.el-url-mapping*))))
    (setq raw-string (shell-command-to-string (format "curl https://www.bible.com/audio-bible/1/%s.%s.KJV | fold -s -w 200 | grep '\"contentUrl\"'"
						      website-book (nth 1 book-chapter))))
  (if (equal raw-string "")
      (message "Curl command result empty..."))
  (setq parsed-string (split-string raw-string "\"contentUrl\":\""))
  (dolist (str parsed-string)
    (progn
      (if (cl-search "https://audio-bible-cdn.youversionapi.com" str)
	  (setq audio-url (car (split-string str "\",\""))))))
  (if (not (equal audio-url ""))
      (async-shell-command (format "mpv %s" audio-url))
    (message "Audio Url did not parse correctly.."))))

(defun bib.el-get-book-chapter ()
  "Return list (BOOK? CHAPTER?) looking at current pointer line."
  (let (line res)
    (setq line (split-string (thing-at-point 'line t) ":"))
    (if (> (length line) 1)
	(progn
	  (setq res (list (nth 0 line) (nth 1 line)))
	  (catch 'my-early-return
    (when t
      (throw 'my-early-return res)))))))

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
  (bib.el-read-in-bookmarks :override t)
  (if (get-buffer bib.el-buffer-name)
      (progn
	(switch-to-buffer bib.el-buffer-name)
	(catch 'open-bib.el-return (when t
	(throw 'open-bib.el-return "Opening bib.el buffer...")))))
  (let (db)
    (setq db (sqlite-open bib.el-sqlite-path))
    (setq *bib.el-books*
	  (sqlite-execute db (format "select bi.\"order\", bi.title_short, t.c, t.b, t.v, t.t, bi.abbreviation, bi.category from book_info as bi join %s as t on t.b = bi.\"order\" order by \"order\";" bib.el-version)))
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
    (insert "#+TITLE: Bib.el\n")
    (dolist (c *bib.el-books*)
	(if (not (= cur-book (nth 0 c)))
	    (cond
	     ((= (nth 0 c) 1) (progn
				(insert "* Old Testament \n")
				(insert (format "** %s\n" (nth 1 c)))))
	     ((= (nth 0 c) 40) (progn
				 (insert "* New Testament \n")
				 (insert (format "** %s \n" (nth 1 c)))))
	     (t (insert (format "** %s :%s:\n" (nth 1 c) (nth 7 c))))))
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
