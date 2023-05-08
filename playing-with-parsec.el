(require 'parsec)

(defun parse-entry-type ()
  "Parse the entry type, e.g., @book"
  (parsec-and
    (parsec-ch ?@)
    (parsec-or
     (parsec-string "book")
     (parsec-string "article"))))

(defun parse-citation-key ()
  "Parse the citation key, e.g. hemingway1952old"
  (parsec-collect-as-string
   (parsec-many-as-string (parsec-letter))
   (parse-year)
   (parsec-many-as-string (parsec-letter))))

(defun parse-field-name ()
  "Parse a field name"
  (parsec-or
   (parsec-try
    (parsec-string "year"))
   (parsec-try
    (parsec-string "title"))
   (parsec-try
    (parsec-string "author"))))

(defun parse-year ()
  "Parse a year, e.g. 1952"
  (reverse (parsec-count-as-string 4 (parsec-digit))))

(defun parse-author-name ()
  "Parse an author name, e.g., Ernest Hemingway"
  (parsec-many-as-string
   (parsec-or
    (parsec-letter)
    (parsec-ch ? )
    (parsec-ch ?,))))

(defun parse-author-list ()
  "Parse a author names separated by the string \"and\""
  (parsec-collect*
   (parse-author-name)
   (parsec-many
    (parsec-and
      (parsec-string " and ")
      (parse-author-name)))))


(defun parse-title ()
  "Parse a title, e.g., The Old Man and the Sea"
  (parsec-many-as-string
   (parsec-or
    (parsec-letter)
    (parsec-ch ? )
    (parsec-between (parsec-ch ?{) (parsec-ch ?})
		    (parsec-letter)))))

(defun parse-value ()
  "Parse value"
  (parsec-or
   (parsec-try
    (parse-year))
   (parsec-try
    (parse-author-name))
   (parsec-try
    (parse-author-list))
   (parsec-try
    (parse-title))))

(defun parse-whitespace ()
  "Parse zero or more whitespace values"
  (parsec-many (parsec-or (parsec-ch ? ) (parsec-newline))))

(defun parse-key-value-pair ()
  "Parse key-value pair"
    (parsec-collect
     (parsec-and (parse-whitespace) (parse-field-name))
     (parsec-and (parse-whitespace) (parsec-ch ?=))
     (parsec-and (parse-whitespace)
		 (parsec-or
		  (parse-year)
		  (parsec-between (parsec-ch ?{) (parsec-ch ?})
				  (parse-value))))))

(defun parse-entry ()
  "Parse a BibTeX entry"
  (parsec-collect
   (parse-entry-type)
   (parsec-between (parsec-ch ?{) (parsec-ch ?})
		   (parsec-collect
		    (parse-citation-key)
		    (parsec-many
		     (parsec-and
		       (parsec-ch ?,)
		       (parse-whitespace)
		       (parse-key-value-pair)))))))

(defun parse-bibtex ()
  "Parse a BibTeX file"
  (parsec-collect*
   (parse-entry)
   (parsec-many
    (parsec-and
      (parsec-many (parsec-newline))
      (parse-entry)))))


;; ============================================
;; Here are some examples to test this out with
;; ============================================

(parsec-with-input
    "@book{hemingway1952old,  year =         1952,  title =        {The Old Man and the Sea},  author =       {Hemingway, Ernest}}"
  (parse-entry))

(parsec-with-input
    "@book{hesse1943glass,
  year =         1943,
  title =        {The Glass Bead Game},
author =       {Hesse, Hermann and Winston, Richard and Winston, Clara}}"
  (parse-entry))

(parsec-with-input
    "@book{hesse1943glass,
  year =         1943,
  title =        {The Glass Bead Game},
author =       {Hesse, Hermann and Winston, Richard and Winston, Clara}}


@book{hemingway1952old,  year =         1952,  title =        {The Old Man and the Sea},  author =       {Hemingway, Ernest}}"
  (parse-bibtex))
