;;; Nicemacs.v2 -*- lexical-binding: t -*-
;;; ======================================
;;
;; To update your packages carry out the following steps:
;;
;; 1. M-x list-packages
;; 2. Press `r` to refresh
;; 3. Press `U` to mark upgradable packages
;; 4. Press `x` to execute the upgrades
;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives  '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)

(setq user-full-name "Alexander E. Zarebski")

(defvar nice-journal-directory "~/Documents/journal"
  "The directory for nicemacs journal files.")
(defvar nice-notes-directory "~/public-site/org/notes"
  "The directory for nicemacs notes files.")
(defvar nice-resources-dir "~/Documents/nicemacs/resources"
  "The path to the nicemacs resources directory on this machine.")
(defvar nice-nicemacs-directory "~/Documents/nicemacs"
  "The path to the nicemacs directory on this machine.")

(require 'cl-lib)

;; Be evil
;; -------

(setq evil-want-keybinding ())

(require 'evil)
(require 'evil-leader)
(require 'evil-collection)

(evil-mode 1)
(evil-leader-mode 1)
(global-evil-leader-mode 1)
(evil-collection-init)

;; Look stunning
;; =============

(require 'hl-todo)
(global-hl-todo-mode)

(setq nice-colours-alist
      '((strong-warning . "red")
	(weak-warning . "magenta")
	(weak-note . "cyan")
	(strong-note . "blue")
	(light-theme-comment-background . "#e4ecda")
	(light-theme-comment-foreground . "#207e7b")
	(light-theme-shadow-background . "#eee8d5")
	(light-theme-shadow-foreground . "#93a1a1")
	(dark-theme-comment-background . "#207e7b")
	(dark-theme-comment-foreground . "#e4ecda")
	(dark-theme-shadow-background . "#202c2a")
	(dark-theme-shadow-foreground . "#254d48")))

(defun nice-colour (colour)
  "Return the colour associated with the symbol COLOUR."
  (cdr (assoc colour nice-colours-alist)))

(setq hl-todo-keyword-faces
      `(("TODO"   . ,(nice-colour 'strong-warning))
        ("FIXME"  . ,(nice-colour 'weak-warning))
        ("NOTE"   . ,(nice-colour 'weak-note))
        ("DONE"   . ,(nice-colour 'strong-note))))

(setq fill-column 70)

(evil-leader/set-key "t l" 'display-fill-column-indicator-mode)

(add-to-list `custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'solarized-light-high-contrast t)

(defun nice-modeline-dark-theme ()
  (interactive)
  (set-face-background 'mode-line (nice-colour 'dark-theme-comment-background))
  (set-face-foreground 'mode-line (nice-colour 'dark-theme-comment-foreground))
  (set-face-background 'mode-line-inactive (nice-colour 'dark-theme-shadow-background))
  (set-face-foreground 'mode-line-inactive (nice-colour 'dark-theme-shadow-foreground)))

(defun nice-modeline-light-theme ()
  (interactive)
  (set-face-background 'mode-line (nice-colour 'light-theme-comment-background))
  (set-face-foreground 'mode-line (nice-colour 'light-theme-comment-foreground))
  (set-face-background 'mode-line-inactive (nice-colour 'light-theme-shadow-background))
  (set-face-foreground 'mode-line-inactive (nice-colour 'light-theme-shadow-foreground)))


(defun nice-toggle-themes ()
  "Toggle between two themes: solarized-light-high-contrast and
solarized-dark-high-contrast and adjust the comment face to one
that is visible in both."
  (interactive)
  (if (eq (car custom-enabled-themes) 'solarized-light-high-contrast)
      (progn
        (disable-theme 'solarized-light-high-contrast)
        (load-theme 'solarized-dark-high-contrast t)
	(setq font-lock-comment-delimiter-face
	      `((t (
		    :background ,(nice-colour 'dark-theme-comment-background)
  				:foreground ,(nice-colour 'dark-theme-comment-foreground)
				:slant normal))))
	(setq font-lock-comment-face
	      `((t (
		    :background ,(nice-colour 'dark-theme-comment-background)
				:foreground ,(nice-colour 'dark-theme-comment-foreground)
				:slant normal))))
	(nice-modeline-dark-theme))
    (progn
      (disable-theme 'solarized-dark-high-contrast)
      (load-theme 'solarized-light-high-contrast t)
      (setq font-lock-comment-delimiter-face
	    `((t (
		  :background ,(nice-colour 'light-theme-comment-background)
			      :foreground ,(nice-colour 'light-theme-comment-foreground)
			      :slant normal))))
      (setq font-lock-comment-face
	    `((t (
		  :background ,(nice-colour 'light-theme-comment-background)
			      :foreground ,(nice-colour 'light-theme-comment-foreground)
			      :slant normal)))))
    (nice-modeline-light-theme)))

(evil-leader/set-key "t t" 'nice-toggle-themes)

(tool-bar-mode -1)

(global-hl-line-mode t)
(set-face-attribute 'hl-line nil :background "#eee8d5")

(defun next-window-and-pulse ()
  "Switch to another window and pulse the current window."
  (interactive)
  (other-window 1)
  (let ((orig-color (face-background 'mode-line)))
    (set-face-background 'mode-line "#dc322f")
    (sit-for 0.1)
    (set-face-background 'mode-line orig-color)))

(defun previous-window-and-pulse ()
  "Switch to another window and pulse the current window."
  (interactive)
  (other-window -1)
  (let ((orig-color (face-background 'mode-line)))
    (set-face-background 'mode-line "#dc322f")
    (sit-for 0.1)
    (set-face-background 'mode-line orig-color)))

;; Rainbow-mode will highlight strings indicating colours,
;; e.g. hexcodes in their corresponding colour.
(require 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

(setq inhibit-splash-screen t)

(evil-leader/set-key "z j" 'text-scale-decrease)
(evil-leader/set-key "z k" 'text-scale-increase)

;; Be sensible
;; ===========

(global-evil-leader-mode)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "<SPC>" 'execute-extended-command)

(evil-leader/set-key "q r" 'restart-emacs)
(evil-leader/set-key "q q" 'save-buffers-kill-emacs)

;; The which-key package is a great way to be reminded of what keys
;; are available from the start of a key sequence.
(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.6)

(defmacro nice-meld-files (name fa fb key)
  "Generate function named nice-meld-NAME which opens meld diff for
files FA and FB using SPC f m KEY."
  `(progn
     (defun ,(intern (format "nice-meld-%s" name)) ()
       (interactive)
       (async-shell-command ,(format "meld %s %s &" fa fb)))
     (evil-leader/set-key ,(concat "f m " key) (intern ,(format "nice-meld-%s" name)))))

(nice-meld-files "init" "~/.emacs.d/init.el" "~/Documents/nicemacs/nicemacs-v2.el" "i")
(nice-meld-files "aspell" "~/.aspell.en.pws" "~/Documents/nicemacs/resources/aspell.en.pws" "a")

;; Shell stuff
;; -----------

(setq eshell-cmpl-ignore-case t)
(evil-leader/set-key "s e" 'eshell)
(evil-leader/set-key "s b" (lambda () (interactive) (ansi-term "/bin/bash")))
(evil-leader/set-key "s i" 'ielm)
(evil-leader/set-key "s r" 'R)

;; Buffer stuff
;; ------------

;; TODO Configure functions to print the put the full path of the
;; current buffer's file to the kill ring

;; TODO Configure functions to tell me what the link at the point is
;; pointing to both as a message and by putting the full path on the
;; kill ring.

(evil-leader/set-key "b r" 'revert-buffer)

;; File stuff
;; ----------

;; TODO Configure functions to move/copy the most recent file in the
;; ~/Downloads directory to the current directory so that they can be
;; used from eshell.

(evil-leader/set-key "f f" 'find-file)
(evil-leader/set-key "f s" 'save-buffer)

(require 'dired)
(evil-leader/set-key "f d" 'dired)
(define-key dired-mode-map "-" 'dired-up-directory)
(setq dired-listing-switches "-alh")

(evil-leader/set-key "b b" 'switch-to-buffer)
(evil-leader/set-key "b d" 'kill-buffer)
(evil-leader/set-key "b s" 'scratch-buffer)

(evil-leader/set-key "w s" 'split-window-below)
(evil-leader/set-key "w v" 'split-window-right)
(evil-leader/set-key "TAB" 'next-window-and-pulse)
(evil-leader/set-key "<backtab>" 'previous-window-and-pulse)
(evil-leader/set-key "w d" 'delete-window)
(evil-leader/set-key "w L" 'evil-window-move-far-right)
(evil-leader/set-key "w H" 'evil-window-move-far-left)
(evil-leader/set-key "w J" 'evil-window-move-very-bottom)
(evil-leader/set-key "w K" 'evil-window-move-very-top)

;; Consult the oracle
;; ------------------

(evil-leader/set-key "h s" 'apropos)
(evil-leader/set-key "h d f" 'describe-function)
(evil-leader/set-key "h d m" 'describe-mode)
(evil-leader/set-key "h d k" 'describe-key)
(evil-leader/set-key "h d v" 'describe-variable)

;; Learn from your past
;; --------------------

(defmacro nice-rgrep-directory (dname path pattern key)
  "Create a function that calls `rgrep` on the specified DIRECTORY
and binds it to a KEY.

DNAME is the name of the directory used to generate the function
name.
PATH is the path to the directory to be searched.
KEY is the keybinding (as a string) to trigger the rgrep function."
  `(progn
     (defun ,(intern (format "nice-rgrep-%s" dname)) ()
       ,(format "Search for a string in %s using rgrep." dname)
       (interactive)
       (rgrep (read-string "Search terms: ") ,pattern ,path))
     (evil-leader/set-key ,(concat "s g " key) (intern ,(format "nice-rgrep-%s" dname)))))

(nice-rgrep-directory "notes" "~/public-site/org/notes" "*" "n")
(nice-rgrep-directory "journal" "~/Documents/journal" "*.org" "j")
(nice-rgrep-directory "reviews" "~/Documents/bibliography" "*" "r")

(evil-leader/set-key "s g ." (lambda () (interactive) (rgrep (read-string "Search terms: ") "*")))

;; Be virtuous and lead by example 
;; =============================== 

(setq-default major-mode
              (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(recentf-mode t)

(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)

;; Write well
;; ----------

;; TODO Configure the =dictionary= command so that it works off of a
;; local copy of Webster's

;; TODO Configure this so there is a command to meld the current
;; private dictionary and one that is stored in VC.

(require 'flyspell)
(require 'writegood-mode)

(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/.aspell.en.pws")

(defun nice-diff-dictionaries ()
  "Run ediff on the current ispell-personal-dictionary and the
backup dictionary."
  (interactive)
  (let ((backup-dictionary (concat nice-resources-dir "/aspell.en.pws")))
    (ediff-files ispell-personal-dictionary backup-dictionary)))

(set-face-attribute 'flyspell-duplicate nil
                    :underline nil
		    :foreground "white"
                    :background "red")
(set-face-attribute 'flyspell-incorrect nil
                    :underline nil
		    :foreground "white"
                    :background "red")

(evil-leader/set-key "t S" 'flyspell-mode) ; toggle flyspell on/off.
(evil-leader/set-key "S b" 'flyspell-buffer)
(evil-leader/set-key "S r" 'flyspell-region)
(evil-leader/set-key "S c" 'flyspell-correct-word-before-point)
(evil-leader/set-key "S d" 'nice-diff-dictionaries)

(evil-leader/set-key "t w" 'writegood-mode)
(add-to-list 'writegood-weasel-words "respectively")

;; Be powerful with packages
;; =========================

;; NXML
;; ----

;; TODO Install and configure nxml-mode.

;; Yasnippet
;; ---------

(require 'yasnippet)
(yas-global-mode 1)

(defun nice-load-snippets ()
  (interactive)
  (let ((snippets-dir "~/.emacs.d/snippets")) 
    (unless (file-exists-p snippets-dir)
      (make-directory snippets-dir))
    (yas-load-directory snippets-dir)))

(nice-load-snippets)

(evil-leader/set-key
  "y i" 'yas-insert-snippet     ; Insert a snippet
  "y n" 'yas-new-snippet        ; Create a new snippet
  "y v" 'yas-visit-snippet-file ; Visit the snippet file for the current mode
  "y r" 'yas-reload-all         ; Reload all snippets
  "y l" 'nice-load-snippets)    ; Load your custom snippets

;; Multiple cursors
;; ----------------
;;
;; Using mutiple cursors is a little bit tricky but here are some
;; simple steps you can try on the following example text.
;;
;; ```
;; the cat sat on the mat
;; catch this ball said pat
;; the food was eaten by the cat
;; ```
;;
;; 1. Select the an instance of "cat" with the cursor at the start
;; 2. Use the keys below, e.g. `SPC c n` to select occurrences
;; 3. Use `evil-insert` (`SPC c i`) to start editing.
;; 4. Exit using `mc/keyboard-quit` (`SPC c q`)

(require 'multiple-cursors)
(require 'evil-mc)
(global-evil-mc-mode 1)

(evil-leader/set-key
  "c n" 'mc/mark-next-like-this        ; Mark next occurrence
  "c p" 'mc/mark-previous-like-this    ; Mark previous occurrence
  "c N" 'mc/skip-to-next-like-this     ; Skip and mark next occurrence
  "c P" 'mc/skip-to-previous-like-this ; Skip and mark previous occurrence
  "c u" 'mc/unmark-next-like-this      ; Unmark next cursor
  "c U" 'mc/unmark-previous-like-this  ; Unmark previous cursor
  "c i" 'evil-insert                   ; Drop into using the cursors 
  "c q" 'mc/keyboard-quit              ; Quit multiple-cursors mode
  )

;; Magit
;; -----

(require 'magit)

(evil-leader/set-key "g s" 'magit-status)

(defmacro nice-canned-commit-message (fname cmessage key)
  "Define a canned commit message function with an Evil key binding.

  This macro takes in three arguments:
  - FNAME: A string that will be used to construct the function name.
  - CMESSAGE: A string that represents the canned commit message.
  - KEY: A string that represents the keybinding for the function using the Evil leader.

  The function created by this macro generates a commit message with a timestamp by
  concatenating the specified CMESSAGE string with the current day and time. The commit
  is created using `magit-commit-create`, which is invoked with the `--edit` option to
  open the commit message in an editor. The function is bound to the Evil leader key
  sequence `g c KEY`, where `KEY` is the specified key string.

  Example usage:
  (nice-canned-commit-message \"my-canned-commit\" \"Fix some bugs\" \"c\")"
  `(progn
     (defun ,(intern (format "nccm-%s" fname)) ()
       "Generate a canned commit message with a timestamp."
       (interactive)
       (let ((commit-message (format "%s %s"
                                     ,cmessage
                                     (downcase (format-time-string "%A %l:%M %p")))))
	 (magit-commit-create (list "--edit" (concat "-m \"" commit-message "\"")))))
     (evil-leader/set-key ,(concat "g c " key) (intern ,(format "nccm-%s" fname)))))

(nice-canned-commit-message review "update reading list" "r")
(nice-canned-commit-message website "update website" "w")
(nice-canned-commit-message journal "update journal" "j")

(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

;; Emacs Lisp
;; ----------

;; TODO Configure a linter

(evil-leader/set-key-for-mode 'emacs-lisp-mode "m s c" 'eval-last-sexp)
(evil-leader/set-key-for-mode 'emacs-lisp-mode "m s b" 'eval-buffer)
(evil-leader/set-key-for-mode 'emacs-lisp-mode "m s r" 'eval-region)

;; Emacs Speaks Statistics
;; -----------------------

(require 'ess-site)
(setq ess-default-style 'DEFAULT)

(evil-leader/set-key-for-mode 'ess-r-mode "m s b" 'ess-eval-buffer)
(evil-leader/set-key-for-mode 'ess-r-mode "m s r" 'ess-eval-region)
(evil-leader/set-key-for-mode 'ess-r-mode "m '" 'ess-switch-to-inferior-or-script-buffer)

;; Scheme/Racket
;; -------------

;; TODO Work out how to start a repl properly, running the key does
;; not seem to work, I need to run the command via M-x directly.

(require 'racket-mode)
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
(setq racket-program "/usr/bin/racket")

(evil-leader/set-key-for-mode 'racket-mode "m s b" 'racket-run)
(evil-leader/set-key-for-mode 'racket-mode "m s r" 'racket-send-region)
(evil-leader/set-key-for-mode 'racket-mode "m s c" 'racket-send-last-sexp)

;; LaTeX/BibTeX
;; ------------

;; TODO Configure this so that there is a good way to search the key
;; bibtex files, perhaps with a SQL type search

;; TODO Configure this so there is the command to convert ris to bib.

(defun most-recent-file (files)
  "Sort FILES by modification time and return the most recent file."
  (car (sort files
             (lambda (a b)
               (time-less-p (nth 5 (file-attributes b))
                            (nth 5 (file-attributes a)))))))

(defun copy-file-with-bib-extension (file-path)
  "Create a copy of the file at FILE-PATH with a .bib extension."
  (let* ((file-name (file-name-nondirectory file-path))
         (file-base-name (file-name-sans-extension file-name))
         (new-file-name (concat file-base-name ".bib"))
         (new-file-path (concat (file-name-directory file-path) new-file-name)))
    (copy-file file-path new-file-path t)
    new-file-path))

(defun nice-visit-last-bib ()
  "Visit the most recent BIB file in Downloads. If there is a TXT
file that is younger than the last BIB file, send a message to
indicate this."
  (interactive)
  (let* ((bib-files (directory-files "~/Downloads" t ".*bib" "ctime"))
         (most-recent-bib (most-recent-file bib-files))
         (txt-files (directory-files "~/Downloads" t ".*txt" "ctime"))
	 (most-recent-txt (most-recent-file txt-files)))
    (if most-recent-bib
        (if (and most-recent-txt
                 (time-less-p (nth 5 (file-attributes most-recent-bib))
                              (nth 5 (file-attributes most-recent-txt))))
            (progn (message (concat "A more recent .txt file exists: " most-recent-txt))
		   (find-file (copy-file-with-bib-extension most-recent-txt)))
          (find-file most-recent-bib))
      (message "No bib files found in ~/Downloads/"))))

(defun nice-bibtex-braces ()
  "Wrap upper case letters with brackets for bibtex titles."
  (interactive)
  (evil-ex "'<,'>s/\\([A-Z]+\\)/\\{\\1\\}/g"))

(evil-leader/set-key "v b l" 'nice-visit-last-bib)

(evil-leader/set-key-for-mode 'bibtex-mode "m b b" 'nice-bibtex-braces)
(evil-leader/set-key-for-mode 'bibtex-mode "m b f" 'bibtex-reformat)

;; Org-Mode
;; --------

(defun nice-org-mode-hook ()
  "Set up org-mode specific keybindings."
  (local-set-key (kbd "<tab>") #'org-cycle))

(add-hook 'org-mode-hook #'nice-org-mode-hook)

(setq org-agenda-start-day "-7d")
(setq org-agenda-span 30)
(setq org-agenda-start-on-weekday nil)

(evil-leader/set-key "a a" 'org-agenda)
(evil-leader/set-key-for-mode 'org-mode "a s" 'org-schedule)

;; TODO Configure the publishing system so it generates the website :(

;; TODO Configure the publishing system so it generates the nicemacs pages :(

;; Visitors
;; ========

(defmacro NVF (fname pname path)
 "Define a function for visiting a file with a given name and path.

FNAME is the name of the function to define.
PNAME is a string describing the file being visited.
PATH is the path to the file to be visited.

The resulting function will display a message indicating that the
file is being visited, then open the file using `find-file'."
  `(defun ,(intern (format "nice-visit-%s" fname)) ()
     (interactive)
     (progn
       (message ,(format "Visiting %s" pname))
       (find-file ,path))))

(defmacro NVNF (fname pname file key)
  "Macro to define a function for visiting a notes file and set an Evil leader key binding.

  This macro takes in four arguments:
  - FNAME: A string that will be used to construct the function name.
  - PNAME: A string that will be used in the message displayed to the user.
  - FILE: A string that represents the name of the notes file.
  - KEY: A string that represents the keybinding for the function using the Evil leader.

  The function created by this macro opens the notes file specified by FILE in
  the directory specified by `nice-notes-directory`. The keybinding is set using
  the Evil leader, and is constructed using the specified KEY string.

  Example usage:
  (NVNF \"my-notes\" \"My Notes\" \"my-notes.org\" \"n\")"

  `(progn
     (defun ,(intern (format "nice-visit-%s" fname)) ()
       "Visit a notes file."
       (interactive)
       (progn
         (message ,(format "Visiting %s" pname))
         (find-file ,(concat nice-notes-directory "/" file))))
     (evil-leader/set-key ,(concat "v n " key) (intern ,(format "nice-visit-%s" fname)))))

(defmacro NVD (dname pname path key)
  "Macro to define a function for visiting a directory and set an Evil leader key binding.

  This macro takes in four arguments:
  - DNAME: A string that will be used to construct the function name.
  - PNAME: A string that will be used in the message displayed to the user.
  - PATH: A string that represents the path of the directory.
  - KEY: A string that represents the keybinding for the function using the Evil leader.

  The function created by this macro jumps to the directory specified by PATH using `dired-jump`.
  The keybinding is set using the Evil leader, and is constructed using the specified KEY string.

  Example usage:
  (NVD \"my-dir\" \"My Directory\" \"/path/to/directory\" \"d\")"

  `(progn
     (defun ,(intern (format "nice-visit-%s" dname)) ()
       "Visit a directory."
       (interactive)
       (progn
         (message ,(format "Visiting %s" pname))
         (dired-jump nil ,path)
         (revert-buffer)))
     (evil-leader/set-key ,(concat "v d " key) (intern ,(format "nice-visit-%s" dname)))))

(NVF colleagues "Colleagues notes" "~/Documents/professional/colleague-details.org")
(NVNF academia-notes "Academia notes" "academic-journal-notes.org" "a")
(NVNF beast-notes "BEAST2 notes" "beast2-notes.org" "b")
(NVNF git-notes "Git notes" "git-notes.org" "g")
(NVNF haskell-notes "Haskell notes" "haskell-notes.org" "h")
(NVNF java-notes "Java notes" "java-notes.org" "j")
(NVNF latex-notes "LaTeX notes" "latex-notes.org" "l")
(NVNF maxima-notes "Maxima notes" "maxima-notes.org" "m")
(NVNF org-mode-notes "org-mode notes" "org-mode-notes.org" "o")
(NVNF python-notes "Python notes" "python-notes.org" "p")
(NVNF r-notes "R notes" "r-notes.org" "r")
(NVNF ubuntu-notes "Ubuntu/Linux notes" "linux-notes.org" "u")
(NVF nicemacs "nicemacs" "~/Documents/nicemacs/nicemacs.org")
(NVF nicemacs-el "nicemacs emacs lisp" "~/Documents/nicemacs/nicemacs.el")
(NVF reading-list "Reading list" "~/Documents/bibliography/review2/reading-list.org")
(NVF review-2 "Review 2" "~/Documents/bibliography/review2/review.org")
(NVF review-engineering "Literature review: Software engineering" "~/Documents/bibliography/review/software.tex")
(NVF review-phylodynamics "Literature review: Phylodynamics" "~/Documents/bibliography/review/phylodynamics.tex")
(NVF review-references "Bibtex references" "~/Documents/bibliography/references.bib")
(NVF spelling "Spelling list" "~/public-site/org/misc/spelling.org")
(NVF statistics-notes "Statistics notes" "~/public-site/org/notes/statistics-notes.org")
(NVF timtam-manuscript "TimTam manuscript" "~/Documents/manuscripts/zarebski2022xxx/README.org")
(NVF wikipedia-notes "Wikipedia notes" "~/public-site/org/notes/wikipedia-notes.org")
(NVF xml-notes "XML notes" "~/public-site/org/notes/xml-notes.org")
(NVF zarebski-bib "Bibliography: Zarebski" "~/Documents/bibliography/zarebski/zarebski.bib")

(NVD library "Library" "~/Documents/library/fake.org" "l")
(NVD music "Music" "~/Music/fake.org" "m")
(NVD documents "Documents" "~/Documents/fake.org" "d")
(NVD downloads "Downloads" "~/Downloads/fake.org" "D")
(NVD professional "Professional" "~/Documents/professional/README.org" "p")
(NVD teaching "Teaching" "~/Documents/teaching/fake.org" "t")
(NVD website-org "Website (org files)" "~/public-site/org/fake.org" "w")
(NVD website-html "Website (HTML files)" "~/aezarebski.github.io/fake.org" "W")
(NVD notes "My notes" "~/public-site/org/notes/fake.org" "n")

(defun nice-visit-journal ()
  "Opens the current journal file. If it does not yet exist, it
  makes a copy of the one from one week ago."
  (interactive)
  (let* ((filepath-template (concat nice-journal-directory "/journal-%s.org"))
         (curr-file (format filepath-template (format-time-string "%Y-%m")))
         (prev-file (format filepath-template (format-time-string "%Y-%m" (time-subtract (current-time) (* 7 24 60 60))))))
    (unless (file-exists-p curr-file)
      (message "Creating new journal file")
      (copy-file prev-file curr-file))
    (message "Opening journal file")
    (setq org-agenda-files (list curr-file))
    (find-file curr-file)
    (goto-char (point-min))
    (recenter-top-bottom)))

(evil-leader/set-key "v e" 'nice-visit-nicemacs
                     "v E" 'nice-visit-nicemacs-el
                     "v j" 'nice-visit-journal
                     "v n s" 'nice-visit-statistics-notes
                     "v n w" 'nice-visit-wikipedia-notes
                     "v n x" 'nice-visit-xml-notes
                     "v p" 'nice-visit-professional
                     "v r e" 'nice-visit-review-engineering
                     "v r l" 'nice-visit-reading-list
                     "v r 2" 'nice-visit-review-2
                     "v r r" 'nice-visit-review-references
                     "v r p" 'nice-visit-review-phylodynamics
                     "v t" 'nice-visit-timtam-manuscript
                     "v r z" 'nice-visit-zarebski-bib
                     "v s" 'nice-visit-spelling)

;; Copilot
;; =======

;; (add-to-list 'load-path "/home/aez/.emacs.d/copilot.el/")

;; (require 'copilot)

;; (setq copilot-node-executable "/home/aez/.nvm/versions/node/v17.3.1/bin/node")
;; (add-hook 'python-mode-hook 'copilot-mode)
;; (add-hook 'ess-r-mode-hook 'copilot-mode)

;; (defun nice/copilot-tab ()
;;   "Accept the current suggestion from copilot"
;;   (interactive)
;;   (or (copilot-accept-completion)
;;       (indent-for-tab-command)))

;; (with-eval-after-load 'copilot
;;   (evil-define-key 'insert copilot-mode-map
;;     (kbd "<tab>") #'nice/copilot-tab))

;; (defun nice/copilot-cycle ()
;;   "Cycle through suggested completions"
;;   (interactive)
;;   (copilot-next-completion))

;; (with-eval-after-load 'copilot
;;   (evil-define-key 'insert copilot-mode-map
;;     (kbd "<backtab>") #'nice/copilot-cycle))

;; (evil-leader/set-key "t c" 'copilot-mode)

;; Explore new worlds
;; ==================

;; TODO Work out how to browse gopher with =gopher.el=.

;; TODO Work out how to configure auth-source.

;; TODO Work out how to use mediawiki-mode to read and edit wikipedia.

;; Customization
;; =============

;; There be dragons here
;; ---------------------

