(defun nicemacs-launch-firefox ()
  "Launch firefox asynchronously"
  (interactive)
  (shell-command "firefox &"))

(setq user-full-name "Alexander E. Zarebski")

(defvar nicemacs-resources-dir "~/Documents/nicemacs/resources"
  "The path to nicemacs on my machine.")

(defvar nicemacs-journal-directory "~/Documents/journal"
  "The directory for nicemacs journal files.")

(defvar nicemacs-quick-links-page "~/Documents/quick-links.html"
  "The HTML file with some useful in the browser.")

(spacemacs/declare-prefix "o" "own-menu")

(spacemacs/declare-prefix "oa" "applications-menu")
(spacemacs/declare-prefix "ob" "bibtex-menu")
(spacemacs/declare-prefix "oc" "commits-menu")
(spacemacs/declare-prefix "of" "file-stuff")
(spacemacs/declare-prefix "off" "fetch resource")
(spacemacs/declare-prefix "ofu" "update resource")
(spacemacs/declare-prefix "oh" "haskell-menu")
(spacemacs/declare-prefix "ol" "latex")
(spacemacs/declare-prefix "oo" "org-menu")
(spacemacs/declare-prefix "op" "paragraph-modification-menu")
(spacemacs/declare-prefix "os" "sheila-menu")
(spacemacs/declare-prefix "oS" "Search")
(spacemacs/declare-prefix "ou" "unicode-stuff")
(spacemacs/declare-prefix "ov" "visit friends")
(spacemacs/declare-prefix "ovd" "directories")
(spacemacs/declare-prefix "ovn" "notes")
(spacemacs/declare-prefix "ovr" "reviews")

(spacemacs/set-leader-keys "oaf" 'nicemacs-launch-firefox)

(setq dotspacemacs-startup-buffer-show-icons nil)

(setq dotspacemacs-scratch-mode 'org-mode)

(advice-remove 'mwheel-scroll #'spacemacs//scroll-bar-show-delayed-hide)

(setq undo-tree-auto-save-history nil)

(spacemacs/set-leader-keys "oSg" 'rgrep)

(setq company-idle-delay 0.5)
(setq company-minimum-prefix-length 3)

(setq lsp-headerline-breadcrumb-enable nil)

(setq lsp-ui-doc-delay 1.0)
(setq lsp-ui-sideline-delay 1.0)

(spacemacs/set-leader-keys "ohr" 'haskell-process-restart)

;; Set the input method to TeX for using unicode. Use C-\ to unset this.
(spacemacs/set-leader-keys "ohu" 'set-input-method)

(setq lsp-haskell-formatting-provider "stylish-haskell")

(setq exec-path (append exec-path '("/home/aez/.ghcup/bin")))
(setq lsp-haskell-server-path "/home/aez/.ghcup/bin/haskell-language-server-8.10.4")

(defvar my-node-path "/home/aez/.nvm/versions/node/v17.3.1/bin"
  "The path to node on my machine.")

(setenv "JAVA_HOME" "/usr/lib/jvm/zulu-fx-17-amd64")
(setenv "PATH" (concat (getenv "JAVA_HOME") ":" (getenv "PATH") ":" my-node-path))
(setq exec-path (append exec-path (list my-node-path)))


(defun my-nodejs-repl-command ()
  (concat my-node-path "/node"))

(setq nodejs-repl-command 'my-nodejs-repl-command)

(defun nicemacs-d3-setup (dir)
  "Set up a minimal D3 project"
  (interactive "Where should the D3 project go? ")
  (progn
    (make-directory dir)
    (let ((d3-files (list "d3.js" "demo.js" "demo.org" "index.html"
                          "blah.csv")))
      (mapc (lambda (x)
              (copy-file (concat nicemacs-resources-dir "/d3-template/"
                                 x)
                         (concat dir "/" x)))
            d3-files))))

(spacemacs/set-leader-keys "ofj" 'nicemacs-d3-setup)

(spacemacs/set-leader-keys "opf" 'org-fill-paragraph)
(spacemacs/set-leader-keys "opu" 'unfill-paragraph)

(add-to-list 'auto-mode-alist '("\\.bibtex\\'" . bibtex-mode))

(defun nvf-last-bib ()
  "Visit the most recent BIB file in Downloads. TODO There should
be a fall back such that if there is a TXT file that is younger
than the last BIB file then copy it to a new file with the same
basename but a BIB extension and open that instead."
  (interactive)
  (let* ((bib-files (directory-files-and-attributes "~/Downloads"
                                                    t ".*bib" "ctime"))
         (path-and-time (lambda (x)
                          (list (first x)
                                (eighth x))))
         (time-order (lambda (a b)
                       (time-less-p (second b)
                                    (second a))))
         (most-recent (lambda (files)
                        (car (car (sort (mapcar path-and-time files)
                                        time-order))))))
    (if (not (null bib-files))
        (find-file (funcall most-recent bib-files))
      (message "No bib files found in ~/Downloads/"))))

(defun bibtex-braces ()
  "Wrap upper case letters with brackets for bibtex titles."
  (interactive)
  (evil-ex "'<,'>s/\\([A-Z]+\\)/\\{\\1\\}/g"))

(defun bibtex-ris2bib ()
  "Convert the most recent RIS file in my downloads to a BIB
file. TODO Add error message if there are no RIS files."
  (interactive "*")
  (let* ((all-ris-files (directory-files "~/Downloads" 1 ".*ris"))
         (modification-time (lambda (fp)
                              (list (time-convert (file-attribute-modification-time (file-attributes fp))
                                                  'integer)
                                    fp)))
         (ris-filepath (nth 1
                            (car (sort (mapcar modification-time all-ris-files)
                                       (lambda (x y)
                                         (> (car x) (car y)))))))
         (target-bib "/home/aez/Downloads/new.bib")
         (ris2xml-command (format "ris2xml \"%s\" | xml2bib > %s" ris-filepath
                                  target-bib)))
    (shell-command ris2xml-command)))

(add-hook 'LaTeX-mode-hook 'variable-pitch-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)

(custom-set-faces '(font-lock-comment-face ((t (:inherit fixed-pitch))))
                  '(font-lock-keyword-face ((t (:inherit fixed-pitch))))
                  '(font-latex-sectioning-2-face ((t (:inherit bold :foreground "#3a81c3"
                                                               :height 1.3
                                                               :family "Noto Sans"))))
                  '(font-latex-sectioning-3-face ((t (:inherit bold :foreground "#2d9574"
                                                               :height 1.2
                                                               :family "Noto Sans")))))

(spacemacs/set-leader-keys "obl" 'nvf-last-bib)
(spacemacs/set-leader-keys "obf" 'bibtex-reformat)
(spacemacs/set-leader-keys "obb" 'bibtex-braces)
(spacemacs/set-leader-keys "obc" 'bibtex-ris2bib)

(spacemacs/declare-prefix "oot" "org-toggle-menu")

(require 'oc-csl)

(setq org-hide-emphasis-markers nil)

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(custom-set-faces
'(org-block ((t (:inherit fixed-pitch))))
'(org-block-begin-line ((t (:inherit fixed-pitch :extend t :background "#ddd8eb" :foreground "#9380b2"))))
'(org-block-end-line ((t (:inherit fixed-pitch :extend t :background "#ddd8eb" :foreground "#9380b2"))))
'(org-code ((t (:inherit (shadow fixed-pitch)))))
'(org-document-info ((t (:inherit fixed-pitch))))
'(org-document-info-keyword ((t (:inherit fixed-pitch))))
'(org-document-title ((t (:inherit nil :foreground "#6c3163" :underline t :weight bold :height 2.0 :family "Noto Sans"))))
'(org-level-1 ((t (:inherit nil :extend nil :foreground "#3a81c3" :weight bold :height 1.4 :family "Noto Sans"))))
'(org-level-2 ((t (:inherit nil :extend nil :foreground "#2d9574" :weight bold :height 1.2 :width normal :family "Noto Sans"))))
'(org-level-3 ((t (:extend nil :foreground "#67b11d" :weight normal :height 1.1 :family "Noto Sans"))))
'(org-level-4 ((t (:extend nil :foreground "#b1951d" :weight normal :height 1.0 :family "Noto Sans"))))
'(org-link ((t (:underline t))))
'(org-meta-line ((t (:inherit fixed-pitch))))
'(org-property-value ((t (:inherit fixed-pitch))) t)
'(org-special-keyword ((t (:inherit fixed-pitch))))
'(org-table ((t (:inherit fixed-pitch))))
'(org-tag ((t (:inherit fixed-pitch))))
'(org-verbatim ((t (:inherit fixed-pitch))))
'(font-lock-comment-face ((t (:inherit fixed-pitch))))
'(variable-pitch ((t (:family "Noto Serif")))))

(require 'writeroom-mode)

(defvar writeroom-active t "variable to say if writeroom is active")

(defun toggle-writeroom ()
  "Toggle the writeroom-mode on the current buffer."
  (interactive)
  (if writeroom-active
      (writeroom--enable)
    (writeroom--disable))
  (setq writeroom-active (not writeroom-active))
  )

(spacemacs/set-leader-keys "ootw" 'toggle-writeroom)

(require 'ox-latex)

(add-to-list 'org-latex-classes
             '("scrartcl" "\\documentclass[11pt,onecolumn]{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("scrartcl2" "\\documentclass[11pt,twocolumn]{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-packages-alist nil)
(add-to-list 'org-latex-packages-alist
             '("" "listings"))
(add-to-list 'org-latex-packages-alist
             '("" "xcolor"))
(add-to-list 'org-latex-packages-alist
             '("right" "lineno"))
(setq org-latex-listings t)

(org-babel-do-load-languages
'org-babel-load-languages
'((maxima . t)
  (R . t)))

(setq org-agenda-start-day "-7d")
(setq org-agenda-span 30)
(setq org-agenda-start-on-weekday nil)

(defun nicemacs-journal-filepath ()
  "The filepath of the current journal file."
  (interactive)
  (let* ((filepath-template (concat nicemacs-journal-directory "/journal-%s.org"))
         (time-string (format-time-string "%Y-%m"))
         (agenda-file (format filepath-template time-string)))
    (setq org-agenda-files (list agenda-file))
    agenda-file))

(defun nicemacs-journal-previous-filepath ()
  "The filepath of the /previous/ journal file."
  (interactive)
  (let* ((filepath-template (concat nicemacs-journal-directory "/journal-%s.org"))
         (seconds-in-week (* 7 (* 24 (* 60 (* 60 1)))))
         (time-string (format-time-string "%Y-%m" (time-subtract (current-time) seconds-in-week)))
         (agenda-file (format filepath-template time-string)))
    agenda-file))

(defun nvf-journal ()
  "Opens the current journal file. If it does not yet exist it
makes a copy of the one from one week ago."
  (interactive)
  (let* ((current-journal-file (nicemacs-journal-filepath))
        (previous-journal-file (nicemacs-journal-previous-filepath)))
    (if (not (file-exists-p current-journal-file))
        (progn
          (message "creating new journal file")
          (copy-file previous-journal-file current-journal-file))
      (message "opening journal file"))
          (find-file current-journal-file)
          (goto-char 1)
          (recenter-top-bottom)))

(spacemacs/set-leader-keys "oos" 'org-schedule)

(defun nicemacs-update-quick-links ()
  "Update the quick links file to use the one from the journal."
  (interactive)
  (copy-file (concat nicemacs-journal-directory "/" "quick-links.html")
             nicemacs-quick-links-page
             1))

(spacemacs/set-leader-keys "ofuq" 'nicemacs-update-quick-links)

(defun nicemacs-fetch-dotspacemacs ()
  "Put a copy of the current dotspacemacs file in the resources directory."
  (interactive)
  (copy-file (dotspacemacs/location)
             (concat nicemacs-resources-dir
                     "/dotspacemacs-"
                     (user-real-login-name)
                     "-"
                     (system-name))
             1))

(spacemacs/set-leader-keys "offd" 'nicemacs-fetch-dotspacemacs)

(defun nicemacs-fetch-aspell ()
  "Put a copy of the aspell personal word list in the resources directory."
  (interactive)
  (copy-file "~/.aspell.en.pws"
             (concat nicemacs-resources-dir "/aspell.en.pws")
             1))

(spacemacs/set-leader-keys "offs" 'nicemacs-fetch-aspell)

(defun nicemacs-update-aspell ()
  "Update the aspell personal word list."
  (interactive)
  (copy-file (concat nicemacs-resources-dir "/aspell.en.pws")
             "~/.aspell.en.pws"
             1))

(spacemacs/set-leader-keys "ofus" 'nicemacs-update-aspell)

;; open the export menu
(spacemacs/set-leader-keys "ooe" 'org-export-dispatch)

;; Make sure org files open with lines truncated
(add-hook 'org-mode-hook 'spacemacs/toggle-truncate-lines-on)

(spacemacs/set-leader-keys "ooi" 'org-toggle-inline-images)

(spacemacs/set-leader-keys "ool" 'org-latex-preview)

(require 'ol)

(add-to-list 'org-link-frame-setup '(file . find-file))

(require 'htmlize)
(require 'ox-publish)

(setq org-publish-project-alist
    '(
      ("org-notes-org-files"
       :base-directory "~/public-site/org/notes/"
       :base-extension "org"
       :exclude ".*~undo-tree~"
       :publishing-directory "~/aezarebski.github.io/notes/"
       :recursive t
       :publishing-function org-html-publish-to-html
       :headline-levels 4
       :auto-preamble t)
      ("org-lists-org-files"
       :base-directory "~/public-site/org/lists/"
       :base-extension "org"
       :exclude ".*~undo-tree~"
       :publishing-directory "~/aezarebski.github.io/lists/"
       :recursive t
       :publishing-function org-html-publish-to-html
       :headline-levels 4
       :auto-preamble nil)
      ("org-notes-static"
        :base-directory "~/public-site/org/"
        :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|txt\\|cur\\|svg\\|csv\\|html\\|json\\|bib\\|webp"
        :exclude "~/public-site/org/misc/matplotlib/.*\\|~/public-site/org/misc/d3/.*"
        :publishing-directory "~/aezarebski.github.io/"
        :recursive t
        :publishing-function org-publish-attachment)
      ("org-misc-d3-org-files"
       :base-directory "~/public-site/org/misc/d3/"
       :base-extension "org"
       :publishing-directory "~/aezarebski.github.io/misc/d3/"
       :recursive t
       :publishing-function org-html-publish-to-html
       :headline-levels 4
       :auto-preamble t)
      ("org-misc-d3-extra-files"
       :base-directory "~/public-site/org/misc/d3/"
       :base-extension "js\\|png\\|svg\\|csv\\|html\\|json"
       :publishing-directory "~/aezarebski.github.io/misc/d3/"
       :recursive t
       :publishing-function org-publish-attachment)
      ("org-nicemacs"
       :base-directory "~/Documents/nicemacs/"
       :base-extension "org"
       :publishing-directory "~/aezarebski.github.io/misc/nicemacs/"
       :recursive ()
       :publishing-function org-html-publish-to-html
       )
      ("org-bibliography"
       :base-directory "~/Documents/bibliography/"
       :base-extension "png"
       :publishing-directory "~/aezarebski.github.io/resources/"
       :recursive ()
       :publishing-function org-publish-attachment
       )
      ("review2-org"
       :base-directory "~/Documents/bibliography/review2"
       :base-extension "org"
       :publishing-directory "~/aezarebski.github.io/notes/review2"
       :recursive ()
       :publishing-function org-html-publish-to-html
       )
      ("review2-static"
       :base-directory "~/Documents/bibliography/review2"
       :base-extension "css\\|png"
       :publishing-directory "~/aezarebski.github.io/notes/review2"
       :recursive t
       :publishing-function org-publish-attachment
       )
      ("d3" :components ("org-misc-d3-org-files"
                         "org-misc-d3-extra-files"))
      ("org" :components ("org-lists-org-files"
                          "org-notes-org-files"
                          "org-notes-static"
                          "org-nicemacs"
                          "org-bibliography"
                          "review2-org"
                          "review2-static"))))

(defun publish-my-site ()
  (interactive)
  (org-publish "org" nil t)
  (copy-file "~/Documents/nicemacs/resources/nicemacs-logo.png"
             "~/aezarebski.github.io/misc/nicemacs/resources/nicemacs-logo.png"
             t)
  (copy-file "~/public-site/org/scratch.html"
             "~/aezarebski.github.io/index.html"
             t)
  (copy-file "~/.aspell.en.pws"
             "~/Documents/nicemacs/resources/aspell.en.pws"
             t)
  (copy-file "~/.spacemacs"
             "~/Documents/nicemacs/resources/spacemacs"
             t)
  )

(defun force-publish-and-magit ()
  (interactive)
  (publish-my-site)
  (org-publish "org" t nil)
  (magit-status "~/aezarebski.github.io")
  )

(spacemacs/set-leader-keys "oop" 'publish-my-site)
(spacemacs/set-leader-keys "ooP" 'force-publish-and-magit)

(defun visit-my-site-index ()
  (interactive)
  (find-file "~/public-site/org/index.org"))
(spacemacs/set-leader-keys "oov" 'visit-my-site-index)

(setq org-adapt-indentation nil)

(spacemacs/set-leader-keys "osb" 'shell)

(defun shell-and-delete-windows ()
  (interactive)
  (spacemacs/default-pop-shell)
  (delete-other-windows)
  )

(spacemacs/set-leader-keys "\"" 'shell-and-delete-windows)

(spacemacs/set-leader-keys "osh" 'helm-eshell-history)

(defun eshell-aliases ()
    "Visit the file containing the eshell aliases."
    (interactive)
    (find-file-other-window eshell-aliases-file))

(spacemacs/set-leader-keys "osa" 'eshell-aliases)

(require 'em-alias)
(eshell/alias "cdk" "cd ..")
(eshell/alias "cdkk" "cd ../..")
(eshell/alias "cdkkk" "cd ../../..")
(eshell/alias "ls1" "ls -1 $1")
(eshell/alias "ff" "find-file $1")

(setq eshell-cmpl-ignore-case t)

(setq exec-path (append exec-path '("/home/aez/.local/bin")))

(spacemacs/set-leader-keys "osi" 'ielm)

(spacemacs/set-leader-keys "osr" 'R)

(setq proced-auto-update-flag t)
(setq proced-auto-update-interval 3)

(setq spacemacs/ess-config
      '(progn
         ;; Follow Hadley Wickham's R style guide
         (setq ess-first-continued-statement-offset 2
               ess-continued-statement-offset 0
               ess-expression-offset 2
               ess-nuke-trailing-whitespace-p t
               ess-default-style 'DEFAULT)
         (when ess-disable-underscore-assign
           (setq ess-smart-S-assign-key nil))

         ;; (define-key ess-doc-map "h" 'ess-display-help-on-object)
         ;; (define-key ess-doc-map "p" 'ess-R-dv-pprint)
         ;; (define-key ess-doc-map "t" 'ess-R-dv-ctable)
         (dolist (mode '(ess-r-mode ess-mode)))))

;; make documentation open in a useful mode in ess
(evil-set-initial-state 'ess-r-help-mode 'motion)

(setq ess-startup-directory-function '(lambda nil default-directory))

(setq ess-use-flymake nil)
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

(defmacro nicemacs-commits (fname cmessage)
  (list 'defun
        (intern (format "ncf-%s" fname))
        ()
        (list 'interactive)
        (list 'magit-commit-create
              `(list "--edit"
                     ,(list 'format
                            "-m %s %s"
                            cmessage
                            (list 'downcase
                                  (list 'format-time-string "%A %l:%M %p")))))))

(nicemacs-commits network "update citation network")
(spacemacs/set-leader-keys "ocn" 'ncf-network)

(nicemacs-commits review "update reading list")
(spacemacs/set-leader-keys "ocr" 'ncf-review)

(nicemacs-commits website "update website")
(spacemacs/set-leader-keys "ocw" 'ncf-website)

(nicemacs-commits journal "update journal")
(spacemacs/set-leader-keys "ocj" 'ncf-journal)

(defun kill-all-other-buffers ()
  "Kill all the buffers other than the current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; Define a short cut to close all windows except the current one without killing
;; their buffers.
(spacemacs/set-leader-keys "wD" 'spacemacs/window-manipulation-transient-state/delete-other-windows)

;; Define a short cut for following files
(spacemacs/set-leader-keys "ofp" 'helm-projectile-find-file)

(setq time-stamp-format "Last modified: %Y-%02m-%02d %02H:%02M:%02S")
(add-hook 'before-save-hook 'time-stamp)

(spacemacs/set-leader-keys "ofv" 'view-file)
(spacemacs/set-leader-keys "ofl" 'find-file-literally)

(setq dired-listing-switches "-alh")

(defmacro nicemacs-visit-file (fname pname path)
  (list 'defun
        (intern (format "nvf-%s" fname))
        ()
        (list 'interactive)
        (list 'progn
              (list 'message
                    (format "Visiting %s" pname))
              (list 'find-file path))))

(nicemacs-visit-file academia-notes "Academia notes" "/home/aez/public-site/org/notes/academic-journal-notes.org")
(nicemacs-visit-file beast-notes "BEAST2 notes" "/home/aez/public-site/org/notes/beast2-notes.org")
(nicemacs-visit-file colleagues "Colleagues notes" "~/Documents/professional/colleague-details.org")
(nicemacs-visit-file git-notes "Git notes" "/home/aez/public-site/org/notes/git-notes.org")
(nicemacs-visit-file haskell-notes "Haskell notes" "/home/aez/public-site/org/notes/haskell-notes.org")
(nicemacs-visit-file java-notes "Java notes" "/home/aez/public-site/org/notes/java-notes.org")
(nicemacs-visit-file latex-notes "LaTeX notes" "/home/aez/public-site/org/notes/latex-notes.org")
(nicemacs-visit-file maxima-notes "Maxima notes" "/home/aez/public-site/org/notes/maxima-notes.org")
(nicemacs-visit-file nicemacs "nicemacs" "~/Documents/nicemacs/nicemacs.org")
(nicemacs-visit-file nicemacs-el "nicemacs emacs lisp" "~/Documents/nicemacs/nicemacs.el")
(nicemacs-visit-file org-mode-notes "org-mode notes" "/home/aez/public-site/org/notes/org-mode-notes.org")
(nicemacs-visit-file python-notes "Python notes" "/home/aez/public-site/org/notes/python-notes.org")
(nicemacs-visit-file r-notes "R notes" "/home/aez/public-site/org/notes/r-notes.org")
(nicemacs-visit-file ubuntu-notes "Ubuntu/Linux notes" "/home/aez/public-site/org/notes/linux-notes.org")
(nicemacs-visit-file reading-list "Reading list" "/home/aez/Documents/bibliography/review2/reading-list.org")
(nicemacs-visit-file review-2 "Review 2" "/home/aez/Documents/bibliography/review2/review.org")
(nicemacs-visit-file review-engineering "Literature review: Software engineering" "/home/aez/Documents/bibliography/review/software.tex")
(nicemacs-visit-file review-phylodynamics "Literature review: Phylodynamics" "/home/aez/Documents/bibliography/review/phylodynamics.tex")
(nicemacs-visit-file review-references "Bibtex references" "/home/aez/Documents/bibliography/references.bib")
(nicemacs-visit-file spelling "Spelling list" "/home/aez/public-site/org/misc/spelling.org")
(nicemacs-visit-file statistics-notes "Statistics notes" "/home/aez/public-site/org/notes/statistics-notes.org")
(nicemacs-visit-file timtam-manuscript "TimTam manuscript" "/home/aez/Documents/manuscripts/zarebski2022xxx/README.org")
(nicemacs-visit-file wikipedia-notes "Wikipedia notes" "/home/aez/public-site/org/notes/wikipedia-notes.org")
(nicemacs-visit-file xml-notes "XML notes" "/home/aez/public-site/org/notes/xml-notes.org")
(nicemacs-visit-file zarebski-bib "Bibliography: Zarebski" "/home/aez/Documents/bibliography/zarebski/zarebski.bib")

(defmacro nicemacs-visit-dir (dname pname path)
  (list 'defun
        (intern (format "nvd-%s" dname))
        ()
        (list 'interactive)
        (list 'progn
              (list 'message
                    (format "Visiting %s" pname))
              (list 'dired-jump nil path)
              (list 'revert-buffer))))

(nicemacs-visit-dir library "Library" "/home/aez/Documents/library/fake.org")
(nicemacs-visit-dir music "Music" "/home/aez/Music/fake.org") ; default in Ubuntu 20.04.4
(nicemacs-visit-dir documents "Documents" "/home/aez/Documents/fake.org")
(nicemacs-visit-dir downloads "Downloads" "/home/aez/Downloads/fake.org")
(nicemacs-visit-dir professional "Professional" "/home/aez/Documents/professional/README.org")
(nicemacs-visit-dir timtam "TimTam" "/home/aez/Documents/timtam2/README.org")
(nicemacs-visit-dir website-org "Website (org files)" "/home/aez/public-site/org/fake.org")
(nicemacs-visit-dir website-html "Website (HTML files)" "/home/aez/aezarebski.github.io/fake.org")
(nicemacs-visit-dir notes "My notes" "/home/aez/public-site/org/notes/fake.org")

(spacemacs/set-leader-keys
  "ovb" 'nvf-last-bib
  "ovc" 'nvf-colleagues
  "ove" 'nvf-nicemacs
  "ovE" 'nvf-nicemacs-el
  "ovj" 'nvf-journal
  "ovdd" 'nvd-documents
  "ovdD" 'nvd-downloads
  "ovdl" 'nvd-library
  "ovdm" 'nvd-music
  "ovdn" 'nvd-notes
  "ovdp" 'nvd-professional
  "ovdt" 'nvd-timtam
  "ovdw" 'nvd-website-org
  "ovdW" 'nvd-website-html
  "ovna" 'nvf-academia-notes
  "ovnb" 'nvf-beast-notes
  "ovng" 'nvf-git-notes
  "ovnh" 'nvf-haskell-notes
  "ovnj" 'nvf-java-notes
  "ovnl" 'nvf-latex-notes
  "ovnm" 'nvf-maxima-notes
  "ovno" 'nvf-org-mode-notes
  "ovnp" 'nvf-python-notes
  "ovnr" 'nvf-r-notes
  "ovns" 'nvf-statistics-notes
  "ovnu" 'nvf-ubuntu-notes
  "ovnw" 'nvf-wikipedia-notes
  "ovnx" 'nvf-xml-notes
  "ovp" 'nvf-professional
  "ovre" 'nvf-review-engineering
  "ovrl" 'nvf-reading-list
  "ovr2" 'nvf-review-2
  "ovrr" 'nvf-review-references
  "ovrp" 'nvf-review-phylodynamics
  "ovt" 'nvf-timtam-manuscript
  "ovrz" 'nvf-zarebski-bib
  "ovs" 'nvf-spelling)

(defun nsg-notes ()
  (interactive)
  (let ((search-terms (read-string "Search term: ")))
    (progn
      (message search-terms)
      (rgrep search-terms "*.org" "/home/aez/public-site/org/notes/"))))

(defun nsg-journal ()
  (interactive)
  (let ((search-terms (read-string "Search term: ")))
    (progn
      (message search-terms)
      (rgrep search-terms "*.org" "/home/aez/Documents/journal/"))))

(defun nsg-review ()
  (interactive)
  (let ((search-terms (read-string "Search term: ")))
    (progn
      (message search-terms)
      (rgrep search-terms "*.tex" "/home/aez/Documents/bibliography/"))))

(spacemacs/set-leader-keys
  "oSn" 'nsg-notes
  "oSj" 'nsg-journal
  "oSr" 'nsg-review)

(spacemacs/set-leader-keys "ofb" 'ibuffer)
;; Open Ibuffer in the motion state rather than as the default emacs mode.
(evil-set-initial-state 'ibuffer-mode 'motion)

(defun message-buffer-file-name ()
  "Print the full path of the current buffer's file to the
minibuffer and store this on the kill ring."
  (interactive)
  (kill-new buffer-file-name)
  (message buffer-file-name))

(spacemacs/set-leader-keys "ofd" 'message-buffer-file-name)

(defun cp-most-recent-download ()
  (interactive)
  (let* ((all-files (directory-files-and-attributes "~/Downloads"
                                                    t ".*" "ctime"))
         (path-and-time (lambda (x)
                          (list (first x)
                                (eighth x))))
         (time-order (lambda (a b)
                       (time-less-p (second b)
                                    (second a))))
         (most-recent (lambda (files)
                        (car (car (sort (mapcar path-and-time files)
                                        time-order))))))
    (if (not (null all-files))
        (let ((most-recent-file (funcall most-recent all-files)))
          (progn
            (message (concat "copying file: " most-recent-file))
            (copy-file most-recent-file
                       (concat default-directory
                               (file-name-nondirectory most-recent-file))
                       1)))
      (message "No file found in ~/Downloads/"))))

(defun ls-most-recent-download ()
  (interactive)
  (let* ((all-files (directory-files-and-attributes "~/Downloads"
                                                    t ".*" "ctime"))
         (path-and-time (lambda (x)
                          (list (first x)
                                (eighth x))))
         (time-order (lambda (a b)
                       (time-less-p (second b)
                                    (second a))))
         (most-recent (car (car (sort (mapcar path-and-time all-files)
                                      time-order)))))
    (if (not (null all-files))
        (progn (message (concat "newest file in ~/Downloads: " most-recent))
               most-recent)
      (message "No file found in ~/Downloads/"))))

(defmacro nicemacs-greek (lname)
    (list 'progn
          (list 'defun (intern (format "nag-%s-small" lname)) ()
                (list 'interactive)
                (list 'insert (char-from-name (upcase (format "greek small letter %s" lname)))))
          (list 'defun (intern (format "nag-%s-capital" lname)) ()
                (list 'interactive)
                (list 'insert (char-from-name (upcase (format "greek capital letter %s" lname)))))))

(nicemacs-greek alpha)
(nicemacs-greek beta)
(nicemacs-greek gamma)
(nicemacs-greek delta)
(nicemacs-greek theta)
(nicemacs-greek lambda)
(nicemacs-greek mu)
(nicemacs-greek nu)
(nicemacs-greek rho)
(nicemacs-greek sigma)
(nicemacs-greek psi)
(nicemacs-greek omega)

(spacemacs/set-leader-keys
  "oua" 'nag-alpha-small
  "ouA" 'nag-alpha-capital
  "oub" 'nag-beta-small
  "ouB" 'nag-beta-capital
  "oug" 'nag-gamma-small
  "ouG" 'nag-gamma-capital
  "oud" 'nag-delta-small
  "ouD" 'nag-delta-capital
  "outh" 'nag-theta-small
  "ouTh" 'nag-theta-capital
  "oul" 'nag-lambda-small
  "ouL" 'nag-lambda-capital
  "oum" 'nag-mu-small
  "ouM" 'nag-mu-capital
  "oun" 'nag-nu-small
  "ouN" 'nag-nu-capital
  "our" 'nag-rho-small
  "ouR" 'nag-rho-capital
  "ous" 'nag-sigma-small
  "ouS" 'nag-sigma-capital
  "oup" 'nag-psi-small
  "ouo" 'nag-omega-small)

(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.x[ms]l\\'" . nxml-mode))

(add-hook 'nxml-mode-hook 'origami-mode)

(files--ensure-directory "~/.emacs.d/private/snippets/ess-r-mode")
(files--ensure-directory "~/.emacs.d/private/snippets/org-mode")
(files--ensure-directory "~/.emacs.d/private/snippets/python-mode")
