(defun visit-nicemacs ()
  (interactive)
  (find-file "~/Documents/nicemacs/README.org"))
(spacemacs/set-leader-keys "oon" 'visit-nicemacs)

(setq dotspacemacs-scratch-mode 'org-mode)

(spacemacs/declare-prefix "o" "own-menu")

(advice-remove 'mwheel-scroll #'spacemacs//scroll-bar-show-delayed-hide)

(setq company-idle-delay 0.5)
(setq company-minimum-prefix-length 3)

(setq lsp-headerline-breadcrumb-enable nil)

(setq lsp-ui-doc-delay 1.0)
(setq lsp-ui-sideline-delay 1.0)

(spacemacs/declare-prefix "oh" "haskell-menu")
(spacemacs/set-leader-keys "ohr" 'haskell-process-restart)

;; Set the input method to TeX for using unicode. Use C-\ to unset this.
(spacemacs/set-leader-keys "ohu" 'set-input-method)

(setq lsp-haskell-formatting-provider "stylish-haskell")

(setq exec-path (append exec-path '("/home/aez/.ghcup/bin")))
(setq lsp-haskell-server-path "/home/aez/.ghcup/bin/haskell-language-server-8.10.4")

(add-to-list 'auto-mode-alist '("\\.bibtex\\'" . bibtex-mode))

(spacemacs/declare-prefix "ol" "latex")
(spacemacs/declare-prefix "ob" "bibtex-menu")

(defun visit-bib-and-tex-file (path-template)
  (interactive)
  (progn
    (find-file path-template)
    (goto-char 1)
    (recenter-top-bottom)))

(defun review-tex-file ()
  "Open my review.tex file"
  (interactive)
  (visit-bib-and-tex-file "~/Documents/bibliography/review/review.tex"))

(spacemacs/set-leader-keys "olr" 'review-tex-file)

(defun reading-list-tex-file ()
  "Open my review.tex file"
  (interactive)
  (visit-bib-and-tex-file "~/Documents/bibliography/review/reading-list.tex"))

(spacemacs/set-leader-keys "oll" 'reading-list-tex-file)

(defun references-bib-file ()
  "Opens my bibtex references."
  (interactive)
  (visit-bib-and-tex-file "~/Documents/bibliography/references.bib"))

(spacemacs/set-leader-keys "obr" 'references-bib-file)

(defun last-bib ()
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
         (ris2xml-command (format "ris2xml %s | xml2bib > %s" ris-filepath
                                  target-bib)))
    (shell-command ris2xml-command)))

(spacemacs/set-leader-keys "obl" 'last-bib)
(spacemacs/set-leader-keys "obf" 'bibtex-reformat)
(spacemacs/set-leader-keys "obb" 'bibtex-braces)
(spacemacs/set-leader-keys "obc" 'bibtex-ris2bib)

(defun nicemacs-open-review-pdf ()
  "Open PDF of reading notes in evince."
  (interactive)
  (let ((pdf-viewer "evince")
        (review-path "/home/aez/Documents/bibliography/review/review.pdf"))
    (shell-command (concat pdf-viewer " " review-path " &"))))

(spacemacs/set-leader-keys "olp" 'nicemacs-open-review-pdf)

(spacemacs/declare-prefix "oo" "org-menu")

(spacemacs/declare-prefix "oot" "org-toggle-menu")

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

(org-babel-do-load-languages
'org-babel-load-languages
'((maxima . t)))

(defvar nicemacs-journal-directory "" "The directory for nicemacs journal files.")
(setq nicemacs-journal-directory "~/Documents/journal")

(setq org-agenda-start-day "-5d")
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

(defun nicemacs-visit-journal ()
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

(defun nicemacs-visit-agenda ()
  "Opens the agenda after checking it has been set correctly."
  (interactive)
  (let ((agenda-file (nicemacs-journal-filepath)))
    (org-agenda-list)))

(spacemacs/set-leader-keys "ooa" 'nicemacs-visit-agenda)
(spacemacs/set-leader-keys "oos" 'org-schedule)

(require 'ox-publish)

(setq org-publish-project-alist
      '(
        ("org-notes"
         :base-directory "~/public-site/org/"
         :base-extension "org"
         :publishing-directory "~/aezarebski.github.io/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-preamble t
         )
        ("org-static"
         :base-directory "~/public-site/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|txt\\|cur\\|svg\\|csv\\|html\\|json\\|webp"
         :exclude "~/public-site/org/misc/matplotlib/ven.*"
         :publishing-directory "~/aezarebski.github.io/"
         :recursive t
         :publishing-function org-publish-attachment
         )
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
      ("org" :components ("org-notes" "org-static" "org-nicemacs" "org-bibliography"))
        ))

(defun publish-my-site ()
  (interactive)
  (org-publish "org")
  (let ((readme "~/aezarebski.github.io/misc/nicemacs/README.html")
        (index "~/aezarebski.github.io/misc/nicemacs/index.html"))
    (if (file-exists-p readme)
        (copy-file readme index t)))
  (copy-file "~/Documents/nicemacs/resources/nicemacs-logo.png"
             "~/aezarebski.github.io/misc/nicemacs/resources/nicemacs-logo.png"
             t)
  (copy-file "~/public-site/org/scratch.html"
             "~/aezarebski.github.io/index.html"
             t)
  )

(defun publish-my-site-and-magit ()
  (interactive)
  (publish-my-site)
  (magit-status "~/aezarebski.github.io")
  )

(spacemacs/set-leader-keys "oop" 'publish-my-site)
(spacemacs/set-leader-keys "ooP" 'publish-my-site-and-magit)

(defun visit-my-site-index ()
  (interactive)
  (find-file "~/public-site/org/index.org"))
(spacemacs/set-leader-keys "oov" 'visit-my-site-index)

(setq org-adapt-indentation nil)

;; open the export menu
(spacemacs/set-leader-keys "ooe" 'org-export-dispatch)

;; Make sure org files open with lines truncated
(add-hook 'org-mode-hook 'spacemacs/toggle-truncate-lines-on)

(spacemacs/set-leader-keys "ooi" 'org-toggle-inline-images)

(spacemacs/set-leader-keys "ool" 'org-latex-preview)

(require 'ol)

(add-to-list 'org-link-frame-setup '(file . find-file))

(defun shell-and-delete-windows ()
  (interactive)
  (spacemacs/default-pop-shell)
  (delete-other-windows)
  )

(spacemacs/set-leader-keys "\"" 'shell-and-delete-windows)

(spacemacs/declare-prefix "os" "sheila-menu")

(spacemacs/set-leader-keys "osh" 'helm-eshell-history)

(spacemacs/set-leader-keys "osb" 'shell)

(defun eshell-aliases ()
    "Visit the file containing the eshell aliases."
    (interactive)
    (find-file-other-window eshell-aliases-file))

(spacemacs/set-leader-keys "osa" 'eshell-aliases)

(require 'em-alias)
(eshell/alias "cdk" "cd ..")
(eshell/alias "cdkk" "cd ../..")
(eshell/alias "cdkkk" "cd ../../..")
(eshell/alias "ff" "find-file $1")

(setq eshell-cmpl-ignore-case t)

(setq exec-path (append exec-path '("/home/aez/.local/bin")))
(setq exec-path (append exec-path '("/home/aez/.nvm/versions/node/v14.6.0/bin")))

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

(setq ess-use-flymake nil)
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

(spacemacs/declare-prefix "oc" "commits-menu")

(defun journal-commit-function ()
  "Create a commit and insert a string describing a generic
commit to my journal file. This should be fun from the magit
buffer"
  (interactive)
  (let* ((date-string (downcase (format-time-string "%A %l:%M %p")))
         (edit-string (format "-mupdate tasklist %s"  date-string)))
    (magit-commit-create `("--edit", edit-string))))

(spacemacs/set-leader-keys "ocj" 'journal-commit-function)

(defun website-commit-function ()
  (interactive)
  (let* ((date-string (downcase (format-time-string "%A %l:%M %p")))
         (edit-string (format "-mupdate website %s"  date-string)))
    (magit-commit-create `("--edit", edit-string))))

(spacemacs/set-leader-keys "ocw" 'website-commit-function)

(defun review-commit-function ()
  (interactive)
  (let* ((date-string (downcase (format-time-string "%A %l:%M %p")))
         (edit-string (format "-mupdate reading list %s"  date-string)))
    (magit-commit-create `("--edit", edit-string))))

(spacemacs/set-leader-keys "ocr" 'review-commit-function)

(defun kill-all-other-buffers ()
  "Kill all the buffers other than the current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; Define a short cut to close all windows except the current one without killing
;; their buffers.
(spacemacs/set-leader-keys "wD" 'spacemacs/window-manipulation-transient-state/delete-other-windows)

;; Define a short cut for C-g which is a little awkward.
(spacemacs/set-leader-keys "og" 'keyboard-quit)

;; Define a short cut for following files
(spacemacs/declare-prefix "of" "file-stuff")
(spacemacs/set-leader-keys "off" 'find-file-at-point)
(spacemacs/set-leader-keys "ofp" 'helm-projectile-find-file)

(setq dired-listing-switches "-alh")

(spacemacs/declare-prefix "ov" "visit friendly files")

(defmacro nicemacs-visit (fname pname path)
  (list 'defun (intern (format "nvf-%s" fname)) ()
        (list 'interactive)
        (list 'progn
              (list 'message (format "Visiting %s" pname))
              (list 'find-file path))))

(nicemacs-visit nicemacs "nicemacs README" "~/Documents/nicemacs/README.org")
(nicemacs-visit beast-notes "BEAST2 notes" "/home/aez/public-site/org/notes/beast2-notes.org")
(nicemacs-visit colleagues "Colleagues notes" "~/Documents/professional/colleague-details.org")
(nicemacs-visit spelling "Spelling list" "/home/aez/public-site/org/misc/spelling.org")
(nicemacs-visit haskell-notes "Haskell notes" "/home/aez/public-site/org/notes/haskell-notes.org")
(nicemacs-visit java-notes "Java notes" "/home/aez/public-site/org/notes/java-notes.org")
(nicemacs-visit python-notes "Python notes" "/home/aez/public-site/org/notes/python-notes.org")
(nicemacs-visit r-notes "R notes" "/home/aez/public-site/org/notes/r-notes.org")
(nicemacs-visit org-mode-notes "org-mode notes" "/home/aez/public-site/org/notes/org-mode-notes.org")
(nicemacs-visit reading-list "Reading list" "/home/aez/Documents/bibliography/review/reading-list.tex")
(nicemacs-visit references "Bibtex references" "/home/aez/Documents/bibliography/references.bib")
(nicemacs-visit statistics-notes "Statistics notes" "/home/aez/public-site/org/notes/statistics-notes.org")

(defun nvf-journal ()
  (interactive)
  (nicemacs-visit-journal))

(defun nvf-last-bib ()
  (interactive)
  (last-bib))

(defun nvf-website ()
  (interactive)
  (dired-jump nil "/home/aez/public-site/org/index.org"))

(defun nvf-professional ()
  (interactive)
  (dired-jump nil "/home/aez/Documents/professional/README.org"))

(spacemacs/set-leader-keys
  "ovb" 'nvf-last-bib
  "ovc" 'nvf-colleagues
  "ove" 'nvf-nicemacs
  "ovj" 'nvf-journal
  "ovl" 'nvf-reading-list
  "ovnb" 'nvf-beast-notes
  "ovnh" 'nvf-haskell-notes
  "ovnj" 'nvf-java-notes
  "ovno" 'nvf-org-mode-notes
  "ovnp" 'nvf-python-notes
  "ovnr" 'nvf-r-notes
  "ovns" 'nvf-statistics-notes
  "ovp" 'nvf-professional
  "ovr" 'nvf-references
  "ovs" 'nvf-spelling
  "ovw" 'nvf-website)

(spacemacs/set-leader-keys "ofb" 'ibuffer)
;; Open Ibuffer in the motion state rather than as the default emacs mode.
(evil-set-initial-state 'ibuffer-mode 'motion)

(defun message-working-directory ()
  "Print the directory of the current buffer."
  (interactive)
  (message default-directory))

(spacemacs/set-leader-keys "ofd" 'message-working-directory)

(defun insert-greek (case-name letter-name)
  (interactive)
  (insert (char-from-name (upcase (format "GREEK %s LETTER %s" case-name letter-name)))))

(spacemacs/declare-prefix "ou" "unicode-stuff")

(defun insert-greek-small-alpha ()
  (interactive)
  (insert-greek "small" "alpha"))
(defun insert-greek-capital-alpha ()
  (interactive)
  (insert-greek "capital" "alpha"))
(spacemacs/set-leader-keys "oua" 'insert-greek-small-alpha)
(spacemacs/set-leader-keys "ouA" 'insert-greek-capital-alpha)

(defun insert-greek-small-beta ()
  (interactive)
  (insert-greek "small" "beta"))
(defun insert-greek-capital-beta ()
  (interactive)
  (insert-greek "capital" "beta"))
(spacemacs/set-leader-keys "oub" 'insert-greek-small-beta)
(spacemacs/set-leader-keys "ouB" 'insert-greek-capital-beta)

(defun insert-greek-small-gamma ()
  (interactive)
  (insert-greek "small" "gamma"))
(defun insert-greek-capital-gamma ()
  (interactive)
  (insert-greek "capital" "gamma"))
(spacemacs/set-leader-keys "oug" 'insert-greek-small-gamma)
(spacemacs/set-leader-keys "ouG" 'insert-greek-capital-gamma)

(defun insert-greek-small-delta ()
  (interactive)
  (insert-greek "small" "delta"))
(defun insert-greek-capital-delta ()
  (interactive)
  (insert-greek "capital" "delta"))
(spacemacs/set-leader-keys "oud" 'insert-greek-small-delta)
(spacemacs/set-leader-keys "ouD" 'insert-greek-capital-delta)

(defun insert-greek-small-theta ()
  (interactive)
  (insert-greek "small" "theta"))
(defun insert-greek-capital-theta ()
  (interactive)
  (insert-greek "capital" "theta"))
(spacemacs/set-leader-keys "outh" 'insert-greek-small-theta)
(spacemacs/set-leader-keys "ouTh" 'insert-greek-capital-theta)

(defun insert-greek-small-lambda ()
  (interactive)
  (insert-greek "small" "lambda"))
(defun insert-greek-capital-lambda ()
  (interactive)
  (insert-greek "capital" "lambda"))
(spacemacs/set-leader-keys "oul" 'insert-greek-small-lambda)
(spacemacs/set-leader-keys "ouL" 'insert-greek-capital-lambda)

(defun insert-greek-small-mu ()
  (interactive)
  (insert-greek "small" "mu"))
(defun insert-greek-capital-mu ()
  (interactive)
  (insert-greek "capital" "mu"))
(spacemacs/set-leader-keys "oum" 'insert-greek-small-mu)
(spacemacs/set-leader-keys "ouM" 'insert-greek-capital-mu)

(defun insert-greek-small-nu ()
  (interactive)
  (insert-greek "small" "nu"))
(defun insert-greek-capital-nu ()
  (interactive)
  (insert-greek "capital" "nu"))
(spacemacs/set-leader-keys "oun" 'insert-greek-small-nu)
(spacemacs/set-leader-keys "ouN" 'insert-greek-capital-nu)

(defun insert-greek-small-rho ()
  (interactive)
  (insert-greek "small" "rho"))
(defun insert-greek-capital-rho ()
  (interactive)
  (insert-greek "capital" "rho"))
(spacemacs/set-leader-keys "our" 'insert-greek-small-rho)
(spacemacs/set-leader-keys "ouR" 'insert-greek-capital-rho)

(defun insert-greek-small-sigma ()
  (interactive)
  (insert-greek "small" "sigma"))
(defun insert-greek-capital-sigma ()
  (interactive)
  (insert-greek "capital" "sigma"))
(spacemacs/set-leader-keys "ous" 'insert-greek-small-sigma)
(spacemacs/set-leader-keys "ouS" 'insert-greek-capital-sigma)

(defun insert-greek-small-psi ()
  (interactive)
  (insert-greek "small" "psi"))
(defun insert-greek-capital-psi ()
  (interactive)
  (insert-greek "capital" "psi"))
(spacemacs/set-leader-keys "oups" 'insert-greek-small-psi)

(defun insert-greek-small-omega ()
  (interactive)
  (insert-greek "small" "omega"))
(defun insert-greek-capital-omega ()
  (interactive)
  (insert-greek "capital" "omega"))
(spacemacs/set-leader-keys "ouo" 'insert-greek-small-omega)

(files--ensure-directory "~/.emacs.d/private/snippets/ess-r-mode")
(files--ensure-directory "~/.emacs.d/private/snippets/json-mode")
(files--ensure-directory "~/.emacs.d/private/snippets/web-mode")
(files--ensure-directory "~/.emacs.d/private/snippets/org-mode")

(defvaralias
  'helm-c-yas-space-match-any-greedy
  'helm-yas-space-match-any-greedy
  "Temporary alias for Emacs27")
