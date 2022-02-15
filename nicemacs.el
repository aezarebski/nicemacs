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

(setenv "PATH" (concat (getenv "PATH") "/home/aez/.nvm/versions/node/v17.3.1/bin"))
(setq exec-path (append exec-path '("/home/aez/.nvm/versions/node/v17.3.1/bin")))

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

(add-hook 'LaTeX-mode-hook 'variable-pitch-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)

(custom-set-faces
 '(font-lock-keyword-face ((t (:inherit fixed-pitch))))
 '(font-latex-sectioning-2-face ((t (:inherit bold :foreground "#3a81c3" :height 1.3 :family "Noto Sans"))))
 '(font-latex-sectioning-3-face ((t (:inherit bold :foreground "#2d9574" :height 1.2 :family "Noto Sans")))))

(spacemacs/declare-prefix "oo" "org-menu")

(spacemacs/declare-prefix "oot" "org-toggle-menu")

(require 'oc-csl)

(setq org-hide-emphasis-markers t)

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
'(org-link ((t (:underline t))))
'(org-meta-line ((t (:inherit fixed-pitch))))
'(org-property-value ((t (:inherit fixed-pitch))) t)
'(org-special-keyword ((t (:inherit fixed-pitch))))
'(org-table ((t (:inherit fixed-pitch))))
'(org-tag ((t (:inherit fixed-pitch))))
'(org-verbatim ((t (:inherit fixed-pitch))))
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

(spacemacs/declare-prefix "os" "sheila-menu")

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

(spacemacs/set-leader-keys "osb" 'shell)

(setq proced-auto-update-flag t)
(setq proced-auto-update-interval 1)

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

(defmacro nicemacs-commits (fname cmessage)
  (list 'defun (intern (format "ncf-%s" fname)) ()
        (list 'interactive)
        (list 'magit-commit-create `(list "--edit" ,(list 'format "-m %s %s" cmessage (list 'downcase (list 'format-time-string "%A %l:%M %p")))))))

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
(spacemacs/declare-prefix "of" "file-stuff")
(spacemacs/set-leader-keys "off" 'find-file-at-point)
(spacemacs/set-leader-keys "ofp" 'helm-projectile-find-file)

(spacemacs/set-leader-keys "ofv" 'view-file)
(spacemacs/set-leader-keys "ofl" 'find-file-literally)

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
(nicemacs-visit latex-notes "LaTeX notes" "/home/aez/public-site/org/notes/latex-notes.org")
(nicemacs-visit nix-notes "Nix notes" "/home/aez/public-site/org/notes/nix-notes.org")
(nicemacs-visit python-notes "Python notes" "/home/aez/public-site/org/notes/python-notes.org")
(nicemacs-visit r-notes "R notes" "/home/aez/public-site/org/notes/r-notes.org")
(nicemacs-visit ubuntu-notes "Ubuntu/Linux notes" "/home/aez/public-site/org/notes/linux-notes.org")
(nicemacs-visit org-mode-notes "org-mode notes" "/home/aez/public-site/org/notes/org-mode-notes.org")
(nicemacs-visit reading-list "Reading list" "/home/aez/Documents/bibliography/review/reading-list.tex")
(nicemacs-visit review-references "Bibtex references" "/home/aez/Documents/bibliography/references.bib")
(nicemacs-visit review-phylodynamics "Literature review: Phylodynamics" "/home/aez/Documents/bibliography/review/phylodynamics.tex")
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
  "ovnl" 'nvf-latex-notes
  "ovnn" 'nvf-nix-notes
  "ovno" 'nvf-org-mode-notes
  "ovnp" 'nvf-python-notes
  "ovnr" 'nvf-r-notes
  "ovns" 'nvf-statistics-notes
  "ovnu" 'nvf-ubuntu-notes
  "ovp" 'nvf-professional
  "ovrr" 'nvf-review-references
  "ovrp" 'nvf-review-phylodynamics
  "ovs" 'nvf-spelling
  "ovw" 'nvf-website)

(defun nsg-notes ()
  (interactive)
  (let ((search-terms (read-string "Search term: ")))
    (progn
      (message search-terms)
      (rgrep search-terms "*.org" "/home/aez/public-site/org/notes/"))))

(spacemacs/declare-prefix "oS" "Search")

(spacemacs/set-leader-keys
  "oSn" 'nsg-notes)

(spacemacs/set-leader-keys "ofb" 'ibuffer)
;; Open Ibuffer in the motion state rather than as the default emacs mode.
(evil-set-initial-state 'ibuffer-mode 'motion)

(defun message-buffer-file-name ()
  "Print the full path of the current buffer and store this on the kill ring."
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

(spacemacs/declare-prefix "ou" "unicode-stuff")

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

(files--ensure-directory "~/.emacs.d/private/snippets/ess-r-mode")
(files--ensure-directory "~/.emacs.d/private/snippets/python-mode")
