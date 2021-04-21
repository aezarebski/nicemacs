(defun visit-nicemacs ()
  (interactive)
  (find-file "~/Documents/nicemacs/README.org"))
(spacemacs/set-leader-keys "oon" 'visit-nicemacs)

(spacemacs/set-leader-keys "tt" 'spacemacs/cycle-spacemacs-theme)

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
(defun haskell-insert-language-pragma ()
  "Insert \"{-# LANGUAGE <point here!> #-}\""
  (interactive)
  (progn
    (insert "{-# LANGUAGE #-}")
    (evil-backward-char 3)))
(spacemacs/set-leader-keys "ohl" 'haskell-insert-language-pragma)

;; pretty printing the code with hindent (requires executable to be on path)
(spacemacs/set-leader-keys "ohhr" 'hindent-reformat-region)
(spacemacs/set-leader-keys "ohhb" 'hindent-reformat-buffer)

;; Send the current buffer to the REPL
(spacemacs/set-leader-keys "ohb" 'haskell-process-load-file)

;; Switch to the haskell REPL.
(spacemacs/set-leader-keys "ohg" 'haskell-interactive-switch)

;; Set the input method to TeX for using unicode. Use C-\ to unset this.
(spacemacs/set-leader-keys "ohu" 'set-input-method)

;; Go to the next error found by flycheck
(spacemacs/set-leader-keys "ohe" 'flycheck-next-error)

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
  "Opens the most recent bibtex file in the Downloads directory
in a new buffer."
  (interactive)
  (let* ((bib-files (directory-files-and-attributes "~/Downloads" t ".*bib" "ctime"))
         (path-and-time (lambda (x) (list (first x) (eighth x))))
         (time-order (lambda (a b) (time-less-p (second b) (second a))))
         (most-recent (lambda (files) (car (car (sort (mapcar path-and-time files) time-order))))))
   (find-file (funcall most-recent bib-files))))

(defun bibtex-braces ()
  "Wrap upper case letters with brackets for bibtex titles."
  (interactive)
  (evil-ex "'<,'>s/\\([A-Z]+\\)/\\{\\1\\}/g"))

(defun bibtex-ris2bib ()
  "Convert the most recent RIS file in my downloads to a BIB
file. TODO Make this less ugly please!"
  (interactive "*")
  (let ((ris-filepath (nth 1 (car (sort (mapcar (lambda (fp) (list (time-convert (file-attribute-modification-time (file-attributes fp)) 'integer) fp)) (directory-files "~/Downloads" 1 ".*ris")) (lambda (x y) (> (car x) (car y))))))))
    (shell-command (format "ris2xml %s | xml2bib > /home/aez/Downloads/new.bib" ris-filepath))))

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

(spacemacs/declare-prefix "oo" "orgo-menu")

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

(defun nicemacs-visit-journal ()
  "Opens the current journal file."
  (interactive)
  (let ((agenda-file (nicemacs-journal-filepath)))
    (find-file agenda-file)
    (goto-char 1)
    (recenter-top-bottom)))

(defun nicemacs-visit-agenda ()
  "Opens the agenda after checking it has been set correctly."
  (interactive)
  (let ((agenda-file (nicemacs-journal-filepath)))
    (org-agenda-list)))

(spacemacs/set-leader-keys "ooj" 'nicemacs-visit-journal)
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
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|txt\\|cur\\|svg\\|csv\\|json"
         :publishing-directory "~/aezarebski.github.io/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("org" :components ("org-notes" "org-static"))
        ))

(defun publish-my-site ()
  "Publish site and open version control for the published site."
  (interactive)
  (org-publish "org")
  (magit-status "~/aezarebski.github.io"))

(spacemacs/set-leader-keys "oop" 'publish-my-site)

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

(add-to-list 'org-link-frame-setup '(file . find-file))

(spacemacs/declare-prefix "os" "sheila-menu")

(spacemacs/set-leader-keys "osh" 'helm-eshell-history)

(spacemacs/set-leader-keys "osb" 'shell)

(defun eshell-aliases ()
  "Visit the file containing the eshell aliases."
  (interactive)
  (find-file-other-window eshell-aliases-file))

(require 'em-alias)
(eshell/alias "cdk" "cd ..")
(eshell/alias "cdkk" "cd ../..")
(eshell/alias "cdkkk" "cd ../../")
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
  (if
      (equal case-name "small")
      (insert (char-from-name (format "GREEK SMALL LETTER %s" (upcase letter-name))))
      (insert (char-from-name (format "GREEK CAPITAL LETTER %s" (upcase letter-name))))))

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

(defvaralias
  'helm-c-yas-space-match-any-greedy
  'helm-yas-space-match-any-greedy
  "Temporary alias for Emacs27")
