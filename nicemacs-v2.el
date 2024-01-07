;; [[file:nicemacs-v2.org::*Preamble][Preamble:1]]
;;; Nicemacs.v2 -*- lexical-binding: t -*-
;;; ==================================================================
;;
;;   ███╗   ██╗██╗ ██████╗███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;   ████╗  ██║██║██╔════╝██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;   ██╔██╗ ██║██║██║     █████╗  ██╔████╔██║███████║██║     ███████╗
;;   ██║╚██╗██║██║██║     ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;   ██║ ╚████║██║╚██████╗███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;   ╚═╝  ╚═══╝╚═╝ ╚═════╝╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;
;; To update your packages carry out the following steps:
;;
;; 1. M-x list-packages
;; 2. Press `r' to refresh
;; 3. Press `U' to mark upgradable packages
;; 4. Press `x' to execute the upgrades
;;
;;
;;  Maybe just: don't neglect the Elisp manual.  I've read it _far_
;;  more than the regular Emacs manual, and it's surprisingly fun.
;;  Seeing Emacs as an interactive lisp machine to be programmed
;;  rather than an editor to be configured makes many things easier.
;;
;;  ~ Tim Vaughan (2023)
;;
;; Packages used
;; -------------
;;
;; - `cl-lib'
;; - `copilot'
;; - `dired'
;; - `editorconfig' EditorConfig Emacs Plugin
;; - `ess' Emacs Speaks Statistics
;; - `evil' Extensible Vi layer for Emacs.
;; - `evil-collection' A set of keybindings for Evil mode
;; - `evil-leader' let there be <leader>
;; - `evil-mc'
;; - `evil-mc-extras' Extra functionality for evil-mc
;; - `evil-surround'
;; - `flyspell'
;; - `hl-todo' Highlight TODO and similar keywords
;; - `htmlize' Convert buffer text and decorations to HTML.
;; - `ligature' Ligature support for Emacs.
;; - `lorem-ipsum' Insert dummy pseudo Latin text
;; - `magit' A Git porcelain inside Emacs.
;; - `markdown-mode' Major mode for Markdown-formatted text
;; - `multiple-cursors' Multiple cursors for Emacs.
;; - `pyvenv' Python virtual environment interface
;; - `quarto-mode' A (poly)mode for https://quarto.org
;; - `rainbow-mode' Colorize color names in buffers
;; - `realgud' A front-end for interacting debuggers
;; - `s' The long lost Emacs string manipulation library.
;; - `unfill' Do the opposite of fill-paragraph or fill-region
;; - `which-key' Display available keybindings in popup
;; - `writegood-mode' Polish up poor writing on the fly
;; - `yasnippet' Yet another snippet extension for Emacs
;; - `yasnippet-snippets' Collection of yasnippet snippets
;;
;; Changelog
;; ---------
;;
;; - 2024-01
;;   + Add `python' to the list of languages used by Babel.
;;
;; - 2023-12
;;   + Abbreviate the eshell prompt in a nice way.
;;   + Include emoji keybinding amount yasnippet bindings since they
;;     play a similar role.
;;
;; - 2023-11
;;   + Refactor window selection.
;;   + Configure size of latex previews in org-mode.
;;   + Clean up snippet configuration.
;;
;; - 2023-10
;;   + Add a scrartcl class to `org-latex-classes' as nicer org-mode
;;     PDF export option.
;;
;; - 2023-09
;;   + Use `evil-window-X' functions as a simpler alternative to
;;     `winum'.
;;   + Use `IBuffer' to manage buffers.
;;   + Use `avy' to jump to lines in a buffer using a character.
;;
;; - 2023-08
;;   + Use `indent-guide' to visualise indentation in `python-mode'.
;;   + Use `org-agenda-window-setup' to open agenda in a new frame.
;;   + Use `org-log-done' to record when tasks where completed.
;;   + Upgrade to Emacs 29.1 and enable smooth scrolling.
;;
;; - 2023-07
;;   + Re-organised the themeing and started Leuven.
;;   + Isolate theme configuration so it is easier to edit.
;;   + Set up debugging with `realgud' (for Python).
;;   + Improve window management.
;;   + Set up a major mode for MATLAB.
;;   + Introduce a new org-mode todo states.
;;   + Avoid annoying warning from `git' by setting `GIT_PAGER' to
;;     `cat'.
;;
;; - 2023-06
;;   + Include the `cdf' function for easier eshell navigation.
;;   + Include keys for ESS devtools integration.
;;   + Include wrapper around `pp-emacs-lisp-code'.
;;   + Add some configuration for `nxml-mode'.
;;   + Start using `polymode' (and friends) so I can write R with from
;;     within `org-mode'.
;;   + Configure the `fill-column' to use a clearer face.
;;
;; - 2023-05
;;   + Use the `use-package' macro.
;;   + Use the `calfw' package for a calendar view of my agenda
;;   + Remove racket/scheme pacakge
;;   + Include option to accept AI suggestions line by line.
;;   + Include a linter function for R (using `formatR').
;;   + Use the `unfill' package for better paragraph un/filling.
;;   + Use the `lorem-ipsum' package for convenient dummy text.
;;   + Include python configuration.
;;
;; - 2023-04
;;   + Set up an org-mode file for documentation with tangling and
;;     detangling.
;;   + Use `S <x>` in visual mode to surround the region in <x>.
;;   + Use `SPC b s <x>` to open a scratch buffer in mode<x>
;;   + Edit `message-buffer-file-name' so it works in `dired'.
;;   + Extend `before-save-hook' to avoid accidental trailing
;;     whitespace.
;;   + Use JetBrains Mono as the font with ligatures.
;;
;;; ==================================================================
;; Preamble:1 ends here

;; [[file:nicemacs-v2.org::*STUFF 1][STUFF 1:1]]
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(setq user-full-name "Alexander E. Zarebski")

(defvar nice-journal-directory "~/Documents/journal/"
  "The directory for nicemacs journal files.")
(defvar nice-notes-directory "~/public-site/org/notes"
  "The directory for nicemacs notes files.")
(defvar nice-resources-dir "~/Documents/nicemacs/resources"
  "The path to the nicemacs resources directory on this machine.")
(defvar nice-nicemacs-directory "~/Documents/nicemacs"
  "The path to the nicemacs directory on this machine.")
(defvar nice-snippet-directory "~/.emacs.d/snippets"
  "The path to the whipper snipper directory on this machine.")

(use-package cl-lib
  :ensure t)
;; STUFF 1:1 ends here

;; [[file:nicemacs-v2.org::*Evil][Evil:1]]
;; Be evil
;; -------
;;
;; Evil surroundings
;;
;; 1. Enter visual mode and select the text as the region.
;; 2. Press `S'.
;; 3. Type the symbol to surround it (note, if it is part of a opening
;;    and closing pair, the opening includes a space and the closing
;;    does not.)
;;

(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :init
  (evil-mode 1))

(use-package evil-leader
  :ensure t
  :config
  (evil-leader-mode 1)
  (global-evil-leader-mode 1)
  (evil-leader/set-key "t s" 'evil-surround-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key "<SPC>" 'execute-extended-command))

(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package avy
  :ensure t
  :config
  (evil-leader/set-key "J" 'avy-goto-line))
;; Evil:1 ends here

;; [[file:nicemacs-v2.org::*Fonts][Fonts:1]]
;; Fonts
;; -----
;;
;; To install JetBrains Mono, or any other font, follow these steps:
;;
;; 1. Download and extract the font, you should have a "ttf" directory
;;    containing the font files.
;; 2. Create a font directory if you don't already have one
;;    $ mkdir -p ~/.local/share/fonts
;; 3. Copy the font files to the font directory:
;;    $ cp path/to/extracted/ttf/*.ttf ~/.local/share/fonts
;; 4. Update the font cache:
;;    $ fc-cache -f -v
;;
(set-frame-font "JetBrains Mono" nil t)
(ligature-set-ligatures 'prog-mode '("|>" "<-" "<<-" "==" "!=" ">=" "<="))
(global-ligature-mode nil)

(defun toggle-ligatures ()
  "Toggle ligatures on and off."
  (interactive)
  (if (bound-and-true-p global-ligature-mode)
      (global-ligature-mode -1)
    (global-ligature-mode 1)))
(evil-leader/set-key "t l" 'toggle-ligatures)
;; Fonts:1 ends here

;; [[file:nicemacs-v2.org::*General][General:1]]
;; Look stunning
;; -------------
;;
;; `pixel-scroll-precision-mode' means you can have smooth scrolling
;; if you have a compatible mouse.
;;

(tool-bar-mode -1)			; remove the tool bar

(pixel-scroll-precision-mode 1)
(setq pixel-dead-time 0)

(setq scroll-margin 2
      scroll-conservatively 101
      scroll-preserve-screen-position 1)

(setq-default scroll-bar-width 10)
(setq-default left-fringe-width 10)
(setq-default right-fringe-width 10)

;; I dislike trailing whitespace creeping into my files so the
;; following will make it visible and automatically remove it upon
;; saving. NOTE setting `show-trailing-whitespace' globally leads to
;; some things being highlighted in other buffers such as `calendar'
;; where they should not be hightlight. Doing it with
;; `nice-show-trailing-whitespace' ensures it is set locally as
;; appropriate.
(defun nice-show-trailing-whitespace ()
  "Enable trailing whitespace highlighting only when editing a file."
  (setq show-trailing-whitespace (buffer-file-name)))
(add-hook 'find-file-hook 'nice-show-trailing-whitespace)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode)
  (global-hl-line-mode t))

(defun boxed-face (colour &optional background line-width)
  "Create a face with a specified foreground COLOUR and optional BACKGROUND.

   If LINE-WIDTH is not specified, it defaults to 1.
   This face will be bold and boxed with the same colour as the foreground."
  (let ((width (or line-width 1)))
    `((t (:foreground ,colour
	  :weight bold
	  :background ,background
	  :box (:line-width ,width
		:color ,colour))))))

(setq hl-todo-keyword-faces
      `(("TODO"   . ,(boxed-face "red" "#ffc8c8"))
	("FIXME"  . ,(boxed-face "magenta"))
	("NOTE"   . ,(boxed-face "cyan"))
	("DONE"   . ,(boxed-face "blue" "#E6ECFF"))))

(setq fill-column 70)

(defun nice-toggle-fill-column-indicator ()
  "Toggle display of the fill column indicator.

When active, the indicator is set to a vertical line. It also
turns on `display-fill-column-indicator-mode' if it's not already
active, and turns it off if it is."
  (interactive)
  (display-fill-column-indicator-mode 'toggle)
  (when display-fill-column-indicator-mode
    (setq display-fill-column-indicator-character ?\u2502)
    (set-face-attribute 'fill-column-indicator nil
			:foreground "magenta"
			:weight 'bold)))

(evil-leader/set-key "t f" 'nice-toggle-fill-column-indicator)
;; General:1 ends here

;; [[file:nicemacs-v2.org::*Theme: Leuven][Theme: Leuven:1]]
(setq nice-light-theme 'leuven
      nice-dark-theme 'leuven-dark)

(load-theme nice-light-theme t)

(defun nice-toggle-theme ()
  "Toggle between my light and dark themes."
  (interactive)
  (if (eq (car custom-enabled-themes) nice-light-theme)
      (progn
	(disable-theme nice-light-theme)
	(load-theme nice-dark-theme t))
    (progn
      (disable-theme nice-dark-theme)
      (load-theme nice-light-theme t))))

(evil-leader/set-key "t t" 'nice-toggle-theme)
;; Theme: Leuven:1 ends here

;; [[file:nicemacs-v2.org::*Other][Other:1]]

;; Rainbow-mode will highlight strings indicating colours,
;; e.g. hexcodes in their corresponding colour.
(use-package rainbow-mode
  :ensure t
  :hook ((emacs-lisp-mode . rainbow-mode)
	 (ess-mode . rainbow-mode)))

(setq inhibit-splash-screen t)

(evil-leader/set-key
  "z j" 'text-scale-decrease
  "z k" 'text-scale-increase)

;; Be sensible
;; -----------

(use-package unfill
  :ensure t
  :bind ("M-q" . unfill-toggle))

(evil-leader/set-key
  "q r" 'restart-emacs
  "q q" 'save-buffers-kill-emacs)

;; Frame related commands will have keys starting with `F'.
(evil-leader/set-key "F f" 'toggle-frame-fullscreen)

(defun nice-pop-out-window ()
  "Pop the current window out into a new frame.

If there is only a single window then do nothing because it is
already in its own frame."
  (interactive)
  (unless (one-window-p)
    (let ((current-buffer (current-buffer)))
      (delete-window)
      (display-buffer-pop-up-frame current-buffer nil))))

(evil-leader/set-key "F p" 'nice-pop-out-window)
;; Other:1 ends here

;; [[file:nicemacs-v2.org::*Which-key][Which-key:1]]
;; The which-key package is a great way to be reminded of what keys
;; are available from the start of a key sequence.
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(setq key-description-pairs
      '(("SPC a" . "Agenda (org-mode)")
	("SPC b" . "Buffers/Babel")
	("SPC c" . "Cursors")
	("SPC c" . "Delete")
	("SPC f" . "Files/Dired")
	("SPC F" . "Frame")
	("SPC g" . "Git (magit)")
	("SPC g c" . "Commits")
	("SPC H" . "HELP!!!")
	("SPC m" . "Major")
	("SPC m v" . "EnVironment")
	("SPC m d" . "devtools (ESS)")
	("SPC m s" . "REPL (prog)/Sort (dired)")
	("SPC m c" . "Code lint/format")
	("SPC q" . "Quit/Exit")
	("SPC s" . "Shell/Search")
	("SPC S" . "Spelling")
	("SPC t" . "Toggles")
	("SPC v" . "Visitors")
	("SPC v b" . "Bibtex")
	("SPC v f" . "Files")
	("SPC v d" . "Directories")
	("SPC w" . "Windows")
	("SPC y" . "Yasnippet")
	("SPC z" . "Zoom (without a mouse)")))

(dolist (pair key-description-pairs)
  (which-key-add-key-based-replacements (car pair) (cdr pair)))
;; Which-key:1 ends here

;; [[file:nicemacs-v2.org::*Diff-ing files][Diff-ing files:1]]
;; Diffing files with meld
;; -----------------------
;;
;; + f m a - will diff the aspell dictionary
;; + f m i - will diff the emacs init
;; + f m m - will prompt for two files to diff
;;
(defmacro nice-meld-files (name fa fb key)
  "Generate function named nice-meld-NAME which opens meld diff for
files FA and FB using SPC f m KEY."
  `(progn
     (defun ,(intern (format "nice-meld-%s" name)) ()
       (interactive)
       (async-shell-command ,(format "meld %s %s &" fa fb)))
     (evil-leader/set-key ,(concat "f m " key) (intern ,(format "nice-meld-%s" name)))))

(nice-meld-files "init" "~/.emacs.d/init.el"
		 "~/Documents/nicemacs/nicemacs-v2.el"
		 "i")
(nice-meld-files "aspell" "~/.aspell.en.pws"
		 "~/Documents/nicemacs/resources/aspell.en.pws"
		 "a")

(defun nice-meld ()
  "Prompt for two files and show the difference between them using
`meld`."
  (interactive)
  (let ((file1 (read-file-name "First file: "))
	(file2 (read-file-name "Second file: ")))
    (shell-command (format "meld %s %s &" file1 file2))))

(evil-leader/set-key "f m m" 'nice-meld)
;; Diff-ing files:1 ends here

;; [[file:nicemacs-v2.org::*Evil window management][Evil window management:1]]
(defmacro define-nice-window-move (name move-func)
  `(defun ,name ()
     (interactive)
     (,move-func 1)
     (let ((ov (make-overlay (point-min) (point-max))))
       (overlay-put ov 'window (selected-window))
       (overlay-put ov 'face '(:background "magenta"))
       (sit-for 0.1)
       (delete-overlay ov))))

(define-nice-window-move nice-window-up evil-window-up)
(define-nice-window-move nice-window-down evil-window-down)
(define-nice-window-move nice-window-left evil-window-left)
(define-nice-window-move nice-window-right evil-window-right)

(evil-leader/set-key
  "k" 'nice-window-up
  "j" 'nice-window-down
  "h" 'nice-window-left
  "l" 'nice-window-right
  "w a" 'nice-balance-windows-alt
  "w b" 'balance-windows
  "w s" 'split-window-below
  "w v" 'split-window-right
  "w L" 'evil-window-move-far-right
  "w H" 'evil-window-move-far-left
  "w J" 'evil-window-move-very-bottom
  "w K" 'evil-window-move-very-top)

(defun nice-balance-windows-alt ()
  "Balance windows such that the current window receives a certain
amount of the of the frame's width and height."
  (interactive)
  (balance-windows)
  (let* ((proportion 0.7)
	 (frame-width (frame-width))
	 (frame-height (frame-height))
	 (desired-width (floor (* proportion frame-width)))
	 (desired-height (floor (* proportion frame-height))))
    (enlarge-window-horizontally (- desired-width (window-width)))
    (enlarge-window (- desired-height (window-height)))))
;; Evil window management:1 ends here

;; [[file:nicemacs-v2.org::*Shells][Shells:1]]
;; Shell stuff
;; -----------
;;
(defun nice-eshell ()
  "Open an existing or new eshell buffer in a vertical split."
  (interactive)
  (let ((eshell-buffer (get-buffer "*eshell*"))
        (width (/ (frame-width) 2)))
    (split-window-horizontally)
    (other-window 1)
    (window-resize nil (- width (window-width)) t)
    (if eshell-buffer
        (switch-to-buffer eshell-buffer)
      (eshell))))

(defun nice-eshell-prompt ()
  (let* ((directory (abbreviate-file-name (eshell/pwd)))
         (parent (file-name-directory directory))
         (name (file-name-nondirectory directory))
         (base-prompt (concat (if parent
                                  (concat "... " (file-name-nondirectory (directory-file-name parent)) "/")
                                "")
                              name
                              " $ "))
         (trimmed-prompt (if (> (length base-prompt) 50)
                             (concat "[...] " (substring base-prompt (- (length base-prompt) 44)))
                           base-prompt)))
    (if (string-match-p "~" trimmed-prompt)
        (replace-regexp-in-string "^\\.\\.\\. " "" trimmed-prompt)
      trimmed-prompt)))

(setq eshell-prompt-function 'nice-eshell-prompt)

(setq eshell-cmpl-ignore-case t)
(evil-leader/set-key
  "s e" 'eshell
  "s b" (lambda () (interactive) (ansi-term "/bin/bash"))
  "s i" 'ielm
  "s r" 'R
  "'" 'nice-eshell)

(defun cdf (filepath)
  "Change the current directory in Eshell to the directory of
 FILEPATH."
  (let ((dir (file-name-directory filepath)))
    (when (file-directory-p dir)
      (eshell/cd dir))))

(defun nice-eshell-mode-setup ()
  (setenv "TERM" "dumb")
  (setenv "GIT_PAGER" "cat"))

(add-hook 'eshell-mode-hook 'nice-eshell-mode-setup)
;; Shells:1 ends here

;; [[file:nicemacs-v2.org::*Dired][Dired:1]]
;; Dired
;; -----
;;
;; - R :: mv
;; - C :: cp
;; - + :: mkdir
;; - - :: cd ../
;; - m :: mark a file
;; - u :: unmark a file
;; - d :: flag file for deletion
;; - x :: execute deletion
;;
(use-package dired
  :bind (:map dired-mode-map
	      ("-" . dired-up-directory))
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-dwim-target t)
  (evil-leader/set-key-for-mode 'dired-mode "m s" 'dired-sort-toggle-or-edit))
;; Dired:1 ends here

;; [[file:nicemacs-v2.org::*Buffers, files, and dired][Buffers, files, and dired:1]]
;; Buffer stuff
;; ------------

(evil-leader/set-key
  "b r" 'revert-buffer
  "b l" 'ibuffer)

(defface ibuffer-modified-buffer
  '((t (:foreground "white"
	:weight bold
	:background "red")))
  "Face used for highlighting unsaved buffers in IBuffer.")

;; Declare that IBuffer should use the `ibuffer-modified-buffer' face
;; for modified buffers so that they stand out.
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")
            (add-to-list 'ibuffer-fontification-alist
                         '(0 (buffer-modified-p) 'ibuffer-modified-buffer))))

;; File stuff
;; ----------

(evil-leader/set-key
  "f f" 'find-file
  "f l" 'find-file-literally
  "f t" 'nice-touch-file
  "f F" 'find-file-other-frame
  "f s" 'save-buffer
  "f d" 'nice-dired
  "b b" 'switch-to-buffer
  "d b" 'kill-buffer
  "d w" 'delete-window
  "d F" 'delete-frame
  "F d" 'delete-frame)

(defun nice-dired ()
  "Open dired for the current buffer's directory if it
 corresponds to a file, the working directory of the shell if
 the current buffer is a shell, or the home directory otherwise."
  (interactive)
  (let* ((buffer-mode (with-current-buffer (current-buffer) major-mode))
	 (dir (cond ((buffer-file-name)
		     (file-name-directory (buffer-file-name)))
		    ((or (eq buffer-mode 'term-mode)
			 (eq buffer-mode 'eshell-mode)
			 (eq buffer-mode 'inferior-ess-r-mode))
		     (with-current-buffer (if (eq buffer-mode 'inferior-ess-r-mode)
					      (process-buffer (ess-get-process ess-current-process-name))
					    (current-buffer))
		       (file-name-directory default-directory)))
		    (t (expand-file-name "~/")))))
    (dired dir)))

(defun nice-touch-file ()
  "In the current dired buffer touch a new file with a name
retreived from the prompt."
  (interactive)
  (if (not (eq major-mode 'dired-mode))
      (error "Not in dired mode"))
  (let ((filename (read-string "Filename: ")))
    (shell-command (format "touch %s" filename))
    (revert-buffer)))

(defmacro nice-scratch-buffer (mode key)
  "Create a nice-scratch-buffer function for MODE and bind it to KEY."
  (let ((func-name (intern (format "nice-scratch-buffer-%s" (symbol-name mode))))
	(docstring (format "Open the scratch buffer and set the major mode to `%s'." mode)))
    `(progn
       (defun ,func-name ()
	 ,docstring
	 (interactive)
	 (switch-to-buffer "*scratch*")
	 (,mode))
       (evil-leader/set-key ,key ',func-name))))
(nice-scratch-buffer text-mode "b s t")
(nice-scratch-buffer org-mode "b s o")
(nice-scratch-buffer emacs-lisp-mode "b s e")
;; Buffers, files, and dired:1 ends here

;; [[file:nicemacs-v2.org::*STUFF 2][STUFF 2:1]]
;; Consult the oracle
;; ------------------

(evil-leader/set-key
  "H s" 'apropos
  "H d b" 'message-buffer-file-name
  "H d f" 'describe-function
  "H d m" 'describe-mode
  "H d p" 'describe-package
  "H d k" 'describe-key
  "H d v" 'describe-variable)

(defun message-buffer-file-name ()
  "Print the full path of the current buffer's file or directory to the
minibuffer and store this on the kill ring."
  (interactive)
  (let ((path (or buffer-file-name
		  (and (eq major-mode 'dired-mode)
		       (dired-current-directory)))))
    (when path
      (kill-new path)
      (message path))))

(defun message-link-at-point ()
  "Print the full path of a link at the point so we know where this
will take us."
  (interactive)
  (let* ((link (org-element-context))
	 (link-file-name (org-element-property :path link)))
    (when (eq (org-element-type link) 'link)
      (kill-new link-file-name)
      (message "%s" link-file-name))))

(evil-leader/set-key "H l m" 'message-link-at-point)

;; Learn from your past
;; --------------------

(defmacro nice-rgrep-directory (dname path pattern key)
  "Create a function that calls `rgrep' on the specified DIRECTORY
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

(nice-rgrep-directory "website" "~/public-site/org" "*" "w")
(nice-rgrep-directory "notes" "~/public-site/org/notes" "*" "n")
(nice-rgrep-directory "journal" "~/Documents/journal" "*.org" "j")
(nice-rgrep-directory "reviews" "~/Documents/bibliography" "*" "r")

(evil-leader/set-key "s g ." (lambda ()
			       (interactive)
			       (rgrep (read-string "Search terms: ")
				      "*")))

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


;; Be powerful with packages
;; =========================

;; Obfuscate the text on the screen if there is no movement for 60
;; seconds.
(require 'zone)
(zone-when-idle 0)
(setq zone-programs [zone-pgm-whack-chars])
(evil-leader/set-key "z z" 'zone)


;; NXML
;; ----

;; u - up to parent.
;; p - previous tag.
;; n - next tag.
(evil-leader/set-key-for-mode 'nxml-mode
  "m u" 'nxml-backward-up-element
  "m p" 'nxml-backward-element
  "m n" 'forward-sexp)
;; STUFF 2:1 ends here

;; [[file:nicemacs-v2.org::*Yasnippet][Yasnippet:1]]
;; Yasnippet
;; ---------
;;
;; See https://github.com/aezarebski/whipper-snipper
;;

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(defun nice-load-snippets ()
  "Load the snippets in my snippet directory"
  (interactive)
  (let ((snippets-dir nice-snippet-directory))
    (unless (file-exists-p snippets-dir)
      (make-directory snippets-dir))
    (yas-load-directory snippets-dir)))

(nice-load-snippets)

(evil-leader/set-key
  "y i" 'yas-insert-snippet     ; Insert a snippet
  "y n" 'yas-new-snippet        ; Create a new snippet
  "y v" 'yas-visit-snippet-file ; Visit the snippet file for the current mode
  "y r" 'yas-reload-all         ; Reload all snippets
  "y c" 'yas-compile-directory  ; Compile all snippets
  "y l" 'nice-load-snippets     ; Load your custom snippets
  "y g" 'nice-go-to-snippets-dir
  "y e" 'emoji-list)

(defun nice-go-to-snippets-dir ()
  "Open the snippets directory in dired."
  (interactive)
  (dired nice-snippet-directory))
;; Yasnippet:1 ends here

;; [[file:nicemacs-v2.org::*STUFF 3][STUFF 3:1]]
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
;; 3. Use `evil-insert' (`SPC c i`) to start editing.
;; 4. Exit using `mc/keyboard-quit' (`SPC c q`)

(use-package multiple-cursors
  :ensure t)

(use-package evil-mc
  :ensure t
  :config (global-evil-mc-mode 1))

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
;; STUFF 3:1 ends here

;; [[file:nicemacs-v2.org::*Configuration][Configuration:1]]
;; Magit
;; -----
(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function
	#'magit-display-buffer-fullframe-status-v1)
  (setenv "SSH_AUTH_SOCK" "<ADD THE CORRECT PATH HERE>")
  (evil-leader/set-key
    "g s" 'nice/magit-status
    "g q" 'with-editor-cancel))
;; Configuration:1 ends here

;; [[file:nicemacs-v2.org::*Configuration][Configuration:2]]
(defvar nice/temp-window-configuration nil
  "Temporary variable to hold the window configuration.")

(defun nice/magit-status ()
  "Save the current window configuration and open Magit status."
  (interactive)
  (setq nice/temp-window-configuration (current-window-configuration))
  (magit-status))

(defun nice/magit-quit ()
  "Restore the window configuration from before opening Magit status."
  (interactive)
  (when nice/temp-window-configuration
    (set-window-configuration nice/temp-window-configuration)
    (setq nice/temp-window-configuration nil)))
;; Configuration:2 ends here

;; [[file:nicemacs-v2.org::*Configuration][Configuration:3]]
(defmacro nice-canned-commit-message (fname cmessage key)
  "Define a canned commit message function with an Evil key binding.

  This macro takes in three arguments:
  - FNAME: A string that will be used to construct the function name.
  - CMESSAGE: A string that represents the canned commit message.
  - KEY: A string that represents the keybinding for the function using the Evil leader.

  The function created by this macro generates a commit message with a timestamp by
  concatenating the specified CMESSAGE string with the current day and time. The commit
  is created using `magit-commit-create', which is invoked with the `--edit` option to
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

(nice-canned-commit-message emacs "update emacs config" "e")
(nice-canned-commit-message flashcards "flashcards" "f")
(nice-canned-commit-message journal "update journal" "j")
(nice-canned-commit-message notes "update notes" "n")
(nice-canned-commit-message review "update reading list" "r")
(nice-canned-commit-message website "update website" "w")
(nice-canned-commit-message yasnippet "yasnippet" "y")
;; Configuration:3 ends here

;; [[file:nicemacs-v2.org::*Emacs lisp][Emacs lisp:1]]
;; Emacs Lisp
;; ----------

(setq pp-max-width 70)
(setq pp-use-max-width t)

(defun pp-sexp-to-kill-ring ()
  "Pretty-print the S-expression under the cursor and add it to the
kill ring."
  (interactive)
  (let ((sexp (read (thing-at-point 'sexp)))
	(temp-buffer (generate-new-buffer "*temp*")))
    (with-current-buffer temp-buffer
      (pp-emacs-lisp-code sexp)
      (kill-new (buffer-string)))
    (kill-buffer temp-buffer)))

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "m s c" 'eval-last-sexp
  "m s b" 'eval-buffer
  "m s r" 'eval-region
  "m c l" 'pp-sexp-to-kill-ring)
;; Emacs lisp:1 ends here

;; [[file:nicemacs-v2.org::*Emacs Speaks Statistics (ESS)][Emacs Speaks Statistics (ESS):1]]
;; Emacs Speaks Statistics (ESS)
;; -----------------------------

(use-package ess
  :ensure t
  :init
  (setq ess-etc-directory "~/.emacs.d/nice-packages/etc")
  :mode ("\\.Rmd" . Rmd-mode)
  :config
  (setq ess-default-style 'DEFAULT
        ess-history-file nil)
  (evil-leader/set-key-for-mode 'ess-r-mode
    "m d t" 'ess-r-devtools-test-package
    "m d l" 'ess-r-devtools-load-package
    "m d b" 'ess-r-devtools-build
    "m d i" 'ess-r-devtools-install-package
    "m d c" 'ess-r-devtools-check-package
    "m d d" 'ess-r-devtools-document-package
    "m s b" 'ess-eval-buffer
    "m s r" 'ess-eval-region
    "m s c" 'ess-eval-region-or-line-visibly-and-step
    "m c l" 'nice-code-lint-buffer-r
    "m c i" 'indent-region
    "m '" 'ess-switch-to-inferior-or-script-buffer))

(use-package quarto-mode
  :ensure t)

(defun nice-code-lint-buffer-r ()
  "Lint the current R buffer using lintr."
  (interactive)
  (ess-eval-linewise "library(lintr)\n")
  (ess-eval-linewise (format "print(lint(\"%s\"))\n" buffer-file-name)))
;; Emacs Speaks Statistics (ESS):1 ends here

;; [[file:nicemacs-v2.org::*RealGUD debugging][RealGUD debugging:1]]
;; Debugging
;; ---------
;;
;; Commands
;;   - `n' next line
;;   - `s' step into expression
;;   - `c' continue
;;   - `l' list context
;;   - `p' print variable
;;   - `q' quit debugger
;;
;; Debug a Python script by
;;   1. adding `import pdb; pdb.set_trace()'
;;   2. running the script with `realgud:pdb'
;;

(use-package realgud
  :ensure t
  :config
  (setq realgud:pdb-command-name "python -m pdb"))
;; RealGUD debugging:1 ends here

;; [[file:nicemacs-v2.org::*Python][Python:1]]
;; Python
;; ------
;;
;; Use `pyvenv-activate' to activate a virtual environment.

(use-package pyvenv
  :ensure t)

(use-package python
  :ensure t
  :config
  (setq python-shell-interpreter "python3")
  (setq python-indent-offset 4))

(use-package indent-guide
  :ensure t
  :hook (python-mode . indent-guide-mode)
  :config
  (setq indent-guide-char "|")
  (setq indent-guide-recursive t))

(evil-leader/set-key-for-mode 'python-mode
  "m v a" 'pyvenv-activate
  "m s b" 'python-shell-send-buffer
  "m s r" 'python-shell-send-region
  "m '" 'python-shell-switch-to-shell)
;; Python:1 ends here

;; [[file:nicemacs-v2.org::*LaTeX/BibTeX][LaTeX/BibTeX:1]]
;; LaTeX/BibTeX
;; ------------

;; TODO Configure this so that there is a good way to search the key
;; bibtex files, perhaps with a SQL type search

;; TODO Find a better way to search BIB files.

(defun most-recent-file (files)
  "Return the most recent file from a list of FILES.
FILES should be a list of file paths as strings."
  (when (and files (seq-every-p #'stringp files))
    (cl-flet* ((file-mod-time (file)
		 (nth 5 (file-attributes file)))
	       (mod-time-less-p (a b)
		 (time-less-p (file-mod-time b)
			      (file-mod-time a))))
      (car (sort files #'mod-time-less-p)))))

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

(defun nice-ris2bib ()
  "Convert the most recent RIS file in my downloads to a BIB
file. Signal an error if there are no RIS files or if the
conversion fails."
  (interactive "*")

  (let* ((all-ris-files (directory-files "~/Downloads" t ".*ris"))
	 (ris-filepath (most-recent-file all-ris-files))
	 (target-bib "~/Downloads/new.bib")
	 (ris2xml-command (format "ris2xml \"%s\" | xml2bib > %s" ris-filepath
				  target-bib))
	 (command-result (shell-command ris2xml-command)))
    (unless ris-filepath
      (error "No RIS files found in the directory"))
    (unless (zerop command-result)
      (error "Conversion from RIS to BIB failed with error code: %s" command-result))))

(defun nice-bibtex-braces ()
  "Wrap upper case letters with brackets for bibtex titles within
the selected region."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
	    (end (region-end))
	    (case-fold-search nil))
	(save-excursion
	  (goto-char start)
	  (while (re-search-forward "\\([A-Z]+\\)" end t)
	    (replace-match (format "{%s}" (match-string 0)) t))))
    (message "No region selected.")))

(defun nice-bibtex-guess-key ()
  "Generate a new key for the current BibTeX entry based on author,
year, and the first two words of the title."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
	 (author (downcase (replace-regexp-in-string "," "" (car (split-string (bibtex-text-in-field "author"))))))
	 (year (bibtex-text-in-field "year"))
	 (title (bibtex-text-in-field "title"))
	     (first-two-words (when title
			(let ((split-title (split-string title)))
			  (if (>= (length split-title) 2)
			      (format "%s%s" (nth 0 split-title) (nth 1 split-title))
			    (car split-title))))))
    (if (and author year first-two-words)
	(let ((newkey (format "%s%s%s" author year first-two-words)))
	  (kill-new (replace-regexp-in-string "[{}]" "" newkey))
	  (evil-jump-item)
	  (message "New key generated and copied to clipboard: %s" newkey))
      (error "Author, Year or Title is missing in the current BibTeX entry."))))

(defun nice-browse-url-of-doi ()
  "Open the DOI of the current bibtex entry in the web browser."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let ((doi (bibtex-autokey-get-field "doi")))
      (if doi
	  (browse-url (concat "https://doi.org/" doi))
	(message "No DOI found for this entry")))))

(evil-leader/set-key
  "v b l" 'nice-visit-last-bib
  "v b d" 'nice-browse-url-of-doi
  "v b r" 'nice-ris2bib)

(evil-leader/set-key-for-mode 'bibtex-mode
  "m b b" 'nice-bibtex-braces
  "m b f" 'bibtex-reformat
  "m b k" 'nice-bibtex-guess-key)
;; LaTeX/BibTeX:1 ends here

;; [[file:nicemacs-v2.org::*LaTeX/BibTeX][LaTeX/BibTeX:2]]
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("scrartcl"
		 "\\documentclass{scrartcl}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
;; LaTeX/BibTeX:2 ends here

;; [[file:nicemacs-v2.org::*Markdown][Markdown:1]]
;; Markdown-mode
;; -------------

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)))
;; Markdown:1 ends here

;; [[file:nicemacs-v2.org::*Org-mode][Org-mode:1]]
;; Org-Mode
;; ========

;; NOTE It would be nice to have an additional command and key for
;; moving from level n+1 headers their parent level n header.

;; FIXME Work out why the configuration based approach does not work!
(setq org-return-follows-link t)
(evil-leader/set-key-for-mode 'org-mode "RET" 'org-open-at-point)

(defun nice-org-mode-hook ()
  "Set up org-mode specific keybindings."
  (local-set-key (kbd "<tab>") #'org-cycle))

(add-hook 'org-mode-hook #'nice-org-mode-hook)
;; Org-mode:1 ends here

;; [[file:nicemacs-v2.org::*Writing natural language][Writing natural language:1]]
;; Write well
;; ----------

;; TODO Configure the =dictionary= command so that it works off of a
;; local copy of Webster's

(setq sentence-end-double-space nil)

(use-package flyspell
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-personal-dictionary "~/.aspell.en.pws")
  (set-face-attribute 'flyspell-duplicate nil
		      :underline nil
		      :foreground "white"
		      :background "red")
  (set-face-attribute 'flyspell-incorrect nil
		      :underline nil
		      :foreground "white"
		      :background "red"))

(use-package lorem-ipsum)

(defun nice-diff-dictionaries ()
  "Run ediff on the current ispell-personal-dictionary and the
backup dictionary."
  (interactive)
  (let ((backup-dictionary
	 (concat nice-resources-dir "/aspell.en.pws")))
    (ediff-files ispell-personal-dictionary backup-dictionary)))

(evil-leader/set-key
  "t S" 'flyspell-mode ; toggle flyspell on/off.
  "S b" 'flyspell-buffer
  "S n" 'flyspell-goto-next-error
  "S r" 'flyspell-region
  "S c" 'flyspell-correct-word-before-point
  "S d" 'nice-diff-dictionaries)

(use-package writegood-mode)

(setq words-to-add
      '("many" "various" "very" "quite" "somewhat" "several"
	"extremely" "exceedingly" "fairly" "rather" "remarkably" "few"
	"surprisingly" "mostly" "largely" "almost" "nearly" "in which"
	"generally" "virtually" "essentially" "often" "substantially"
	"significantly" "considerably" "typically" "widely" "really"
	"actually" "basically" "certainly" "possibly" "probably"
	"arguably" "likely" "apparently" "clearly" "naturally"
	"obviously" "seemingly" "surely" "somewhat" "allegedly"
	"supposedly" "purportedly" "perhaps" "maybe" "kind of"
	"sort of" "potentially" "ultimately" "respectively"))
(cl-loop for word in words-to-add
	 unless (member word writegood-weasel-words)
	 do (add-to-list 'writegood-weasel-words word))

(evil-leader/set-key "t w" 'writegood-mode)

;; Formatting text
;; ---------------
;;
;; Some useful functions for writing in natural language.
;;
;; - nice-org-wrapped-lines
;; - nice-org-single-long-line
;; - nice-org-each-sentence-new-line
;;

(defun nice-org-wrapped-lines ()
  "Formats the current paragraph to have wrapped lines at 70"
  (interactive)
  (setq fill-column 70)
  (fill-paragraph)
  (message "Wrapped lines at 70 characters."))

(defun nice-org-single-long-line ()
  "Formats the current paragraph into a single long line."
  (interactive)
  (save-excursion
    (let ((start (progn (backward-paragraph 1) (point)))
          (end (progn (forward-paragraph 1) (point))))
      (goto-char start)
      (while (re-search-forward "[ \t]*\n[ \t]*" end t)
        (replace-match " "))))
  (message "Single long line."))

(defun nice-org-each-sentence-new-line ()
  "Puts each sentence of the current paragraph on a new line."
  (interactive)
  (save-excursion
    (let ((end (save-excursion (forward-paragraph) (point)))
          (beg (save-excursion (backward-paragraph) (point))))
      (goto-char beg)
      (while (< (point) end)
        (forward-sentence)
        ;; Insert newline at the end of a sentence, unless it's the last one.
        (unless (or (= (point) end) (eobp))
          (insert "\n")))))
  (message "Each sentence on a new line."))

;; Writing natural language:1 ends here

;; [[file:nicemacs-v2.org::*LaTeX preview][LaTeX preview:1]]
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
;; LaTeX preview:1 ends here

;; [[file:nicemacs-v2.org::*Agenda and calendar (org-mode)][Agenda and calendar (org-mode):1]]
;; Org-agenda
;; ----------
;;
;; - `n/p' to move up and down lines.
;; - `v-d' will show the day view.
;; - `v-w' the week view.
;; - `v-m' the month view.
;; - `v-SPC' resets the view.
;; - `.' goes to today.
;; - `j' will /jump/ to a date (selected via calendar).
;; - `t' will cycle through TODO/DONE
;; - `S-<left/right>' moves the scheduled date backwards/forwards
;; - `r' rebuilds the agenda view
;;
(setq org-agenda-start-day "-14d"
      org-agenda-span 30
      org-agenda-start-on-weekday nil
      org-agenda-window-setup 'other-frame
      org-log-done 'time
      org-log-schedule 'time)

(setq org-todo-keywords
      '((sequence "TODO" "DONE")
	(sequence "MEETING" "|" "DONE")
	(sequence "DEADLINE" "|" "DONE")
	(sequence "SOCIAL" "|" "DONE")))

(setq org-todo-keyword-faces
      `(("MEETING" . ,(boxed-face "magenta"))
	("DEADLINE" . ,(boxed-face "red" "white"))
	("SOCIAL" . ,(boxed-face "blue" "#E6ECFF"))))

(defun nice-org-agenda-goto-today-advice-after (&rest _args)
  "Adjust the window after calling `org-agenda-goto-today'."
  (recenter-top-bottom 4))

(advice-add 'org-agenda-goto-today
	    :after #'nice-org-agenda-goto-today-advice-after)
(evil-leader/set-key-for-mode 'org-mode "a s" 'org-schedule)
(evil-leader/set-key "a a" 'org-agenda-list)
;; Agenda and calendar (org-mode):1 ends here

;; [[file:nicemacs-v2.org::*Literate programming][Literate programming:1]]
;; Literate programming

(use-package polymode
  :ensure t
  :mode ("\\.org$" . poly-org-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . poly-org-mode)))

(use-package poly-R
  :ensure t
  :after polymode)

(use-package poly-org
  :ensure t
  :after polymode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)))

(evil-leader/set-key-for-mode 'org-mode "b t" 'org-babel-tangle)

(defun nice-detangle-nicemacs-v2 ()
  "Detangle the nicemacs-v2.el file."
  (interactive)
  (let ((nicemacs-v2-source (concat nice-nicemacs-directory
				    "/nicemacs-v2.el")))
    (org-babel-detangle nicemacs-v2-source)))

(evil-leader/set-key-for-mode 'emacs-lisp-mode "b d"
  'nice-detangle-nicemacs-v2)

(setq org-image-actual-width 300)
(evil-leader/set-key-for-mode 'org-mode
  "o t l" 'org-latex-preview
  "o t i" 'org-toggle-inline-images)
;; Literate programming:1 ends here

;; [[file:nicemacs-v2.org::*Website/Publishing][Website/Publishing:1]]
(defun nice-publish-homepage ()
  "Copy my website homepage if it exists."
  (interactive)
  (let ((local-notes "~/public-site/org/scratch.html")
	(remote-notes "~/aezarebski.github.io/notes.html")
	(local-landing "~/public-site/org/index-academic.html")
	(remote-landing "~/aezarebski.github.io/index.html"))
    (when (file-exists-p local-notes)
      (copy-file local-notes remote-notes t)
      (message "Copied %s to %s" local-notes remote-notes))
    (when (file-exists-p local-landing)
      (copy-file local-landing remote-landing t)
      (message "Copied %s to %s" local-landing remote-landing))))

;; The following projects are available for publishing when the
;; `org-publish' command is given.
(setq org-publish-project-alist
      `(("website-notes-org-files"
	 :base-directory "~/public-site/org/notes/"
	 :base-extension "org"
	 :publishing-directory "~/aezarebski.github.io/notes/"
	 :publishing-function org-html-publish-to-html)
	("website-teaching-org-files"
	 :base-directory "~/public-site/org/teaching/"
	 :base-extension "org"
	 :publishing-directory "~/aezarebski.github.io/teaching/"
	 :publishing-function org-html-publish-to-html)
	("website-teaching-static"
	 :base-directory "~/public-site/org/teaching/"
	 :base-extension "css\\|pdf"
	 :publishing-directory "~/aezarebski.github.io/teaching/"
	 :recursive t
	 :publishing-function org-publish-attachment)
	("website-lists-org-files"
	 :base-directory "~/public-site/org/lists/"
	 :base-extension "org"
	 :publishing-directory "~/aezarebski.github.io/lists/"
	 :publishing-function org-html-publish-to-html)
	("website-images-static"
	 :base-directory "~/public-site/org/images/"
	 :base-extension "png"
	 :publishing-directory "~/aezarebski.github.io/images/"
	 :publishing-function org-publish-attachment)
	("website-misc-ggplot2-org-files"
	 :base-directory "~/public-site/org/misc/ggplot2/"
	 :base-extension "org"
	 :publishing-directory "~/aezarebski.github.io/misc/ggplot2/"
	 :publishing-function org-html-publish-to-html)
	("website-misc-ggplot2-static"
	 :base-directory "~/public-site/org/misc/ggplot2/"
	 :base-extension "png\\|jpg\\|pdf"
	 :publishing-directory "~/aezarebski.github.io/misc/ggplot2/"
	 :publishing-function org-publish-attachment)
	("website-misc-basegraphicsR-org-files"
	 :base-directory "~/public-site/org/misc/basegraphicsR/"
	 :base-extension "org"
	 :publishing-directory "~/aezarebski.github.io/misc/basegraphicsR/"
	 :publishing-function org-html-publish-to-html)
	("website-misc-basegraphicsR-static"
	 :base-directory "~/public-site/org/misc/basegraphicsR/"
	 :base-extension "png\\|jpg\\|pdf"
	 :publishing-directory "~/aezarebski.github.io/misc/basegraphicsR/"
	 :publishing-function org-publish-attachment)
	("website-misc-latex-org-files"
	 :base-directory "~/public-site/org/misc/latex/"
	 :base-extension "org"
	 :publishing-directory "~/aezarebski.github.io/misc/latex/"
	 :publishing-function org-html-publish-to-html)
	("website-misc-latex-static"
	 :base-directory "~/public-site/org/misc/latex/"
	 :base-extension "png\\|jpg\\|pdf"
	 :publishing-directory "~/aezarebski.github.io/misc/latex/"
	 :publishing-function org-publish-attachment)
	("website-misc-tikz-org-files"
	 :base-directory "~/public-site/org/misc/tikz/"
	 :base-extension "org"
	 :publishing-directory "~/aezarebski.github.io/misc/tikz/"
	 :publishing-function org-html-publish-to-html)
	("website-misc-tikz-static"
	 :base-directory "~/public-site/org/misc/tikz/"
	 :base-extension "png\\|jpg\\|pdf"
	 :publishing-directory "~/aezarebski.github.io/misc/tikz/"
	 :publishing-function org-publish-attachment)
	("website-misc-matplotlib-org-files"
	 :base-directory "~/public-site/org/misc/matplotlib/"
	 :base-extension "org"
	 :publishing-directory "~/aezarebski.github.io/misc/matplotlib/"
	 :publishing-function org-html-publish-to-html)
	("website-misc-matplotlib-static"
	 :base-directory "~/public-site/org/misc/matplotlib/"
	 :base-extension "png\\|jpg\\|pdf"
	 :publishing-directory "~/aezarebski.github.io/misc/matplotlib/"
	 :publishing-function org-publish-attachment)
	("website-misc-plotnine-org-files"
	 :base-directory "~/public-site/org/misc/plotnine/"
	 :base-extension "org"
	 :publishing-directory "~/aezarebski.github.io/misc/plotnine/"
	 :publishing-function org-html-publish-to-html)
	("website-misc-plotnine-static"
	 :base-directory "~/public-site/org/misc/plotnine/"
	 :base-extension "png\\|jpg\\|pdf"
	 :publishing-directory "~/aezarebski.github.io/misc/plotnine/"
	 :publishing-function org-publish-attachment)
	("website-misc-recipes"
	 :base-directory "~/public-site/org/misc/recipes/"
	 :base-extension "org"
	 :publishing-directory "~/aezarebski.github.io/misc/recipes/"
	 :publishing-function org-html-publish-to-html)
	("website-misc-recipes-static"
	 :base-directory "~/public-site/org/misc/recipes/"
	 :base-extension "png\\|css"
	 :publishing-directory "~/aezarebski.github.io/misc/recipes/"
	 :recursive ()
	 :publishing-function org-publish-attachment)
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
	("nicemacs-org-files"
	 :base-directory "~/Documents/nicemacs/"
	 :base-extension "org"
	 :publishing-directory "~/aezarebski.github.io/misc/nicemacs/"
	 :publishing-function org-html-publish-to-html)
	("python"
	 :components ("website-misc-matplotlib-org-files"
		      "website-misc-matplotlib-static"
		      "website-misc-plotnine-org-files"
		      "website-misc-plotnine-static"))
	("R"
	 :components ("website-misc-basegraphicsR-org-files"
		      "website-misc-basegraphicsR-static"
		      "website-misc-ggplot2-org-files"
		      "website-misc-ggplot2-static"))
	("review"
	 :components ("review2-org"
		      "review2-static"))
	("latex"
	 :components ("website-misc-latex-org-files"
		      "website-misc-latex-static"
		      "website-misc-tikz-org-files"
		      "website-misc-tikz-static"))
	("recipes"
	 :components ("website-misc-recipes"
		      "website-misc-recipes-static"))
	("teaching"
	 :components ("website-teaching-org-files"
		      "website-teaching-static"))
	("website"
	 :components ("website-notes-org-files"
		      "website-images-static"
		      "website-lists-org-files"
		      "nicemacs-org-files"
		      "recipes"
		      "review"
		      "latex"
		      "python"
		      "R"))))
;; Website/Publishing:1 ends here

;; [[file:nicemacs-v2.org::*STUFF 8][STUFF 8:1]]
;; Visitors
;; ========

(defmacro NVNF (fname pname file key)
  "Macro to define a function for visiting a notes file and set an Evil leader key binding.

  This macro takes in four arguments:
  - FNAME: A string that will be used to construct the function name.
  - PNAME: A string that will be used in the message displayed to the user.
  - FILE: A string that represents the name of the notes file.
  - KEY: A string that represents the keybinding for the function using the Evil leader.

  The function created by this macro opens the notes file specified by FILE in
  the directory specified by `nice-notes-directory'. The keybinding is set using
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

(defmacro NVF (fname pname file key)
  `(progn
     (defun ,(intern (format "nice-visit-%s" fname)) ()
       "Visit a file."
       (interactive)
       (progn
	 (message ,(format "Visiting %s" pname))
	 (find-file ,file)))
     (evil-leader/set-key ,(concat "v f" key) (intern ,(format "nice-visit-%s" fname)))))

(defmacro NVD (dname pname path key)
  "Macro to define a function for visiting a directory and set an Evil leader key binding.

  This macro takes in four arguments:
  - DNAME: A string that will be used to construct the function name.
  - PNAME: A string that will be used in the message displayed to the user.
  - PATH: A string that represents the path of the directory.
  - KEY: A string that represents the keybinding for the function using the Evil leader.

  The function created by this macro jumps to the directory specified by PATH using `dired-jump'.
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

(NVF nicemacs2-source "Nicemacs v2 source" "~/Documents/nicemacs/nicemacs-v2.el" "e 3")
(NVF nicemacs2-init "Nicemacs v2 init.el" "~/.emacs.d/init.el" "e 2")
(NVF nicemacs-init "Nicemacs v1 nicemacs.el" "~/Documents/nicemacs/nicemacs.el" "e 1")
(NVF nicemacs-org "Nicemacs v1 nicemacs.org" "~/Documents/nicemacs/nicemacs.org" "e 1")
(NVF review-2 "Review 2" "~/Documents/bibliography/review2/review.org" "r 2")
(NVF review-reading-list "Reading list" "~/Documents/bibliography/review2/reading-list.org" "r l")
(NVF review-references "Bibtex references" "~/Documents/bibliography/references.bib" "r r")

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

(NVD emacs "Emacs" "~/.emacs.d/fake.org" "e")
(NVD library "Library" "~/Documents/library/fake.org" "l")
(NVD manuscripts "Manuscripts" "~/Documents/manuscripts/fake.org" "m")
(NVD music "Music" "~/Music/fake.org" "M")
(NVD documents "Documents" "~/Documents/fake.org" "d")
(NVD downloads "Downloads" "~/Downloads/fake.org" "D")
(NVD professional "Professional" "~/Documents/professional/README.org" "p")
(NVD projects "Projects" "~/projects/fake.org" "P")
(NVD teaching "Teaching" "~/Documents/teaching/fake.org" "t")
(NVD website-org "Website (org files)" "~/public-site/org/fake.org" "w")
(NVD website-html "Website (HTML files)" "~/aezarebski.github.io/fake.org" "W")
(NVD notes "My notes" "~/public-site/org/notes/fake.org" "n")
(NVD yasnippet "Yasnippet" "~/.emacs.d/snippets/fake.org" "y")

(setq org-agenda-files
      (list (concat nice-journal-directory "bike.org")))

(defun nice-visit-journal ()
  "Opens the current journal file. If it does not yet exist, it
  makes a copy of the one from one week ago. This will also
  ensure that the current journal file is among the org agenda
  files and that a previous one is not."
  (interactive)
  (let* ((filepath-template (concat nice-journal-directory "journal-%s.org"))
	 (curr-file (format filepath-template (format-time-string "%Y-%m")))
	 (prev-file (format filepath-template (format-time-string "%Y-%m" (time-subtract (current-time) (* 7 24 60 60))))))
    (unless (file-exists-p curr-file)
      (message "Creating new journal file")
      (copy-file prev-file curr-file))
    (message "Opening journal file")
    (when (member prev-file org-agenda-files)
      (setq org-agenda-files (remove prev-file org-agenda-files)))
    (unless (member curr-file org-agenda-files)
      (add-to-list 'org-agenda-files curr-file))
    (find-file curr-file)
    (goto-char (point-min))
    (recenter-top-bottom)))

(evil-leader/set-key "v f j" 'nice-visit-journal)
;; STUFF 8:1 ends here

;; [[file:nicemacs-v2.org::*Copilot][Copilot:1]]
;; Copilot
;; =======
;;
;; To install this you need to clone the repository and a couple of
;; dependencies yourself: s, editorconfig which are emacs packages and
;; node.js.
;;
;; To enable `copilot' on your buffer, use SPC t c.
;;
(use-package copilot
  :defer 1
  :config
  (evil-leader/set-key "t c" 'copilot-mode)
  (setq copilot-node-executable "~/.nvm/versions/node/v17.3.1/bin/node")
  ;; (setq copilot-node-executable "/usr/bin/node")
  :load-path "~/.emacs.d/copilot.el/")

(defun nice-copilot-tab ()
  "Accept the current suggestion provided by copilot."
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))

(with-eval-after-load 'copilot
  (evil-define-key 'insert copilot-mode-map
    (kbd "<tab>") #'nice-copilot-tab))

(defun nice-copilot-by-line ()
  "Accept the current suggestion by line."
  (interactive)
  (or (copilot-accept-completion-by-line)
      (indent-for-tab-command)))

(with-eval-after-load 'copilot
  (evil-define-key 'insert copilot-mode-map
    (kbd "C-<tab>") #'nice-copilot-by-line))
;; Copilot:1 ends here

;; [[file:nicemacs-v2.org::*STUFF 9][STUFF 9:1]]
;; Explore new worlds
;; ==================

;; TODO Work out how to browse gopher with =gopher.el=.

;; TODO Work out how to configure auth-source.

;; TODO Work out how to use mediawiki-mode to read and edit wikipedia.

;; TODO Explore running spotify through emacs

;; Customization
;; =============

;; There be dragons here
;; ---------------------
;; STUFF 9:1 ends here
