;; [[file:nicemacs-v2.org::*Nicemacs v2][Nicemacs v2:1]]
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
;; - `ess' Emacs Speaks Statistics
;; - `ess-site'
;; - `evil' Extensible Vi layer for Emacs.
;; - `evil-collection' A set of keybindings for Evil mode
;; - `evil-leader' let there be <leader>
;; - `evil-mc'
;; - `evil-mc-extras' Extra functionality for evil-mc
;; - `evil-surround'
;; - `flyspell'
;; - `hl-todo' Highlight TODO and similar keywords
;; - `htmlize' Convert buffer text and decorations to HTML.
;; - `magit' A Git porcelain inside Emacs.
;; - `markdown-mode' Major mode for Markdown-formatted text
;; - `multiple-cursors' Multiple cursors for Emacs.
;; - `ligature' Ligature support for Emacs.
;; - `quarto-mode' A (poly)mode for https://quarto.org
;; - `racket-mode' Racket editing, REPL, and more
;; - `rainbow-mode' Colorize color names in buffers
;; - `s' The long lost Emacs string manipulation library.
;; - `solarized-theme' The Solarized color theme
;; - `which-key' Display available keybindings in popup
;; - `winum' Navigate windows and frames using numbers.
;; - `writegood-mode' Polish up poor writing on the fly
;; - `yasnippet' Yet another snippet extension for Emacs
;; - `yasnippet-snippets' Collection of yasnippet snippets
;;
;; Changelog
;; ---------
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
(require 'evil-surround)

(evil-mode 1)
(evil-leader-mode 1)
(global-evil-leader-mode 1)
(evil-collection-init)

;; Evil surroundings
;;
;; 1. Enter visual mode and select the text as the region.
;; 2. Press `S'.
;; 3. Type the symbol to surround it (note, if it is part of a opening
;;    and closing pair, the opening includes a space and the closing
;;    does not.)
(global-evil-surround-mode 1)
(evil-leader/set-key "t s" 'evil-surround-mode)

;; Look stunning
;; =============
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
(ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                                     "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                                     "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                                     "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                                     "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                                     "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                                     ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                                     "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                                     "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                                     "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                                     "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
(global-ligature-mode t)

(defun toggle-ligatures ()
  "Toggle ligatures on and off."
  (interactive)
  (if (bound-and-true-p global-ligature-mode)
      (global-ligature-mode -1)
    (global-ligature-mode 1)))
(evil-leader/set-key "t l" 'toggle-ligatures)

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
;; FIXME the fill column is a bit hard to see, it should be clearer.
(evil-leader/set-key "t f" 'display-fill-column-indicator-mode)

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
(add-hook 'ess-mode-hook 'rainbow-mode)

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

;; The which-key package is a great way to be reminded of what keys
;; are available from the start of a key sequence.
(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.3)
(require 'which-key)
(which-key-mode)

(setq key-description-pairs
      '(("SPC a" . "Agenda (org-mode)")
        ("SPC b" . "Buffers")
        ("SPC c" . "Cursors")
        ("SPC f" . "Files/Dired")
        ("SPC F" . "Frame")
        ("SPC g" . "Git (magit)")
        ("SPC g c" . "Commits")
        ("SPC h" . "HELP!!!")
        ("SPC m" . "Major")
        ("SPC m s" . "REPL")
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

;; The `winum' package facilitates switching between windows using
;; numbers which appear in the bottom left hand of the window, at the
;; start of the mode-line.
(require 'winum)
(winum-mode)
(setq winum-format " %s ")
(custom-set-faces
 `(winum-face
   ((t
     (:foreground ,(nice-colour 'weak-warning)
      :weight bold
      :underline nil
      :height 1.1)))))
(evil-leader/set-key
  "0" 'winum-select-window-0
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "5" 'winum-select-window-5
  "6" 'winum-select-window-6
  "7" 'winum-select-window-7
  "8" 'winum-select-window-8
  "9" 'winum-select-window-9)

;; Adjust the windows so that they are all the same size.
(evil-leader/set-key "w b" 'balance-windows)

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
(evil-leader/set-key "w a" 'nice-balance-windows-alt)

;; Shell stuff
;; -----------

(setq eshell-cmpl-ignore-case t)
(evil-leader/set-key "s e" 'eshell)
(evil-leader/set-key "s b" (lambda () (interactive) (ansi-term "/bin/bash")))
(evil-leader/set-key "s i" 'ielm)
(evil-leader/set-key "s r" 'R)

;; Buffer stuff
;; ------------

(evil-leader/set-key "b r" 'revert-buffer)

;; File stuff
;; ----------

;; TODO Configure functions to move/copy the most recent file in the
;; ~/Downloads directory to the current directory so that they can be
;; used from eshell.

(evil-leader/set-key "f f" 'find-file)
(evil-leader/set-key "f s" 'save-buffer)

(require 'dired)
(evil-leader/set-key "f d" 'nice-dired)
(define-key dired-mode-map "-" 'dired-up-directory)
(setq dired-listing-switches "-alh")

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

(evil-leader/set-key "b b" 'switch-to-buffer)
(evil-leader/set-key "b d" 'kill-buffer)

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
(evil-leader/set-key "h d p" 'describe-package)
(evil-leader/set-key "h d k" 'describe-key)
(evil-leader/set-key "h d v" 'describe-variable)

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

(evil-leader/set-key "h b n" 'message-buffer-file-name)
(evil-leader/set-key "h l m" 'message-link-at-point)

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

(setq sentence-end-double-space nil)

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

(setq words-to-add
      '("many" "various" "very" "quite" "somewhat" "several"
	"extremely" "exceedingly" "fairly" "rather" "remarkably" "few"
	"surprisingly" "mostly" "largely" "almost" "nearly"
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

(defun nice-org-unfill-paragraph ()
  "Unfill the paragraph at point, joining all lines into a single line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


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
  "y c" 'yas-compile-directory  ; Compile all snippets
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
;; 3. Use `evil-insert' (`SPC c i`) to start editing.
;; 4. Exit using `mc/keyboard-quit' (`SPC c q`)

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
(nice-canned-commit-message notes "update notes" "n")
(nice-canned-commit-message review "update reading list" "r")
(nice-canned-commit-message website "update website" "w")
(nice-canned-commit-message journal "update journal" "j")

(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

(evil-leader/set-key "g q" 'with-editor-cancel)

;; Emacs Lisp
;; ----------

;; TODO Configure a linter

(evil-leader/set-key-for-mode 'emacs-lisp-mode "m s c" 'eval-last-sexp)
(evil-leader/set-key-for-mode 'emacs-lisp-mode "m s b" 'eval-buffer)
(evil-leader/set-key-for-mode 'emacs-lisp-mode "m s r" 'eval-region)

;; Emacs Speaks Statistics (ESS)
;; -----------------------------

(require 'ess-site)
(setq ess-default-style 'DEFAULT)

(evil-leader/set-key-for-mode 'ess-r-mode "m s b" 'ess-eval-buffer)
(evil-leader/set-key-for-mode 'ess-r-mode "m s r" 'ess-eval-region)
(evil-leader/set-key-for-mode 'ess-r-mode "m '" 'ess-switch-to-inferior-or-script-buffer)

(require 'quarto-mode)

(evil-leader/set-key-for-mode 'ess-r-mode "m s c" 'ess-eval-region-or-line-visibly-and-step)

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

(evil-leader/set-key "v b l" 'nice-visit-last-bib)

(evil-leader/set-key-for-mode 'bibtex-mode "m b b" 'nice-bibtex-braces)
(evil-leader/set-key-for-mode 'bibtex-mode "m b f" 'bibtex-reformat)

;; Markdown-mode
;; -------------

(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;; Org-Mode
;; --------

;; NOTE It would be nice to have an additional command and key for
;; moving from level n+1 headers their parent level n header.

;; FIXME Work out why the configuration based approach does not work!
(setq org-return-follows-link t)
(evil-leader/set-key-for-mode 'org-mode "RET" 'org-open-at-point)

(defun nice-org-mode-hook ()
  "Set up org-mode specific keybindings."
  (local-set-key (kbd "<tab>") #'org-cycle))

(add-hook 'org-mode-hook #'nice-org-mode-hook)

;; Org-agenda
;;
;; - To navigate up and down lines in the agend view use =n/p=.
;; - =v d= will show the day view.
;; - =v w= the week view.
;; - =v m= the month view.
;; - =v SPC= resets the view.
;; - =.= goes to today.
;; - =j= will /jump/ to a date (selected via calendar).
;;
(setq org-agenda-start-day "-7d")
(setq org-agenda-span 30)
(setq org-agenda-start-on-weekday nil)

(evil-leader/set-key "a a" 'org-agenda)
(evil-leader/set-key-for-mode 'org-mode "a s" 'org-schedule)
(evil-leader/set-key-for-mode 'org-mode "b t" 'org-babel-tangle)

(defun nice-org-agenda-goto-today-advice-after (&rest _args)
  "Adjust the window after calling `org-agenda-goto-today'."
  (recenter-top-bottom 4))

(advice-add 'org-agenda-goto-today
	    :after #'nice-org-agenda-goto-today-advice-after)

;; Literate programming

(defun nice-detangle-nicemacs-v2 ()
  "Detangle the nicemacs-v2.el file."
  (interactive)
  (let ((nicemacs-v2-source (concat nice-nicemacs-directory "/nicemacs-v2.el")))
    (org-babel-detangle nicemacs-v2-source)))

(evil-leader/set-key-for-mode 'emacs-lisp-mode "b d" 'nice-detangle-nicemacs-v2)

(evil-leader/set-key-for-mode 'org-mode "o t l" 'org-latex-preview)

(setq org-image-actual-width 500)
(evil-leader/set-key-for-mode 'org-mode "o t i" 'org-toggle-inline-images)

(defun nice-publish-homepage ()
  "Copy my website homepage if it exists."
  (interactive)
  (let ((local "~/public-site/org/scratch.html")
	(remote "~/aezarebski.github.io/index.html"))
    (unless (not (file-exists-p local))
      (copy-file local remote t)
      (message "Copied %s to %s" local remote))))

;; The following projects are available for publishing when the
;; `org-publish' command is given.
(setq org-publish-project-alist
      `(("website-notes-org-files"
	 :base-directory "~/public-site/org/notes/"
	 :base-extension "org"
	 :publishing-directory "~/aezarebski.github.io/notes/"
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
	("nicemacs-org-files"
	 :base-directory "~/Documents/nicemacs/"
	 :base-extension "org"
	 :publishing-directory "~/aezarebski.github.io/misc/nicemacs/"
	 :publishing-function org-html-publish-to-html)
	("R"
	 :components ("website-misc-basegraphicsR-org-files"
		      "website-misc-basegraphicsR-static"
		      "website-misc-ggplot2-org-files"
		      "website-misc-ggplot2-static"))
	("latex"
	 :components ("website-misc-tikz-org-files"
		      "website-misc-tikz-static"))
	("website"
	 :components ("website-notes-org-files"
		      "website-images-static"
		      "nicemacs-org-files"
		      "latex"
		      "R"))))

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

(NVF nicemacs2-init "Nicemacs v2 init.el" "~/.emacs.d/init.el" "e 2")
(NVF nicemacs-init "Nicemacs v1 nicemacs.el" "~/Documents/nicemacs/nicemacs.el" "e 1")
(NVF nicemacs-org "Nicemacs v1 nicemacs.org" "~/Documents/nicemacs/nicemacs.org" "e 1")
(NVF review-2 "Review 2" "~/Documents/bibliography/review2/review.org" "r 2")
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

(defun nice-visit-journal ()
  "Opens the current journal file. If it does not yet exist, it
  makes a copy of the one from one week ago. This will also
  ensure that the current journal file is among the org agenda
  files and that a previous one is not."
  (interactive)
  (let* ((filepath-template (concat nice-journal-directory "/journal-%s.org"))
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

;; Copilot
;; =======
;;
;; To install this you need to clone the repository and a couple of
;; dependencies yourself: s, editorconfig which are emacs packages and
;; node.js.
;;
;; TODO There should really be an option to accept suggestions on a
;; line by line basis.
;;

(add-to-list 'load-path "~/.emacs.d/copilot.el/")
(require 'copilot)

;; (setq copilot-node-executable "~/.nvm/versions/node/v17.3.1/bin/node")
;; (setq copilot-node-executable "/usr/bin/node")
(add-hook 'python-mode-hook 'copilot-mode)
(add-hook 'ess-r-mode-hook 'copilot-mode)

(defun nice/copilot-tab ()
  "Accept the current suggestion from copilot"
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))

(with-eval-after-load 'copilot
  (evil-define-key 'insert copilot-mode-map
    (kbd "<tab>") #'nice/copilot-tab))

(defun nice/copilot-cycle ()
  "Cycle through suggested completions"
  (interactive)
  (copilot-next-completion))

(with-eval-after-load 'copilot
  (evil-define-key 'insert copilot-mode-map
    (kbd "<backtab>") #'nice/copilot-cycle))

(evil-leader/set-key "t c" 'copilot-mode)

;; Explore new worlds
;; ==================

;; TODO Work out how to browse gopher with =gopher.el=.

;; TODO Work out how to configure auth-source.

;; TODO Work out how to use mediawiki-mode to read and edit wikipedia.

;; TODO Explore running spotify through emacs

;; TODO Work out how to search for yasnippets with keywords: do they
;; have a keyword field?

;; Customization
;; =============

;; There be dragons here
;; ---------------------
;; Nicemacs v2:1 ends here
