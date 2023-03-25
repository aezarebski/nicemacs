;;; Nicemacs.v2 -*- lexical-binding: t -*-
;;; ======================================

(require 'package)
(add-to-list 'package-archives  '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Look stunning
;; =============

(add-to-list `custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'solarized-light t)
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

;; Be evil
;; =======

(require 'evil)
(require 'evil-leader)

(evil-mode 1)

;; Sensible keys
;; -------------

(global-evil-leader-mode)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "<SPC>" 'execute-extended-command)

(evil-leader/set-key "f f" 'find-file)

(evil-leader/set-key "b b" 'switch-to-buffer)
(evil-leader/set-key "b d" 'kill-buffer)

(evil-leader/set-key "w s" 'split-window-below)
(evil-leader/set-key "w v" 'split-window-right)
(evil-leader/set-key "TAB" 'next-window-and-pulse)
(evil-leader/set-key "<backtab>" 'previous-window-and-pulse)
(evil-leader/set-key "w d" 'delete-window)

(evil-leader/set-key "h s" 'apropos)
(evil-leader/set-key "h d f" 'describe-function)
(evil-leader/set-key "h d m" 'describe-mode)
(evil-leader/set-key "h d k" 'describe-key)
(evil-leader/set-key "h d v" 'describe-variable)

(evil-leader/set-key "z j" 'text-scale-decrease)
(evil-leader/set-key "z k" 'text-scale-increase)

;; Be virtuous
;; ===========

(setq-default major-mode
              (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(recentf-mode t)

;; Be powerful
;; ===========

;; Emacs Speaks Statistics
;; -----------------------
(require 'ess-site)
(setq ess-default-style 'DEFAULT)

(evil-leader/set-key-for-mode 'ess-r-mode "m s b" 'ess-eval-buffer)
(evil-leader/set-key-for-mode 'ess-r-mode "m s r" 'ess-eval-region)

;; Custom
;; ======

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-delimiter-face ((t (:background "MediumPurple1" :foreground "purple4" :slant normal))))
 '(font-lock-comment-face ((t (:background "MediumPurple1" :foreground "purple4")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(ess magit evil-leader solarized-theme evil)))
