;;; packages.el --- maxima layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Alexander E. Zarebski <aezarebski@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst maxima-packages
  '(maxima))

(defun maxima/init-maxima ()
  (use-package maxima
    :defer f
    :load-path "/usr/share/emacs/site-lisp/maxima/"
    :mode ("\\.mac" . maxima-mode)
    :config
    (progn
      (autoload 'maxima-mode "maxima" "Maxima mode" t)
      (autoload 'maxima "maxima" "Maxima interaction" t)

      ;; REPL keybindings
      (spacemacs/declare-prefix-for-mode 'maxima-mode "ms" "repl")
      (spacemacs/set-leader-keys-for-major-mode 'maxima-mode
        "sb" 'maxima-send-buffer
        "sr" 'maxima-send-region
        "sl" 'maxima-send-line)

      ;; Help/Documentation keybindings
      (spacemacs/declare-prefix-for-mode 'maxima-mode "mh" "help")
      (spacemacs/set-leader-keys-for-major-mode 'maxima-mode
        "hi" 'maxima-info)
      )))

;;; packages.el ends here
