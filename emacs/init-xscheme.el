;; init-xscheme.el --- my inits for running MIT Scheme under GNU Emacs
;; $Id: init-xscheme.el,v 1.4 2000/02/14 11:58:51 friedman Exp $

;; For use with xscheme.el RCS version 1.37 or later.

(defun ixscheme-interaction-mode-initialize ()
  (and (fboundp 'protect-process-buffer-from-kill-mode)
       (protect-process-buffer-from-kill-mode 1))
  (define-key scheme-interaction-mode-map "\C-j"
    'xscheme-send-previous-expression))

(setq scheme-program-arguments "-compiler")

(add-hook 'scheme-interaction-mode-hook 'ixscheme-interaction-mode-initialize)

(provide 'init-xscheme)

;; init-xscheme.el ends here
