;;; init-vm.el --- initialization for VM

;; Author: Noah Friedman <friedman@splode.com>
;; Created: 1994
;; Public domain

;; $Id: init-vm.el,v 1.135 2011/12/01 03:16:44 friedman Exp $

;;; Commentary:

;; This file contains expressions and variables that are loaded and
;; evaluated upon startup of the VM MUA.

;; Some of the variables defined in this file aren't meaningful for
;; anyone else; they contain pathnames or addresses that only I would
;; want to use.

;; I use `ivm' ("init vm") to prefix most of my vm extensions.

;;; Code:

(require      'vm)
(require-soft 'vm-vars)
(require-soft 'vm-revno)
(require      'vm-addons)
(require      'vm-advices)
(require      'vm-multdom)

(require      'fmailutils)
(require      'kbd-fns)
(require      'lat1conv)
(require      'timezone)

(require      'list-fns)    ; for push-command
(require      'pb-popup)    ; for summary buffer window management

(require-soft 'bbdb)
(require-soft 'mailcrypt)
(require-soft 'spookmime)
(require-soft 'vm-vcard)
(require-soft 'u-vm-color)

(require-offer-compile 'all.mail)  ; for vm-multdom-user-addresses settings


;;;;;;
;;; My variables
;;;;;;

;; ----------
(defvar ivm-incoming-mail-spool-directory
  (let ((dirs '("/var/mail"
                "/var/spool/mail"
                "/usr/spool/mail"
                "/usr/mail"
                "/com/mail"))
        result)
    (while dirs
      (if (file-directory-p (car dirs))
          (setq result (car dirs)
                dirs nil)
        (setq dirs (cdr dirs))))
    result))

;; ----------
(defvar ivm-mime-attachment-auto-type-alist
  '((",v$"          . "application/octet-stream")
    ("\\.[ch]$"     . "text/plain")
    ("\\.bash$"     . "text/plain")
    ("\\.bmp$"      . "image/bmp")
    ("\\.bz$"       . "application/x-bzip")
    ("\\.bz2$"      . "application/x-bzip2")
    ("\\.c?sh$"     . "text/plain")
    ("\\.cf$"       . "text/plain")
    ("\\.conf$"     . "text/plain")
    ("\\.el$"       . "text/plain")
    ("\\.exe$"      . "application/x-msft-exe")
    ("\\.gz$"       . "application/x-gzip")
    ("\\.html?$"    . "text/html")
    ("\\.li?sp$"    . "text/plain")
    ("\\.mp3$"      . "application/x-mp3")
    ("\\.pgp$"      . "application/x-pgp")
    ("\\.scm$"      . "text/plain")
    ("\\.shar$"     . "application/x-shar")
    ("\\.tar$"      . "application/x-tar")
    ("\\.tar\\.gz$" . "application/octet-stream")
    ("\\.te?xt$"    . "text/plain")
    ("\\.tgz$"      . "application/octet-stream")
    ("\\.vcf$"      . "text/x-vcard")
    ("\\.xcf$"      . "application/octet-stream")
    (".*"           . "application/octet-stream"))
  "These values are appended to `vm-mime-attachment-auto-type-alist'.")


;;;;;;
;;; Defuns
;;;;;;

;; ----------
;; This function goes on `vm-display-buffer-hook'.
;;
;; The effect is that displaying anything but a summary buffer will replace
;; the buffer in the selected window with the new buffer.  For summary
;; buffers, 1/6 of largest window is replaced with a new window showing the
;; summary buffer, and point is shown in the middle of the summary window.
;;
;; If a summary buffer window is created, this function pushes a command
;; back into the interactive event loop which will re-select the original
;; window; we cannot do that within this hook because the *new* window must
;; be selected when the function on vm-display-buffer-hook returns.
(defun ivm-display-buffer ()
  (when (eq major-mode 'vm-summary-mode)
    (let ((pb-popup-ratio 6)
          (orig-win (selected-window)))
      (pb-popup-window (current-buffer) (point))
      (recenter '(4))
      (push-command (list 'select-window orig-win)))))

;; ----------
;; My own modified standard-display-table has glyphs for w32 charsets, so
;; is actually easier to read.
(defadvice vm-fsfemacs-nonmule-display-8bit-chars (after sinit:disptable activate)
  "Do not use VM-specific display table on non-multibyte buffers."
  (unless enable-multibyte-characters
    (setq buffer-display-table standard-display-table)))

;; ----------
(defun ivm-ironport-report-spam ()
  (interactive)
  (vm-forward-message-all-headers)
  (fmailutils-put-unique-header "To" "spam@access.ironport.com" t)
  (fmailutils-remove-header "Reply-To" nil t)
  (fmailutils-remove-header "Fcc"      nil t)
  (let ((vm-confirm-mail-send nil))
    (vm-mail-send-and-exit)))

;; ----------
(defun ivm-lat1conv-to-ascii-query (&optional beg end)
  ;; Do no conversion unless total conversion is possible.
  ;; This behavior is suggested in lat1conv.el.
  (lat1conv-to-ascii-query beg end t)
  (and (lat1conv-other-8bit-in-region-p beg end)
       (message "warning: 8-bit chars in included text.")))

;; ----------
(defun ivm-make-index-file-name (&optional file-name)
  (or file-name (setq file-name buffer-file-name))
  (cond (vm-index-file-suffix
         (let* ((topdir (file-name-directory file-name))
                (name (file-name-nondirectory file-name))
                (idxdir (concat topdir vm-index-file-suffix)))
           (cond ((and (file-exists-p idxdir)
                       (file-directory-p idxdir))
                  (format "%s/%s%s" idxdir name vm-index-file-suffix))
                 (t (throw 'done t)))))
        (t (throw 'done t))))

;; ----------
(defadvice vm-make-index-file-name (around sinit:ivm-make-index activate)
  "Use `ivm-make-index-file-name' to construct name."
  (setq ad-return-value
        (ivm-make-index-file-name buffer-file-name)))

;; ----------
;; This usually goes on mail-setup-hook
(defun ivm-mail-setup ()
  ;; I don't know if VM 5.72 or later even inserts this header anymore.
  (fmailutils-remove-header "Full-Name")
  ;; VM 6.20 started adding this one.
  (fmailutils-remove-header "X-Mailer")
  ;; VM 6.44 started adding a default Message-Id.  Since I generate my own
  ;; Message-Ids, get rid of this.
  (fmailutils-remove-header "Message-Id"))

;; ----------
(defadvice vm-raise-frame (around sinit:ignore activate)
  "Do not raise frames."
  nil)

;; ----------
(defun ivm-set-mode-line-format ()
  (interactive)
  (setq-default vm-mode-line-format
                '(""
                  ivm-buffer-modified-flag
                  mode-line-buffer-identification
                  (vm-message-list
                   (" "
                    vm-ml-message-number
                    "/"
                    vm-ml-highest-message-number)
                   " (no messages)")
                  " " global-mode-string " "
                  (line-number-mode "L%l ")
                  (column-number-mode "C%c ")
                  (buffer-percentage-mode (-3 . "%p"))
                  ;; append a space if enabled; we can't do that above
                  ;; because the string is truncated to 3 chars.
                  (buffer-percentage-mode " ")
                  (escreen-number-mode escreen-mode-line-format)
                  "%[("
                  mode-name
                  minor-mode-alist
                  ")%]"
                  (vm-message-list
                   (" {" vm-ml-message-attributes-alist "}"))
                  global-mode-string-trailer
                  "%-"))
  (setq mode-line-format vm-mode-line-format)
  (force-mode-line-update))

;; ----------
(defadvice vm-su-year (after ivm-truncate activate)
  "Truncate value of year to 2 digits.
This is to squeeze horizontal space for summaries."
  (setq ad-return-value (substring ad-return-value 2)))

;; ----------
;; Note: might not actually use w3 here; the API can be emulated with w3m.
(defun ivm-toggle-use-w3 (&optional prefix)
  (interactive "P")
  (setq vm-mime-use-w3-for-text/html
        (if prefix
            (>= (prefix-numeric-value prefix) 0)
          (not vm-mime-use-w3-for-text/html)))
  (if vm-mime-use-w3-for-text/html
      (setq vm-mime-internal-content-type-exceptions
            (delete "text/html" vm-mime-internal-content-type-exceptions))
    (add-to-list 'vm-mime-internal-content-type-exceptions
                 "text/html"))
  (and (interactive-p)
       (if vm-mime-use-w3-for-text/html
           (message "will use w3")
         (message "will not use w3"))))

;; ----------
(defvar ivm-buffer-modified-flag "-")
(make-variable-buffer-local 'ivm-buffer-modified-flag)
(put 'ivm-buffer-modified-flag 'permanent-local t)

(defun ivm-sync-buffer-modified-flag ()
  (setq ivm-buffer-modified-flag
        (cond (vm-folder-read-only
               "%")
              ((and (buffer-modified-p)
                    (> vm-modification-counter 0))
               "*")
              ("-")))
  (force-mode-line-update))


;; Mode and module initializations

;; ----------
(defun ivm-initialize-vm-mode ()
  (ivm-set-mode-line-format)

  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'ivm-sync-buffer-modified-flag nil t)

  ;; No numbered backups for mail folders.
  (make-local-variable 'version-control)
  (setq version-control nil)

  ;; Disable Eric Eide's scroll-in-place scrolling commands since vm's
  ;; own scrolling commands get confused by them.  Preserving point's
  ;; distance from window-start in mail buffers isn't critical to me.
  (make-local-variable 'scroll-in-place)
  (setq scroll-in-place nil)

  ;; I've accidentally deleted and expunged things; undo is helpful.
  (buffer-enable-undo (current-buffer))

  ;; Kill the summary buffer when we die.
  ;; In emacs 18 you'll have to hack kill-buffer to run
  ;; kill-buffer-hook, because it's not a supported hook yet.
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'vma-kill-vm-summary-buffer nil t))

;; ----------
;; Presentation mode first appears in VM version 6.
(defun ivm-initialize-vm-presentation-mode ()
  (let ((orig-buffer (current-buffer))
        (presentation-buffer vm-presentation-buffer))
    (unwind-protect
        (progn
          (and (bufferp presentation-buffer)
               (set-buffer presentation-buffer))
          (cond
           ((eq major-mode 'vm-presentation-mode)
            (setq mode-name "VMP")
            (save-match-data
              (let ((bufname (buffer-name)))
                (cond ((string-match " Presentation$" bufname)
                       (setq bufname
                             (concat (substring bufname 0 (match-beginning 0))
                                     "<P>"))
                       (rename-buffer bufname)))))

            ;; Disable Eric Eide's scroll-in-place scrolling commands since
            ;; vm's own scrolling commands get confused by them.
            ;; Preserving point's distance from window-start in mail
            ;; buffers isn't critical to me.
            (make-local-variable 'scroll-in-place)
            (setq scroll-in-place nil))))
      (set-buffer orig-buffer))))

;; ----------
(defun ivm-initialize-vm-summary-mode ()
  (ivm-set-mode-line-format))

;; ----------
(defun ivm-initialize-u-vm-color ()
  ;;(remove-hook 'vm-mode-hook              'vma-folder-font-lock-setup)
  ;;(remove-hook 'vm-presentation-mode-hook 'vma-folder-font-lock-setup)

  (add-hook 'vm-summary-mode-hook   'u-vm-color-summary-mode)
  ;;(add-hook 'vm-select-message-hook 'u-vm-color-fontify-buffer)

  (mapc (lambda (fn)
          (eval `(defadvice ,fn (after u-vm-color activate)
                   (save-excursion
                     (when vm-presentation-buffer
                       (set-buffer vm-presentation-buffer))
                     (u-vm-color-fontify-buffer)))))
    '(vm-fill-paragraphs-containing-long-lines
      vm-preview-current-message
      vm-show-current-message))

  (mapc (lambda (elt)
          (let ((face (car elt))
                (attrs (cdr elt)))
            (unless (keywordp (car attrs))
              (setq attrs (cons :inherit attrs)))
            (apply 'override-face-attributes face nil
                   :foreground nil
                   :background nil
                   :slant 'unspecified
                   :weight 'unspecified
                   attrs)))
    '((u-vm-color-default-face       default)

      ;; summary sections
      (u-vm-color-number-face        u-vm-color-default-face)
      (u-vm-color-label-face         font-lock-doc-face)
      (u-vm-color-attribute-face     font-lock-doc-face)
      (u-vm-color-length-face        u-vm-color-default-face)
      (u-vm-color-id-face            u-vm-color-default-face)
      (u-vm-color-user-face          u-vm-color-default-face)

      ;; headers and header contents
      (u-vm-color-header-face        u-vm-color-default-face)
      (u-vm-color-author-face        :foreground "cornflower blue")
      (u-vm-color-recipient-face     font-lock-constant-face)
      (u-vm-color-subject-face       font-lock-function-name-face)
      (u-vm-color-date-face          font-lock-builtin-face)
      (u-vm-color-time-face          font-lock-builtin-face)

      ;; message body
      (u-vm-color-spamassassin-face  font-lock-warning-face)
      (u-vm-color-signature-face     font-lock-comment-face)
      (u-vm-color-citation-1-face    font-lock-string-face)
      (u-vm-color-citation-2-face    font-lock-function-name-face)
      (u-vm-color-citation-3-face    font-lock-type-face)
      (u-vm-color-citation-4-face    font-lock-builtin-face)
      (u-vm-color-citation-5-face    font-lock-constant-face))))


;;;;;;
;;; VM variables
;;;;;;

;; Make sure field numbers are padded with zeros and full name is
;; actually only 3 chars.
(setq vm-month-alist
      '(("jan" "Jan" "01")
        ("feb" "Feb" "02")
        ("mar" "Mar" "03")
        ("apr" "Apr" "04")
        ("may" "May" "05")
        ("jun" "Jun" "06")
        ("jul" "Jul" "07")
        ("aug" "Aug" "08")
        ("sep" "Sep" "09")
        ("oct" "Oct" "10")
        ("nov" "Nov" "11")
        ("dec" "Dec" "12")))

;; Headers I want to see and give top priority to.
;; This is an actual variable used by vm.
(setq vm-visible-headers
      '("From:"
        "To:"
        "Apparently-To:"
        "Cc:"
        "Subject:"
        "Date:"
        "^Reply-To:"
        "Resent-From:"
        "Resent-To:"))

;; Headers not to show unless I explicitly want ALL headers
(setq vm-invisible-header-regexp
      (concat "^\\(?:"
              (mapconcat 'identity
                         '("Authentication-Results"
                           "Blat"
                           "Bounces-To"
                           "Content-.*"
                           ".*-Signature"
                           "Delivered-To"
                           "Errors-To"
                           "Geek-.*"
                           "Importance"
                           "In-Reply-To"
                           "Lines"
                           "List-.*"
                           "Mail-From"
                           "Mailer"
                           "Managed-by"
                           "Message-Id"
                           "Mime-.*"
                           "NNTP-Posting-Host"
                           "Path"
                           "Posted-Date"
                           "Precedence"
                           "RT-.*"
                           "Received"
                           "Received-Date"
                           "Received-SPF"
                           "References"
                           "Resent-Message-Id"
                           "Return-Path"
                           "Sender"
                           "Status"
                           "Thread-.*"
                           "Tomato"
                           "User-Agent"
                           "Via"
                           "X-.*"
                           "X400-.*"
                           "Xref"
                           )
                         "\\|")
              "\\):"))

(setq vm-summary-uninteresting-senders
      (vm-multdom-address-list-regexp vm-multdom-user-addresses))

(setq vm-reply-ignored-addresses
      (list vm-summary-uninteresting-senders))

(setq vm-resend-bounced-headers
      '("From:"
        "To:"
        "Cc:"
        "Subject:"
        "Reply-To:"
        "Resent-"
        "In-Reply-To:"
        "References:"
        "Organization:"
        "X-"))

(setq vm-resend-bounced-discard-header-regexp
      (mapconcat 'identity
                 '("Message-Id"
                   "Received"
                   "Return-Path"
                   "Sender")
                 "\\|"))

;; For normal forwarding, put these headers first.
(setq vm-forwarded-headers
      (copy-sequence vm-visible-headers))

;; For normal forwarding, strip these headers
(setq vm-unforwarded-header-regexp
      (mapconcat 'identity
                 '("Errors-To"
                   "In-Reply-To"
                   "Precedence"
                   "Received"
                   "Return-Path"
                   "Sender"
                   "X-.*"
                   "X400-.*")
                 "\\|"))

;; The HEADER string (e.g. Subject, Cc, To, etc.) should also be a regexp
;; to avoid duplicating lists... Hopefully this will be fixed someday.
(setq vm-auto-folder-alist
      (let ((from-alist
             '(("<\\([^%@> \t]+\\)"    . (vma-afa-match-format "%s" 1))
               ("\\([^%@ \t]+\\)"      . (vma-afa-match-format "%s" 1))))

            (to-alist
             '(("\\([^-@%]*-hackers@.*gnu\\)"
                . (vma-afa-match-format "../org/fsf.org/%s" 1))
               ("\\(fsf-[^@%]*\\)"
                . (vma-afa-match-format "../org/fsf.org/%s" 1))
               ("ange-ftp-lovers"       . "../misc/ange-ftp-lovers")
               ("arcana@"               . "../fun/emacs-arcana")
               ("bug-gnu-cdrom@"        . "../bugs/cdlndir")
               ("bug-texinfo@"          . "../bugs/texinfo")
               ("bull-edit"             . "../org/fsf.org/bull")
               ("ccp@red-bean"          . "../cvs/ccp")
               ("cyclic@"               . "../cvs/cyclic")
               ("cyclic-cvs@"           . "../cvs/cyclic-cvs")
               ;; Keith Bostic doesn't address devnull mail to /dev/null
               ;; anymore, as of August 1997.  Apparently, some mail
               ;; servers complained about attempts to mail directly to files.
               ;("/dev/null@"           . "../fun/devnull")
               ("eunuch-haters@"        . "../fun/unix-haters")
               ("gnu-emacs-sources@"
                . '(("Subject"
                     ("\\(Re:[ \t]+\\)?\\([^ \t\n/]+\\.el\\)"
                      . (vma-afa-match-format "~/lib/elisp/common/%s" 2))
                     ("\\(Re:[ \t]+\\)?\\([^ \t\n/]+\\)"
                      . (vma-afa-match-format "~/lib/elisp/common/%s.el" 2)))))
               ("info-explorer@"        . "../fun/info-explorer")
               ("info-gnu@gnu.org"      . "../org/fsf.org/info-gnu")
               ("its-lovers@"           . "../fun/its-lovers")
               ("league.*@"             . "../org/lpf.org/lpf-todo")
               ("lpf.*@"                . "../org/lpf.org/lpf-todo")
               ("monkeybutter@jwz.org"  . "../fun/monkeybutter")
               ("m17n[0-9]+@etl.go.jp"  . "../emacs/mule-symposium")
               ("naughty-swiss@"        . "../fun/naughty-swiss")
               ("nev@bostic.com"        . "../fun/devnull")
               ("npl@netscape.com"      . "../org/mozilla.org/npl")
               ("qotd@"                 . "../fun/qotd")
               ("request@*.gnu"         . "../org/fsf.org/request")
               ("runkle-posse@"         . "../fun/runkle-posse")
               ("staff@mozilla.org"     . "../org/mozilla.org/staff")
               ("texinfo-maintainers@"  . "../bugs/texinfo")
               ("unix-haters@"          . "../fun/unix-haters")))

            (subject-alist
             '(("\\([^ \t\n/]+\\)\\.el"
                 . (vma-afa-match-format "../bugs/emacs/%s" 1))
                ("GSB"                  . "../fun/gsb-announce")
                ("New account: .*"      . "../org/fsf.org/request")
                ("account"              . "../org/fsf.org/request")
                ("AC_"                  . "../bugs/autoconf")
                ("acconfig\\.h"         . "../bugs/autoconf")
                ("autoconf"             . "../bugs/autoconf")
                ("autoheader"           . "../bugs/autoconf")
                ("changelogmon"         . "../bugs/diffmon")
                ("diffmon"              . "../bugs/diffmon")
                ("info"                 . "../bugs/texinfo")
                ("ispell"               . "../bugs/ispell")
                ("makeinfo"             . "../bugs/texinfo")
                ("patent"               . "../org/lpf.org/lpf-todo")
                ("texinfo"              . "../bugs/texinfo")
                ("type-break"           . "../bugs/emacs/type-break"))))

        (list (cons "To\\|Cc" to-alist)
              (cons "Subject" subject-alist)
              (cons "From"    from-alist))))

(setq vm-auto-folder-case-fold-search          t)
(setq vm-auto-get-new-mail                     nil)
(setq vm-auto-next-message                     nil)
(setq vm-berkeley-mail-compatibility           t)
(setq vm-burst-digest-messages-inherit-labels  t)
(setq vm-check-folder-types                    t)
(setq vm-circular-folders                      t)
;; causes bugs with typing nonexistent folder names to vm-create-imap-folder
(setq vm-completion-auto-correct               nil)
(setq vm-confirm-mail-send                     t)
(setq vm-confirm-new-folders                   t)
(setq vm-confirm-quit                          'ask)
(setq vm-delete-empty-folders                  t)
(setq vm-digest-send-type                      "mime")
(setq vm-edit-message-mode                     'text-mode)
(setq vm-flush-interval                        nil)
(setq vm-folder-directory                      mail-folder-directory)
(setq vm-forwarding-digest-type                vm-digest-send-type)
(setq vm-forwarding-subject-format             "[%f: %s]")
(setq vm-group-by                              "subject")
(setq vm-highlighted-header-regexp             nil)
(setq vm-honor-page-delimiters                 nil)
(setq vm-included-text-prefix                  ">")
(setq vm-index-file-suffix                     ".vmidx")
(setq vm-inhibit-startup-message               t)
(setq vm-keep-sent-messages                    5)
(setq vm-mail-check-interval                   nil)
;(setq vm-mail-window-percentage                80) ; obsolete variable
(setq vm-move-messages-physically              nil)
(setq vm-mutable-windows                       nil)
(setq vm-preview-lines                         0)
(setq vm-preview-read-messages                 t)
(setq vm-reply-subject-prefix                  "Re: ")
(setq vm-review-lines                          0)
(setq vm-skip-deleted-messages                 t)
(setq vm-skip-read-messages                    t)
(setq vm-strip-reply-headers                   t)
(setq vm-tale-is-an-idiot                      t)
(setq vm-visit-when-saving                     nil)

(setq vm-delete-after-saving                   t)
(setq vm-delete-after-archiving                t)
(setq vm-delete-after-bursting                 t)

(setq vm-move-after-deleting                   0)
(setq vm-move-after-undeleting                 nil)
(setq vm-move-after-killing                    nil)

;; mime text/plain presentation formatting; does not modify buffer
(setq vm-paragraph-fill-column                 (default-value 'fill-column))
(setq vm-fill-paragraphs-containing-long-lines nil)

;; Don't use these since they are taken care of via fmailutils hacks.
(setq vm-mail-header-from                 nil)
(setq vm-mail-header-insert-date          nil)
(setq vm-mail-header-insert-message-id    nil)

;; VM 8.1.1:
;; It creeps me out that I can't see things like the References header when
;; I reply; it makes me worry that the message will go out unthreaded.
(setq vm-mail-mode-hidden-headers         nil)

;; pop-related options
(setq vm-pop-md5-program                  "md5sum")
(setq vm-pop-max-message-size             nil) ; (* 1024 64)
(setq vm-pop-messages-per-session         nil)
(setq vm-pop-bytes-per-session            nil)
(setq vm-pop-expunge-after-retrieving     t)
(setq vm-pop-auto-expunge-alist           nil)
(setq vm-pop-read-quit-response           t)

;; imap-related options
(setq vm-imap-max-message-size            vm-pop-max-message-size)
(setq vm-imap-messages-per-session        nil)
(setq vm-imap-bytes-per-session           nil)
(setq vm-imap-expunge-after-retrieving    t)

;; summary-related options
(setq vm-auto-center-summary              nil)
(setq vm-follow-summary-cursor            t)
(setq vm-startup-with-summary             nil) ; 'split-screen
(setq vm-summary-arrow                    ">")
(setq vm-summary-thread-indent-level      2)
(setq vm-thread-using-subject             t)

;; frame-related options
(setq vm-frame-per-completion             nil)
(setq vm-frame-per-composition            nil)
(setq vm-frame-per-edit                   nil)
(setq vm-frame-per-folder                 nil)
(setq vm-frame-per-summary                nil)
(setq vm-mutable-frames                   nil)

;; window configuration --related options
(setq vm-window-configuration-file        nil)
(setq vm-supported-window-configurations  nil)
(setq vm-default-window-configuration     nil)

;; pixmap directory configuration
(setq vm-image-directory
      (let ((vmsrc (locate-library "vm.el" t)))
        (and (stringp vmsrc)
             (concat (file-name-directory vmsrc) "pixmaps/"))))
(setq vm-toolbar-pixmap-directory vm-image-directory)

;(setq vm-in-reply-to-format "%F's message of %w, %d %m %y %h %z %i")
;; I used to use <%f>, but some MUAs (e.g. netscape) would interpret it as a
;; message id and screw up threading.  It also seems to cause problems for
;; some newsgate software.
(setq vm-in-reply-to-format "%i (%f %w, %d %m %y %h %z)")

(setq vm-included-text-attribution-format nil)
;(setq vm-included-text-attribution-format "%F writes:")

;(setq vm-summary-format "%2n%a%*[%f] %y-%2M-%2d %H %l/%c \"%s\"\n")
;(setq vm-summary-format "%2n%a%*%-15.15f %2M-%2d %H %3l/%-5c %I%s\n")
(setq vm-summary-format "%2n%a%*%-20.20F %y-%2M-%2d %H %6c %I%s\n")

(setq vm-primary-inbox (concat mail-folder-directory "inbox"))
(vma-set-spool-file vm-primary-inbox
                    (concat ivm-incoming-mail-spool-directory
                            "/" (user-login-name)))


;; Imap drop list -- VM 8.1.0 and later
(setq vm-imap-account-alist
      '(("imap-ssl:nwc.splode.com:993:*:login:noah:*"    "nwc")
        ("imap-ssl:doh.perforce.com:993:*:login:noahf:*" "doh")))

(setq vm-imap-folder-cache-directory "~/etc/mail/imap-cache/")


;; MIME support --- VM 6.0 and later
(setq vm-auto-decode-mime-messages         nil)
(setq vm-mime-decode-for-preview           nil)
(setq vm-display-using-mime                t)
(setq vm-honor-mime-content-disposition    nil)
(setq vm-infer-mime-types                  t)
(setq vm-mime-8bit-text-transfer-encoding  'quoted-printable) ; '8bit
(setq vm-mime-attachment-save-directory    "~/")
(setq vm-mime-avoid-folding-content-type   t)
(setq vm-mime-composition-armor-from-lines t)
(setq vm-mime-max-message-size             nil)
(setq vm-mime-use-w3-for-text/html         nil)
(setq vm-send-using-mime                   t)

(setq vm-mime-delete-after-saving          nil)
(setq vm-mime-confirm-delete               t)

(setq vm-mime-base64-decoder-program
      (file-in-pathlist-p "base64-decode" exec-path))
(setq vm-mime-base64-encoder-program
      (file-in-pathlist-p "base64-encode" exec-path))
(setq vm-mime-qp-decoder-program
      (file-in-pathlist-p "qp-decode" exec-path))
(setq vm-mime-qp-encoder-program
      (file-in-pathlist-p "qp-encode" exec-path))

(and (eq (emacs-variant) 'emacs)
     (or (eq window-system 'x)
         (getenv "DISPLAY"))
     (add-list-members 'vm-mime-external-content-types-alist
       '("image" "xv")
       '("video" "gmplayer")))

(mapc (lambda (pair)
        (set-alist-slot 'vm-mime-attachment-auto-type-alist
                        (car pair) (cdr pair) nil nil 'append))
  ivm-mime-attachment-auto-type-alist)

;; By default we do not try to render html internally.
;; See ivm-toggle-use-w3.
(add-to-list 'vm-mime-internal-content-type-exceptions "text/html")

;; Do not display images immediately, either via inline rendering or by
;; launching xv, since I may be in a screened emacs from another workstation.
(and (boundp 'vm-auto-displayed-mime-content-types)
     vm-auto-displayed-mime-content-types
     (setq vm-auto-displayed-mime-content-types
           (delete "image" vm-auto-displayed-mime-content-types)))


;; vm-addons setup

(add-hook 'vm-mode-hook              'vma-folder-font-lock-setup)
(add-hook 'vm-presentation-mode-hook 'vma-folder-font-lock-setup)

(add-hook 'vm-arrived-messages-hook 'vma-kill-duplicate-messages-by-message-id)
(add-hook 'vm-arrived-messages-hook 'vma-kill-messages-by-descriptor)

(vma-folder-font-lock-match-me-multdom)

;; Increase chance of fontification finishing entire header contents.
(when (and (boundp 'jit-lock-chunk-size)
           (numberp jit-lock-chunk-size))
  (setq jit-lock-chunk-size
        (max 1024 jit-lock-chunk-size)))


(vm-multdom-install)

(defalias 'vm-peek 'vma-peek)

(add-hook 'mail-setup-hook 'ivm-mail-setup)

(add-hook 'vm-display-buffer-hook 'ivm-display-buffer)

(add-hook 'vm-mode-hook              'ivm-initialize-vm-mode)
(add-hook 'vm-presentation-mode-hook 'ivm-initialize-vm-presentation-mode)
(add-hook 'vm-summary-mode-hook      'ivm-initialize-vm-summary-mode)

(add-hook 'vm-forward-message-hook (lambda () (mail-position-on-field "To")))

(add-hook 'vm-mail-mode-hook (lambda () (set-buffer-modified-p nil)))

(add-hook 'mail-citation-hook 'vma-mail-yank-default)
(add-hook 'mail-citation-hook 'ivm-lat1conv-to-ascii-query 'append)

;; Some MS Outlook clients send messages with this charset.
;; Don't add this anymore with VM 8.1; convert it to utf8.
;(add-to-list 'vm-mime-default-face-charsets "Windows-1252")


;; VM version 6 already has a mime decoding function on this key.
(cond ((string-lessp vm-version "6")
       (autoload 'qpdecode-decode-message "qpdecode" nil t)
       (define-key vm-mode-map "D" 'qpdecode-decode-message)))

(keymap-define-keys vm-mode-map
  '(;; These commands are usually defined on vm-mime-reader-map but that is
    ;; not linked into the global kepmap, or so it appears; it's only bound
    ;; to a mouse button.
    ("$s"   . vm-mime-reader-map-save-message)
    ("$w"   . vm-mime-reader-map-save-file)
    ("$|"   . vm-mime-reader-map-pipe-to-command)
    ("$p"   . vm-mime-reader-map-pipe-to-printer)
    ("$\r"  . vm-mime-reader-map-display-using-default)
    ("$d"   . vm-delete-mime-object)
    ("$e"   . vm-mime-reader-map-display-using-external-viewer)

    ;; These are my personal bindings
    ("$$"   . vma-send-digest-all-headers)
    ("A"    . nil)
    ("Z"    . vm-forward-message-all-headers)
    ("\C-?" . vm-scroll-backward)))

(define-key vm-mail-mode-map "\C-n"  'mail-abbrev-next-line)
(define-key global-map       "\C-xm" 'vm-mail)

(add-forms-to-after-load-alist
  `(("bbdb"
     (bbdb-insinuate-vm))

    ("u-vm-color"
     (ivm-initialize-u-vm-color))))

(provide 'init-vm)

;;; init-vm.el ends here.
