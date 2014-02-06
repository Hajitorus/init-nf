;; $Id: xbindkeysrc.scm,v 1.4 2012/03/05 16:59:37 friedman Exp $
;; This configuration is guile based.

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 format))


(define (bt1 . command)
  (read-delimited "\r\n" (apply open-pipe* OPEN_READ command)))

(define (bt . command)
  (read-delimited "" (apply open-pipe* OPEN_READ command)))

(define (pipe-pid port)
  ;; Comes from popen module
  (hashq-ref port/pid-table port))


(define osd-port #f)

(define osd-command
  '("osd_cat"
    "--color"   "green"
    "--pos"     "bottom"
    "--offset"  "100"
    "--indent"   "60"
    "--delay"     "3"
    "--shadow"    "2"
    "--lines"     "2"
    "--font"    "-*-bitstream vera sans-medium-r-*-*-24-*-*-*-*-*-*-*"))

(define (osd-cancel-existing)
  (if osd-port
      (let ((pid (pipe-pid osd-port)))
        (if pid
            (begin
              (catch #t
                     (lambda () (kill pid SIGTERM))
                     (lambda ignored ignored))
              (set! osd-port #f))))))

(define (osd text . args)
  (osd-cancel-existing)
  (let ((port (apply open-pipe* OPEN_WRITE (append osd-command args))))
    (if text
        (write-line text port))
    (set! osd-port port)))

(define (osd-pct pct text)
  (osd #f "--barmode" "percentage" "--percentage" pct "--text" text))


(define (touchpad-enabled)
  (let ((synclient (bt "synclient")))
    (if (string-match "TouchpadOff += +0" synclient) #t #f)))

(define (toggle-touchpad)
  (cond ((touchpad-enabled)
         (system* "synclient" "TouchpadOff=1")
         (osd "Touchpad disabled"))
        (else
         (system* "synclient" "TouchpadOff=0")
         (osd "Touchpad enabled"))))


(define (modify-brightness direction)
  (let ((pct (bt1 "set-brightness" direction)))
    (osd-pct pct (format #f "Brightness ~a%" pct))))

(define (increase-brightness)
  (modify-brightness "increase"))

(define (decrease-brightness)
  (modify-brightness "decrease"))


(xbindkey-function 'XF86TouchpadToggle    toggle-touchpad)
(xbindkey-function 'XF86MonBrightnessUp   increase-brightness)
(xbindkey-function 'XF86MonBrightnessDown decrease-brightness)

(xbindkey 'XF86Launch1 "pavucontrol")
(xbindkey 'XF86WebCam  "cheese")

(debug-enable 'backtrace 'debug)

;; eof
