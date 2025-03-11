(defvar touchpad-scroll-speed 3 "Scroll speed multiplier.")
(defvar touchpad-momentum-decay 0.93 "Scroll speed decay multiplier. Lower values make the scroll stop faster.")
(defvar touchpad-sensitivity 0.65
  "Determines how susceptible the scroll is to changes in speed in the raw input.
Higher values may make the scroll more responsive, but also more jittery.
Possible values are between 0 and 1.")
(defvar touchpad-frame-rate 60 "Frame rate of the scroll, in Hz.")
(defvar touchpad-pixel-scroll nil
  "If non-nil, use pixel scrolling instead of line scrolling.
If nil, scroll position will be rounded to the nearest line,
but momentum will still behave as normal.
To use pixel scrolling, pixel-scroll-precision-mode must first be enabled
before enabling touchpad-scroll-mode.")
(defvar touchpad-debug nil "If non-nil, print debug messages.")

(defvar touchpad--scroll-momentum 0)
(defvar touchpad--prev-delta 0)
(defvar touchpad--residual 0)
(defvar touchpad--scroll-window)

(defvar touchpad--scroll-timer nil)

(defun touchpad--sign (x)
  (if (> x 0) 1 (if (< x 0) -1 0)))

(defvar touchpad-ultra-scroll nil
  "If non-nil, use ultra-scroll instead of pixel-scroll-precision-mode.
Requires touchpad-pixel-scroll to be non-nil and ultra-scroll to be loaded.")

(defun touchpad--pixel-scroll-up (delta)
  (if touchpad-ultra-scroll
      (ultra-scroll-up delta)
    (pixel-scroll-precision-scroll-up delta)))

(defun touchpad--pixel-scroll-down (delta)
  (if touchpad-ultra-scroll
      (ultra-scroll-down delta)
    (pixel-scroll-precision-scroll-down delta)))

(defun touchpad--do-scroll (delta window)
  (condition-case nil
      (progn
        (with-selected-window window
          (if touchpad-pixel-scroll
              (if (< delta 0)
                  (touchpad--pixel-scroll-up (- (floor delta)))
                (touchpad--pixel-scroll-down (floor delta)))
            (let ((line-delta (- touchpad--residual (/ delta (touchpad--line-height 1 window)))))
              (scroll-down (floor line-delta))
              (setq touchpad--residual (- line-delta (floor line-delta)))))))
    (beginning-of-buffer
     (message (error-message-string '(beginning-of-buffer)))
     (setq touchpad--scroll-momentum 0))
    (end-of-buffer
     (message (error-message-string '(end-of-buffer)))
     (setq touchpad--scroll-momentum 0))))

(defvar touchpad--cached-line-height 0)
(defun touchpad--line-height (line window)
  "Return the height of a line in pixels."
  (let ((line-height (window-line-height line window)))
    (if line-height
        (setq touchpad--cached-line-height (+ (car line-height) (or (nth 3 line-height) 0)))
      (when touchpad-debug (message "line-height not found, using %s" touchpad--cached-line-height))
      touchpad--cached-line-height)))

(defun touchpad-speed-curve (delta)
  "Function which maps the raw delta to a scroll speed."
  (* (expt (abs delta) 0.9) (touchpad--sign delta)))

(defun touchpad--scroll-start-momentum ()
  "Start scrolling."
  (unless touchpad--scroll-timer
    (setq touchpad--scroll-timer (run-with-timer 0 (/ 1.0 touchpad-frame-rate) 'touchpad--scroll-momentum))
    (setq gc-cons-threshold (* gc-cons-threshold 100))))

(defun touchpad--scroll-stop-momentum ()
  "Stop scrolling."
  (when touchpad--scroll-timer
    (cancel-timer touchpad--scroll-timer)
    (setq touchpad--scroll-timer nil)
    (setq gc-cons-threshold (/ gc-cons-threshold 100))))

(defun touchpad-scroll-touchpad (event)
  "Change the momentum based on the scroll event."
  (interactive "e")
  (let ((delta (cdr (nth 4 event)))
        (window (mwheel-event-window event)))
    (when delta
      (setq delta (touchpad-speed-curve (- delta)))
      (if (and (eq (touchpad--sign delta) (touchpad--sign touchpad--prev-delta))
               (or (> (abs delta) (* (min (abs touchpad--prev-delta) (abs touchpad--scroll-momentum)) touchpad-sensitivity))
                   (< (max (abs delta) (abs touchpad--prev-delta)) 10)))
          (progn
            (setq touchpad--scroll-momentum delta)
            (setq touchpad--scroll-window window)
            (touchpad--scroll-start-momentum)
            (when touchpad-debug (message "%s" (round delta))))
        (when touchpad-debug (message "%s*" (round delta))))
      (setq touchpad--prev-delta delta))))

(defvar touchpad--touchscreen-prev-y)
(defvar touchpad--prev-timestamp)
(defun touchpad-scroll-touchscreen-start (event)
  "Start scrolling based on the touchscreen touch start event."
  (interactive "e")
  (setq touchpad--touchscreen-prev-y (cdr (nth 3 (cadr event))))
  (setq touchpad--prev-timestamp (float-time))
  (setq touchpad--scroll-window (cadr (cadr event)))
  (touchpad--scroll-stop-momentum)
  (when touchpad-debug (prin1 touchpad--touchscreen-prev-y)))
(defun touchpad-scroll-touchscreen (event)
  "Change the momentum based on the touchscreen event."
  (interactive "e")
  (let ((time-diff (- (float-time) touchpad--prev-timestamp)))
    (when (>= time-diff (/ 1.0 touchpad-frame-rate))
      (when (eq (length (cadr event)) 1)
        (let* ((y (cdr (nth 3 (car (cadr event)))))
               (delta (- y touchpad--touchscreen-prev-y)))
          (touchpad--do-scroll (- delta) touchpad--scroll-window)
          (setq touchpad--touchscreen-prev-y y)
          (setq touchpad--prev-delta delta)))
      (setq touchpad--scroll-momentum (/ (/ touchpad--prev-delta time-diff) -60))
      (setq touchpad--prev-timestamp (float-time)))))
(defun touchpad-scroll-touchscreen-end ()
  "Delegate scrolling to momentum, after a touchscreen end touch event."
  (interactive)
  (let ((time-diff (- (float-time) touchpad--prev-timestamp)))
    (when (<= time-diff (/ 5.0 touchpad-frame-rate))
      (touchpad--scroll-start-momentum))))

(defun touchpad--scroll-momentum ()
  "Scroll the window based on the momentum."
  (let ((delta (* touchpad--scroll-momentum
                  (/ 60.0 touchpad-frame-rate)
                  touchpad-scroll-speed)))
    (if (>= (abs touchpad--scroll-momentum) 1)
        (progn
          (touchpad--do-scroll delta touchpad--scroll-window)
          (setq touchpad--scroll-momentum (* touchpad--scroll-momentum (expt touchpad-momentum-decay (/ 60.0 touchpad-frame-rate)))))
      (setq touchpad--scroll-momentum 0))
    (when (eq touchpad--scroll-momentum 0)
      (touchpad--scroll-stop-momentum))))

(defvar touchpad-scroll-mode-map (make-sparse-keymap))
(define-key touchpad-scroll-mode-map [wheel-up] 'touchpad-scroll-touchpad)
(define-key touchpad-scroll-mode-map [wheel-down] 'touchpad-scroll-touchpad)
(define-key touchpad-scroll-mode-map [touchscreen-begin] 'touchpad-scroll-touchscreen-start)
(define-key touchpad-scroll-mode-map [touchscreen-update] 'touchpad-scroll-touchscreen)
(define-key touchpad-scroll-mode-map [touchscreen-end] 'touchpad-scroll-touchscreen-end)

(define-minor-mode touchpad-scroll-mode
  "Toggle touchpad scroll mode."
  :global t
  :keymap touchpad-scroll-mode-map
  (if touchpad-scroll-mode
      (setq mwheel-coalesce-scroll-events nil)
    (setq mwheel-coalesce-scroll-events t)
    (touchpad--scroll-stop-momentum)))

