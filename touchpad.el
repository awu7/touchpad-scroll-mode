(defvar touchpad-scroll-speed 3 "Scroll speed multiplier.")
(defvar touchpad-momentum-decay 0.93 "Scroll speed decay multiplier. Lower values make the scroll stop faster.")
(defvar touchpad-sensitivity 0.65
  "Determines how susceptible the scroll is to changes in speed in the raw input.
Higher values may make the scroll more responsive, but also more jittery.
Possible values are between 0 and 1.")
(defvar touchpad-touchscreen-correction-factor 0.6
  "Multiplier to scroll momentum upon touchscreen release.")
(defvar touchpad-mwheel-threshold 50)
(defvar touchpad-frame-rate 60 "Frame rate of the scroll, in Hz.")
(defvar touchpad-pixel-scroll nil
  "If non-nil, use pixel scrolling instead of line scrolling.
If nil, scroll position will be rounded to the nearest line,
but momentum will still behave as normal.
To use pixel scrolling, pixel-scroll-precision-mode must first be enabled
before enabling touchpad-scroll-mode.")
(defvar touchpad-hscroll t
  "If non-nil, enable horizontal scrolling.")
(defvar touchpad-diagonal-threshold 0.6
  "Threshold between 0 and 1 which controls how much scrolling attempts to only
move along one axis. Lower values make it easier to scroll diagonally.
This variable only has an effect if `touchpad-hscroll' is non-nil.")
(defvar touchpad-ultra-scroll nil
  "If non-nil, use ultra-scroll instead of pixel-scroll-precision-mode.
Requires touchpad-pixel-scroll to be non-nil and ultra-scroll to be loaded.")
(defvar touchpad-debug nil "If non-nil, print debug messages.")

(defvar touchpad--scroll-window)

(defvar touchpad--repeat-deltas nil)
(defvar touchpad--jump-deltas nil)
(defvar touchpad--prev-raw-delta-y 0)

(defvar touchpad--x '(0 0 0))
(defvar touchpad--y '(0 0 0))
(defmacro touchpad--momentum (axis)
  `(car ,axis))
(defmacro touchpad--prev-delta (axis)
  `(cadr ,axis))
(defmacro touchpad--residual (axis)
  `(caddr ,axis))
(defvar touchpad--axes (list touchpad--x touchpad--y))

(defvar touchpad--scroll-timer nil)

(defun touchpad--sign (x)
  (if (> x 0) 1 (if (< x 0) -1 0)))

(defun touchpad--cons-to-list (c)
  (list (car c) (cdr c)))

(defun touchpad--fix-dir (d)
  (let ((max (apply 'max (mapcar 'abs d))))
    (mapcar (lambda (v)
              (if (> (abs v) (* max touchpad-diagonal-threshold))
                  v
                0))
            d)))

(defun touchpad--pixel-scroll (delta window)
  (if touchpad-ultra-scroll
      (ultra-scroll--scroll (- delta) window)
    (with-selected-window window
      (if (< delta 0)
          (pixel-scroll-precision-scroll-up (- delta))
        (pixel-scroll-precision-scroll-down delta)))))

(defun touchpad--discrete-scroll (delta window axis step-size scroll-fn)
  (let ((step-delta (- (touchpad--residual axis) (/ (float delta) step-size))))
    (with-selected-window window
      (funcall scroll-fn (floor step-delta)))
    (setf (touchpad--residual axis) (- step-delta (floor step-delta)))))

(defun touchpad--do-scroll (delta window)
  (let ((dx (car delta))
        (dy (cadr delta)))
    (condition-case nil
        (if (and touchpad-pixel-scroll
                 (zerop (window-hscroll window))) ;; pixel scrolling breaks when there is nonzero hscroll
            (touchpad--pixel-scroll (floor dy) window)
          (touchpad--discrete-scroll dy window touchpad--y (touchpad--line-height 1 window) 'scroll-down))
      (beginning-of-buffer
       (message (error-message-string '(beginning-of-buffer)))
       (setf (touchpad--momentum touchpad--y) 0))
      (end-of-buffer
       (message (error-message-string '(end-of-buffer)))
       (setf (touchpad--momentum touchpad--y) 0)))
    (when touchpad-hscroll
      (touchpad--discrete-scroll dx window touchpad--x (frame-char-width (window-frame window)) 'scroll-left))))

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
  (setq touchpad--prev-raw-delta-y 0)
  (when touchpad--scroll-timer
    (cancel-timer touchpad--scroll-timer)
    (setq touchpad--scroll-timer nil)
    (setq gc-cons-threshold (/ gc-cons-threshold 100))))

(defun touchpad-scroll-touchpad (event)
  "Change the momentum based on the scroll event.
Also detects mouse wheel vs touchpad scroll using a heuristic:
If all 4 of these checks are true, then this is probably a mouse wheel event:
1. The x delta is 0.
2. The y delta is big (> `touchpad-mwheel-threshold').
3. The exact y delta has occurred twice in a row.
4. The y delta has occurred as the first delta in a scroll."
  (interactive "e")
  (let ((delta (touchpad--cons-to-list (nth 4 event)))
        (window (mwheel-event-window event)))
    ;; mouse detection
    (let ((dx (abs (car delta)))
          (dy (abs (cadr delta))))
      (when (and (= dx 0)
                 (> dy touchpad-mwheel-threshold))
        (when (= dy touchpad--prev-raw-delta-y)
          (add-to-list 'touchpad--repeat-deltas dy))
        (when (= touchpad--prev-raw-delta-y 0)
          (add-to-list 'touchpad--jump-deltas dy)))
      (setq touchpad--prev-raw-delta-y dy)
      (if (and (= dx 0)
               (member dy touchpad--repeat-deltas)
               (member dy touchpad--jump-deltas))
          (progn
            (touchpad--scroll-stop-momentum)
            (mwheel-scroll event))

        ;; touchpad scroll
        (setq delta (touchpad--fix-dir delta))
        (cl-mapc (lambda (axis delta)
                   (when (eq axis touchpad--y)
                     (setq delta (touchpad-speed-curve (- delta))))
                   (when (and (eq (touchpad--sign delta) (touchpad--sign (touchpad--prev-delta axis)))
                              (or (> (abs delta) (* (min (abs (touchpad--prev-delta axis)) (abs (touchpad--momentum axis))) touchpad-sensitivity))
                                  (< (max (abs delta) (abs (touchpad--prev-delta axis))) 10)))
                     (setf (touchpad--momentum axis) delta)
                     (setq touchpad--scroll-window window)
                     (touchpad--scroll-start-momentum))
                   (when touchpad-debug (message "%s*" (round delta)))
                   (setf (touchpad--prev-delta axis) delta))
                 touchpad--axes delta)))))

(defvar touchpad--touchscreen-prev-pos nil)
(defvar touchpad--prev-timestamp)
(defun touchpad-scroll-touchscreen-start (event)
  "Start scrolling based on the touchscreen touch start event."
  (interactive "e")
  (setq touchpad--touchscreen-prev-pos (nth 3 (caadr event)))
  (setq touchpad--prev-timestamp (float-time))
  (setq touchpad--scroll-window (cadr (caadr event)))
  (touchpad--scroll-stop-momentum)
  (when touchpad-debug (prin1 touchpad--touchscreen-prev-pos)))
(defun touchpad-scroll-touchscreen (event)
  "Change the momentum based on the touchscreen event."
  (interactive "e")
  (when (not touchpad--touchscreen-prev-pos)
    (touchpad-scroll-touchscreen-start event))
  (let ((time-diff (- (float-time) touchpad--prev-timestamp)))
    (when (>= time-diff (/ 1.0 touchpad-frame-rate))
      (let* ((pos (nth 3 (car (cadr event))))
             (raw-dx (- (car pos) (car touchpad--touchscreen-prev-pos)))
             (raw-dy (- (cdr pos) (cdr touchpad--touchscreen-prev-pos)))
             (delta (touchpad--fix-dir (list raw-dx raw-dy)))
             (dx (car delta))
             (dy (cadr delta)))
        (when (eq (length (cadr event)) 1)
          (touchpad--do-scroll (list dx (- dy)) touchpad--scroll-window)
          (setq touchpad--touchscreen-prev-pos pos)
          (setf (touchpad--prev-delta touchpad--x) dx)
          (setf (touchpad--prev-delta touchpad--y) dy))
        (setf (touchpad--momentum touchpad--x) (/ (touchpad--prev-delta touchpad--x) (min 1 (* time-diff 60))))
        (setf (touchpad--momentum touchpad--y) (- (/ (touchpad--prev-delta touchpad--y) (min 1 (* time-diff 60)))))
        (setq touchpad--prev-timestamp (float-time))))))
(defun touchpad-scroll-touchscreen-end ()
  "Delegate scrolling to momentum, after a touchscreen end touch event."
  (interactive)
  (mapc (lambda (axis)
          (setf (touchpad--momentum axis) (* (touchpad--momentum axis) touchpad-touchscreen-correction-factor)))
        touchpad--axes)
  (let ((time-diff (- (float-time) touchpad--prev-timestamp)))
    (when (<= time-diff (/ 5.0 touchpad-frame-rate))
      (touchpad--scroll-start-momentum)))
  (setq touchpad--touchscreen-prev-pos nil))

(defun touchpad--scroll-momentum ()
  "Scroll the window based on the momentum."
  (let ((delta (mapcar (lambda (axis)
                         (* (touchpad--momentum axis)
                            (/ 60.0 touchpad-frame-rate)
                            touchpad-scroll-speed))
                       touchpad--axes)))
    (touchpad--do-scroll delta touchpad--scroll-window)
    (if (>= (apply 'max (mapcar (lambda (axis)
                                  (abs (touchpad--momentum axis)))
                                touchpad--axes))
            1)
        (mapc (lambda (axis)
                (setf (touchpad--momentum axis)
                      (* (touchpad--momentum axis) (expt touchpad-momentum-decay (/ 60.0 touchpad-frame-rate)))))
              touchpad--axes)
      (touchpad--scroll-stop-momentum))))

(defvar touchpad-scroll-mode-map (make-sparse-keymap))
(mapc (lambda (key)
        (define-key touchpad-scroll-mode-map (vector key) 'touchpad-scroll-touchpad))
      '(wheel-up wheel-down wheel-left wheel-right))
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
