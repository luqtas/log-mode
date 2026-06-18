;;; clock.el --- Time percentage tracker -*- lexical-binding: t; -*-

(defvar clock-birth-year 1996
  "The user's birth year, used to calculate current age.")

;;(defvar clock-waking-hour 12
;;  "Waking time in UTC hours (default 12:00 UTC = 21:00 Brazil).")
;;(defvar clock-sleep-hours 8
;;  "How many hours per night the user sleeps.")
;;(defun clock-day-percent ()
;;  "Return how much of the active day has passed as a percentage.
;;The active day is defined as (24 - `clock-sleep-hours') hours starting
;;from `clock-waking-hour' UTC."
;;  (let* ((now        (decode-time (current-time) t)) ; t = UTC
;;         (h          (nth 2 now))
;;         (m          (nth 1 now))
;;         (s          (nth 0 now))
;;         (active-day (* (- 24 clock-sleep-hours) 3600))
;;         (woke-secs  (* clock-waking-hour 3600))
;;         (now-secs   (+ (* h 3600) (* m 60) s))
;;         ;; seconds elapsed since waking (wraps around midnight)
;;         (elapsed    (mod (- now-secs woke-secs) 86400)))
;;    (* 100.0 (/ (float elapsed) active-day))))

(defun clock-day-percent ()
  "Return how much of the current UTC day has passed as a percentage."
  (let* ((now      (decode-time (current-time) t))
         (h        (nth 2 now))
         (m        (nth 1 now))
         (s        (nth 0 now))
         (elapsed  (+ (* h 3600) (* m 60) s)))
    (* 100.0 (/ (float elapsed) 86400))))

(defun clock-year-percent ()
  "Return how much of the current year has passed as a percentage."
  (let* ((now        (decode-time (current-time) t))
         (year       (nth 5 now))
         (jan1       (encode-time 0 0 0 1 1 year t))
         (elapsed    (float-time (time-subtract (current-time) jan1)))
         (leap        (if (and (= 0 (mod year 4))
                               (or (/= 0 (mod year 100))
                                   (= 0 (mod year 400))))
                          366.0 365.0))
         (year-secs  (* leap 86400.0)))
    (* 100.0 (/ elapsed year-secs))))

(defun clock-age ()
  "Return the user's current age based on `clock-birth-year'."
  (- (nth 5 (decode-time (current-time) t)) clock-birth-year))

(defun clock-status ()
  "Display day%, year%, and age in the minibuffer."
  (interactive)
  (let ((day   (clock-day-percent))
        (year  (clock-year-percent))
        (age   (clock-age)))
    (if (>= day 100.0)
        (message "zzz")
      ;;(message "day: %.3f%%   year: %.2f%%   age: %d"
      (message "day: %d%%   year: %d%%   age: %d"
               ;;day year age))))
               (truncate day) (truncate year) age))))

(provide 'clock)
;;; clock.el ends here
