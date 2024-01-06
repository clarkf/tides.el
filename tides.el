;;; tides.el --- Tidal forecast utilities -*- lexical-binding: t; -*-

;; Homepage: https://github.com/clarkf/tides.el
;; Version: 0.0.1-alpha
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm

;;; Commentary:

;; tides.el provides a simple wrapper around The National Oceanic and
;; Atmospheric Administration's Tides & Currents web APIs.
;;
;; Currently, tides.el supports only tidal predictions via
;; `tides-predict' which can be used to asynchronously retrieve
;; today's tidal forecast for a given station.
;;
;; See https://tidesandcurrents.noaa.gov/ for information about the
;; data and https://api.tidesandcurrents.noaa.gov/api/prod/ for
;; information about the API.

;;; Code:

(require 'calendar)

(defgroup tides nil
  "NOAA tidal forecasts."
  :group 'external
  :group 'communication)

(defcustom tides--application-identifier
  "tides.el"
  "The application identifier to send when making API requests.

See https://api.tidesandcurrents.noaa.gov/api/prod/#application."
  :group 'tides
  :type '(string))

(defconst tides--time-regex
  ;; "2024-01-05 00:38"
  (rx string-start
      (group (repeat 4 digit))
      "-"
      (group (repeat 2 digit))
      "-"
      (group (repeat 2 digit))
      " "
      (group (repeat 2 digit))
      ":"
      (group (repeat 2 digit))
      string-end)
  "Regular expression for parsing NOAA times strings.")

(defun tides-predict (station-id &optional callback)
  "Retrieve today's tidal forecast for STATION-ID.

CALLBACK if present, is a function that takes a single argument,
the prediction, and will be called after the prediction is
retrieved.

The prediction will be a list where each element is a time,
height and classification for today.  For example:

  (
    (:time (0 38 0 5 1 2024 nil -1 0) :level 0.576 :type low)
    (:time (0 9 7 5 1 2024 nil -1 0) :level 3.365 :type low)
    (:time (0 18 13 5 1 2024 nil -1 0) :level 0.88 :type low)
    (:time (0 24 19 5 1 2024 nil -1 0) :level 2.901 :type low)
  )

The :time property is a calendrical date-time indicating the time
of the prediction.  The :level property indicates the height in
feet of the tide at that time.  The :type property will be a
symbol of either high or low, indicating whether the prediction
is for a high- or low-tide."
  (interactive (list (read-from-minibuffer "Station ID: ")))

  (url-retrieve (tides--build-url station-id)
                (lambda (status)
                  (if-let (err (plist-get status :error))
                      (error "Error: %s" err))

                  ;; FIXME it doesn't seem like there's a better way
                  ;; to do this presently.
                  (goto-char (1+ url-http-end-of-headers))

                  (let ((result (tides--parse-predictions-response)))

                    ;; if executed interactively, display the prediction
                    (when (not (or executing-kbd-macro noninteractive))
                      (message
                       "Forecast: %s"
                       (seq-map (lambda (p)
                                  (format "%s tide of %fft at %s"
                                          (plist-get p :type)
                                          (plist-get p :level)
                                          (format-time-string
                                           "%X" (encode-time (plist-get p :time)))))
                                result)))

                    (when callback
                      (apply callback result))))
                nil t t))

(defun tides--build-url (station-id)
  "Build the API URL for a request to STATION-ID."
  (concat
   "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
   "?date_begin=" (tides--get-period-start)
   "&range=24"
   "&station=" (format "%s" station-id)
   "&product=predictions"
   "&datum=MLLW"
   "&time_zone=gmt"
   "&interval=hilo"
   "&units=english"
   "&application=" tides--application-identifier
   "&format=json"))

(defun tides--parse-predictions-response ()
  "Interpret a NOAA tide prediction response.

Intended to be used as a callback for `url-retrieve', this will
parse the data from the current buffer."
  (let* ((parsed (json-parse-buffer :object-type 'alist))
         (predictions (alist-get 'predictions parsed)))
    (seq-map (lambda (p) (tides--interpret-prediction p))
             predictions)))

(defun tides--interpret-prediction (prediction)
  "Interpret a single PREDICTION element."
  (list :time (tides--interpret-time (alist-get 't prediction))
        :level (string-to-number (alist-get 'v prediction))
        :type (if (string= (alist-get 'type prediction)
                           "h")
                  'high 'low)))

(defun tides--interpret-time (time)
  "Parse a NOAA TIME string.

Returns calendrical data in the format

  (SECONDS MINUTES HOUR DAY MONTH YEAR DOW DST UTCOFF)

See `time-convert' for information on time formats."

  (save-match-data
    (string-match
     tides--time-regex
     time)
    (list 0 ; seconds
          (string-to-number (match-string 5 time)) ; minutes
          (string-to-number (match-string 4 time)) ; hours
          (string-to-number (match-string 3 time)) ; day
          (string-to-number (match-string 2 time)) ; month
          (string-to-number (match-string 1 time)) ; year
          nil ; day of week
          -1 ; DST
          0 ; UTC offset
          )))

(defun tides--get-period-start (&optional now)
  "Get the period starting string for a date.

NOW must be a Lisp timestamp and not a calendrical date-time.  If
omitted, defaults to `current-time'."
  (let* ((midnight (decode-time now)))
    (setf (decoded-time-hour midnight) 0
          (decoded-time-minute midnight) 0
          (decoded-time-second midnight) 0)
    (format-time-string "%Y%m%d%H%M"
                        (encode-time midnight) t)))

(provide 'tides)
;;; tides.el ends here
