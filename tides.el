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

(defun tides-predict (station-id &optional tz callback)
  "Retrieve today's tidal forecast for STATION-ID.

TZ is the timezone that the result should be interpreted in.

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
                (lambda (_status)
                  ;; FIXME it doesn't seem like there's a better way
                  ;; to do this presently.
                  (goto-char (1+ url-http-end-of-headers))

                  (let* ((zone (or tz calendar-time-zone))
                         (result (tides--parse-predictions-response zone)))

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
   "?date=today"
   "&station=" (format "%s" station-id)
   "&product=predictions"
   "&datum=MLLW"
   ;; while it's tempting to retrieve in GMT/UTC, the API actually
   ;; determines "today" via the provided time zone.
   "&time_zone=lst_ldt"
   "&interval=hilo"
   "&units=english"
   "&application=" tides--application-identifier
   "&format=json"))

(defun tides--parse-predictions-response (tz)
  "Interpret a NOAA tide prediction response in timezone TZ.

Intended to be used as a callback for `url-retrieve', this will
parse the data from the current buffer."
  (let* ((parsed (json-parse-buffer :object-type 'alist))
         (predictions (alist-get 'predictions parsed)))
    (seq-map (lambda (p) (tides--interpret-prediction p tz))
             predictions)))

(defun tides--interpret-prediction (prediction tz)
  "Interpret a single PREDICTION element in TZ."
  (list :time (tides--interpret-time (alist-get 't prediction) tz)
        :level (string-to-number (alist-get 'v prediction))
        :type (if (string= (alist-get 'type prediction)
                           "h")
                  'high 'low)))

(defun tides--interpret-time (time tz)
  "Parse a NOAA TIME string in timezone TZ.

TZ should be an appropriate value for the UTC offset parameter of
a date-time, i.e. a number of minutes offset from UTC.

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
          tz ; UTC offset
          )))

(provide 'tides)
;;; tides.el ends here
