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
(require 'time-date)
(require 'url-util)

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

(defcustom tides-buffer-name
  "*tides.el predictions*"
  "Buffer name for presenting tides.el predictions."
  :group 'tides
  :type '(string))

(defcustom tides-station-id
  nil
  "The default station identifier for tides.el."
  :group 'tides
  :type '(choice integer (const nil)))

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

(defun tides-predict (station-id &optional begin-date end-date callback)
  "Retrieve today's tidal forecast for STATION-ID.

CALLBACK if present, is a function that takes a single argument,
the prediction, and will be called after the prediction is
retrieved.

The predictions will be bounded by BEGIN-DATE and END-DATE, which
are calendrical date-times.  If nil or absent, BEGIN-DATE will
default to midnight of the current day local time.  If nil or
absent, END-DATE will default to 24 hours after BEGIN-DATE.

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
  (interactive (list (read-from-minibuffer "Station ID: "
                                           (if tides-station-id
                                               (format "%s" tides-station-id)
                                             nil))))

  (unless begin-date
    (setq begin-date (tides-midnight-today)))

  (unless end-date
    (setq end-date (decoded-time-add begin-date
                                     (make-decoded-time :day 1))))

  (url-retrieve (tides--build-url station-id begin-date end-date)
                (lambda (status)
                  (if-let (err (plist-get status :error))
                      (error "Error: %s" err))

                  ;; FIXME it doesn't seem like there's a better way
                  ;; to do this presently.
                  (goto-char (1+ url-http-end-of-headers))

                  (let ((result (tides--parse-predictions-response)))

                    ;; if executed interactively, display the prediction
                    (when (not (or executing-kbd-macro noninteractive))
                      (tides--present-predictions result))

                    (when callback
                      (apply callback result))))
                nil))

(defun tides--build-url (station-id begin-date end-date)
  "Build the API URL for a request.

Predictions will for the station identified by the numeric
STATION-ID between BEGIN-DATE and END-DATE."
  (concat
   "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
   "?begin_date=" (url-hexify-string (tides--format-date begin-date)
                                     url-query-allowed-chars)
   "&end_date=" (url-hexify-string (tides--format-date end-date)
                                   url-query-allowed-chars)
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
        :type (if (string= (downcase (alist-get 'type prediction))
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

(defun tides--present-predictions (predictions)
  "Present the PREDICTIONS to the user in `tides-buffer-name'."
  (let ((buffer (get-buffer-create tides-buffer-name)))
    (with-current-buffer buffer
      ;; enable writing to the buffer
      (setq buffer-read-only nil)

      (erase-buffer)
      (insert "Forecast:\n")

      (dolist (p predictions)
        (let ((type (plist-get p :type))
              (level (plist-get p :level))
              (time (plist-get p :time)))
          (insert (format-time-string "%X" (encode-time time)))
          (insert (format " %s tide of %fft\n"
                          (propertize
                           (format "%s" type)
                           'face (if (eq type 'high) 'error
                                   'success))
                          level))))

      ;; ensure that we're in special mode and that it's read only.
      ;; TODO: probably derive a specific major mode?
      (unless (eq major-mode 'special-mode)
        (special-mode))
      (setq buffer-read-only t))

    (switch-to-buffer buffer)))

(defun tides-midnight-today (&optional now)
  "Convenience function for determining the midnight preceding NOW.

Useful for computing the date arguments to `tides-predict'.

Returns a calendrical date-time, see `decode-time' for details on
the format."
  (let* ((now (or now (current-time)))
         (midnight (decode-time now)))
    (setf (decoded-time-hour midnight) 0
          (decoded-time-minute midnight) 0
          (decoded-time-second midnight) 0)
    midnight))

(defun tides--format-date (date)
  "Format DATE according to the NOAA API expectations.

Note that seconds are discarded and that the result will be in UTC."
  (format-time-string "%Y%m%d %H:%M"
                      (encode-time date) t))

(provide 'tides)
;;; tides.el ends here
