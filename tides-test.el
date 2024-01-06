;;; tides-test.el --- Unit tests for tides.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; You can run the tests with
;;
;;   $ emacs -batch -l ert \
;;           -l tides.el \
;;           -l tides-tests.el \
;;           -f ert-run-tests-batch-and-exit
;;
;; Or, more succinctly with eldev:
;;   eldev test

;;; Code:

(require 'ert)
(require 'tides)
(require 'time-date)

(ert-deftest test-build-url ()
  (let ((url (tides--build-url 1234 (tides-midnight-today) (tides-midnight-today))))
    (should (string-search "station=1234" url)))

  (let ((url (tides--build-url 4321 (tides-midnight-today) (tides-midnight-today))))
    (should (string-search "station=4321" url)))

  (let* ((start (make-decoded-time :year 2024 :month 1 :day 6
                                   :hour 0 :minute 0 :second 0
                                   :zone -28800))
         (stop (decoded-time-add start (make-decoded-time :day 1)))
         (url (tides--build-url 1 start stop)))
    ;; midnight PT is 8am UTC
    (should (string-search "begin_date=20240106%2008:00&" url))
    (should (string-search "end_date=20240107%2008:00&" url))))

(ert-deftest test-tides-midnight-today ()
  ;; 01:02:03AM
  (let ((now (encode-time (make-decoded-time :year 2024 :month 1 :day 6
                                             :hour 1 :minute 2 :second 3
                                             :zone -28800))))
    ;; 00:00:00AM
    (should (time-equal-p (make-decoded-time :year 2024 :month 1 :day 6
                                             :hour 0 :minute 0 :second 0
                                             :zone -28800)
                          (tides-midnight-today now)))))

(ert-deftest test-interpret-predictions-response ()
  (with-temp-buffer
    (insert-file-contents-literally "./tests/example.json")
    (let* ((result (tides--parse-predictions-response)))
      (should (plistp result))

      (let ((one (nth 0 result)))
        (should (equal (plist-get one :time)
                       (parse-time-string "2024-01-05T00:38:00-00:00")))
        (should (= (plist-get one :level)
                   0.576))
        (should (eq (plist-get one :type)
                    'low))))))

(provide 'tides-test)
;;; tides-test.el ends here
