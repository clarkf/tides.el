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

(ert-deftest test-build-url ()
  (let ((url (tides--build-url 1234)))
    (should (string-search "station=1234" url)))
  (let ((url (tides--build-url 4321)))
    (should (string-search "station=4321" url))))

(ert-deftest test-interpret-predictions-response ()
  (with-temp-buffer
    (insert-file-contents-literally "./tests/example.json")
    (let* ((result (tides--parse-predictions-response 0)))
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
