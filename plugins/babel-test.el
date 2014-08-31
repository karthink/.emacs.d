;; Run all tests in this buffer with `elk-test-run-buffer'.
;; Or evaluate, then run individual tests with `elk-test-run':

(deftest "test-google"
  (assert-equal (let* ((fetcher 'babel-google-fetch)
		       (washer 'babel-google-wash))
		  (babel-work "network" "en" "de" fetcher washer))
		"Netzwerk"))

(deftest "test-babelfish"
  (assert-equal (let* ((fetcher 'babel-fish-fetch)
		       (washer 'babel-fish-wash))
		  (babel-work "network" "en" "de" fetcher washer))
		"Netz"))

(deftest "test-free"
  (assert-equal (let* ((fetcher 'babel-free-fetch)
		       (washer 'babel-free-wash))
		  (babel-work "network" "en" "de" fetcher washer)) "Netz"))

(deftest "test-apertium"
  (assert-equal (let* ((fetcher 'babel-apertium-fetch)
		       (washer 'babel-apertium-wash))
		  (babel-work "network" "en" "es" fetcher washer)) "Red"))

(deftest "test-google-umlaut"
  (assert-equal (let* ((fetcher 'babel-google-fetch)
		       (washer 'babel-google-wash))
		  (babel-work "Exercise" "en" "de" fetcher washer))
		"Übung"))

