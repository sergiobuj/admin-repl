;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(defpackage :github-repl-test
  (:use :cl :fiveam))

(in-package :github-repl-test)

(def-suite :github-repl)
(in-suite :github-repl)

(test create-add-team-url
  (is (> 1 0)))

;;
;;(test get-google
;;  (let ((drakma:*header-stream* *standard-output*))
;;    (multiple-value-bind (body-or-stream status-code)
;;        (drakma:http-request "http://google.com/")
;;      (is (> (length body-or-stream) 0))
;;      (is (= 200 status-code)))))
;;
;;(test get-google-ssl
;;  (let ((drakma:*header-stream* *standard-output*))
;;    (multiple-value-bind (body-or-stream status-code)
;;        (drakma:http-request "https://google.com/")
;;      (is (> (length body-or-stream) 0))
;;      (is (= 200 status-code)))))
;;
;;(test post-google
;;  (let ((drakma:*header-stream* *standard-output*))
;;    (multiple-value-bind (body-or-stream status-code headers uri stream must-close reason-phrase)
;;        (drakma:http-request "http://google.com/" :method :post :parameters '(("a" . "b")))
;;      (declare (ignore headers uri stream must-close))
;;      (is (> (length body-or-stream) 0))
;;      (is (= 405 status-code))
;;      (is (string= "Method Not Allowed" reason-phrase)))))
;;
;;(test post-google-ssl
;;  (let ((drakma:*header-stream* *standard-output*))
;;    (multiple-value-bind (body-or-stream status-code headers uri stream must-close reason-phrase)
;;        (drakma:http-request "https://google.com/" :method :post :parameters '(("a" . "b")))
;;      (declare (ignore headers uri stream must-close))
;;      (is (> (length body-or-stream) 0))
;;      (is (= 405 status-code))
;;      (is (string= "Method Not Allowed" reason-phrase)))))
;;
;;
;;(test gzip-content
;;  (let ((drakma:*header-stream* *standard-output*)
;;        (drakma:*text-content-types* (cons '(nil . "json") drakma:*text-content-types*)))
;;    (multiple-value-bind (body-or-stream status-code)
;;        (drakma:http-request "http://httpbin.org/gzip" :decode-content t)
;;      (is (= 200 status-code))
;;      (is (typep body-or-stream 'string))
;;      (is (search "\"gzipped\": true" body-or-stream)))))
;;
;;(test deflate-content
;;  (let ((drakma:*header-stream* *standard-output*)
;;        (drakma:*text-content-types* (cons '(nil . "json") drakma:*text-content-types*)))
;;    (multiple-value-bind (body-or-stream status-code)
;;        (drakma:http-request "http://httpbin.org/deflate" :decode-content t)
;;      (is (= 200 status-code))
;;      (is (typep body-or-stream 'string))
;;      (is (search "\"deflated\": true" body-or-stream)))))
;;
;;(test gzip-content-undecoded
;;      (let ((drakma:*header-stream* *standard-output*))
;;        (multiple-value-bind (body-or-stream status-code)
;;            (drakma:http-ggrequest "http://httpbin.org/gzip")
;;          (is (= 200 status-code))
;;          (is (typep body-or-stream '(vector flexi-streams:octet)))
;;          (is (> (length body-or-stream) 0))
;;          (is (equalp #(#x1f #x8b)
;;                      (subseq body-or-stream 0 2))))))
;;
;;(test deflate-content-undecoded
;;      (let ((drakma:*header-stream* *standard-output*))
;;        (multiple-value-bind (body-or-stream status-code)
;;            (drakma:http-request "http://httpbin.org/deflate")
;;          (is (= 200 status-code))
;;          (is (typep body-or-stream '(vector flexi-streams:octet)))
;;          (is (> (length body-or-stream) 0))
;;          (is (equalp #x78 (aref body-or-stream 0))))))
;;
;;(test stream
;;  (multiple-value-bind (stream status-code)
;;      (drakma:http-request "http://google.com/" :want-stream t)
;;    (is (streamp stream))
;;    (is (= 200 status-code))
;;    (is (subtypep (stream-element-type stream) 'character))
;;    (let ((buffer (make-string 1)))
;;      (read-sequence buffer stream))))
;;
;;(test force-binary
;;  (multiple-value-bind (stream status-code)
;;      (drakma:http-request "http://google.com/" :want-stream t :force-binary t)
;;    (is (streamp stream))
;;    (is (= 200 status-code))
;;    (is (subtypep (stream-element-type stream) 'flexi-streams:octet))
;;    (let ((buffer (make-array 1 :element-type 'flexi-streams:octet)))
;;      (read-sequence buffer stream))))
;;
;;
