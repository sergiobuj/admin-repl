;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package #:github-repl)

(defpackage :github-repl
  (:nicknames :ghrepl)
  (:use #:cl)
  (:export #:*repos*
           #:*teams*
           #:start-repl))

(defparameter +github-api+ "https://api.github.com")
(defparameter *repos* '())
(defparameter *teams* '())
(defparameter +org+ "ride")

(defparameter +options+ '(("Create New Team" new-team)
                         ("Add Repositories to Teams" add-repos-to-teams)))

(setf github:*username* "your-username")
(setf github:*password* "your-github-token") ;; Not your everyday password, https://github.com/settings/tokens

(defun start-repl (org)
  "This will load the list of repositories and teams for the given organization ORG.
  After loading the resources it will present the options supported and wait for
  user input."
  (loop for (title _) in +options+
        for i from 0 to (length +options+)
        do (format t "~%~d~3T~a" i title))
  (format t "~&Enter selection ([q/quit] to quit): ")
  (finish-output nil)
  (let* ((user-input (read-line))
         (numeric-input (parse-integer user-input :junk-allowed t)))
    (cond ((or (equal user-input "quit") (equal user-input "q")) nil)
        ((withinp numeric-input 0 (length +options+))
         (funcall (cadr (nth numeric-input +options+)) org)
         (start-repl org))
        (t (start-repl org)))))

(defun withinp (test down up)
  "Predicate function to test if a number TEST falls in a range defined by lower
  boundary DOWN (inclusive) and upper boundary UP"
  (cond ((not (numberp test)) nil)
        (t (and (>= test down) (< test up)))))

(defun new-team (org)
  "Create a new team on the organization ORG"
  '())

(defun add-repo-team-request (org team repo)
  "Single API request that adds a repository REPO to a TEAM in the organization ORG."
 ;; (github:api-command (format nil "") :method :put :parameters '(Content-Size 0)))
  (format t "~&PUT https://api.github.com/teams/~a/repos/~a/~a" team org repo)
  (format nil "PUT https://api.github.com/teams/~a/repos/~a/~a" team org repo))

(defun add-repos-to-teams (org)
  "Create a list with all combinations of teams and new repositories for access in
  organization ORG."
  (if (= 0 (length *repos*)) (load-repos org))
  (if (= 0 (length *teams*)) (load-teams org))
  (let* ((teams-selected (select-plist *teams* '()))
        (repos-selected (select-plist *repos* '()))
        (teams (fetch-ids *teams* teams-selected))
        (repos (fetch-ids *repos* repos-selected)))
    (mapcar (lambda (x) (add-repo-team-request org (nth 0 x) (nth 1 x)))
            (apply #'alexandria:map-product #'list (list teams repos)))))

(defun fetch-ids (collection selection)
  "Fetch the ID field of the elements of COLLECTION at positions from SELECTION."
  (mapcar (lambda (x) (getf (nth x collection) :ID)) selection))

(defun load-repos (org)
  "Load all repos for organization ORG"
  (setf *repos* (all-resources 'fetch-repos org 1))
  (length *repos*))

(defun load-teams (org)
  "Load all teams for organization ORG"
  (setf *teams* (all-resources 'fetch-teams org 1))
  (length *teams*))

(defun fetch-repos (org params)
  "API request to fetch repositories from an organization ORG adding PARAMS to the request."
  (github:api-command (format nil "/orgs/~A/repos" org) :method :get :parameters params))

(defun fetch-teams (org params)
  "API request to fetch teams from an organization ORG adding PARAMS to the request."
  (github:api-command (format nil "/orgs/~A/teams" org) :method :get :parameters params))

(defun all-resources (func org page)
  "Function to walk through all pages of a resource on an organization ORG.
  This function checks if the number of elements in the response is larger than 0
  and call the function FUNC again increasing the PAGE value.

  **NOTE**: Relies on resources being paginated in the querystring and that an invalid page
  index returns an empty response."
  (let ((resource-page (funcall func org `(:page ,(write-to-string page)))))
    (if (> (length resource-page) 0)
      (apply #'append (list resource-page (all-resources func org (incf page))))
      '())))

(defun maxnamelength (repos)
  ""
  (reduce #'max (mapcar (lambda (h) (length (getf h :name))) repos)))

(defun select-plist (plist selection)
  "Show a list of checkboxes to the user and return the indexes of the selection.
  This will run until the user enters 'q' or 'quit'"
  (let ((current-selection (set-plist-selection plist selection)))
    (format t "~&~{~2d [~:[ ~;*~]] ~a~^~45T ~2d [~:[ ~;*~]] ~a~^~90T ~2d [~:[ ~;*~]] ~a~%~}" current-selection)
    (format t "~&Enter selection ([c] to continue): ")
    (finish-output nil)
    (let ((user-input (read-line)))
      (if (or (equal user-input "c"))
        selection
        (let* ((user-selection (understand-selection user-input))
               (updated-selection (limit-selection selection user-selection (length plist))))
          (select-plist plist updated-selection))))))

(defun limit-selection (selection new-selection limit)
  "Create a new list with items from SELECTION and NEW-SELECTION and making sure
  that all items are in the range 0 to LIMIT"
  (let ((unselection (intersection selection new-selection))
        (new-selection (append selection new-selection)))
       (remove-if #'(lambda (x) (or (numberp (position x unselection)) (< x 0) (>= x limit))) new-selection)))

(defun split-input (line)
  "Take the user's input LINE and run a .split(' ')."
  (if (numberp (position #\Space line))
    (let ((token (subseq line 0 (position #\Space line))))
         (append (list token)
                 (split-input (subseq line (+ (length token) 1) (length line)))))
    (list line)))

(defun understand-selection (user-input)
  "Process the USER-INPUT from the selection function. The user may refer to a
  selection of items in different ways and this is where we make sense of it."
  (let ((tokens (split-input user-input)))
    (apply #'append
      (mapcar (lambda (x)
                (cond
                  ((numberp (position #\- x)) (expand-dash x))
                  ((numberp (parse-integer x :junk-allowed t)) (list (parse-integer x))))) tokens))))

(defun expand-dash (line)
  "Takes a string representing a range in the form of ##-## and returns a list
  that contains those numbers.

  Input:  4-8
  Output: '(4 5 6 7 8)"
  (let* ((_left (subseq line 0 (position #\- line)))
         (_right (subseq line (+ (length _left) 1) (length line)))
         (from (parse-integer _left :junk-allowed t))
         (to (parse-integer _right :junk-allowed t)))
    (if (and (numberp from) (numberp to))
      (loop for i from 0 to (- to from) collect (+ i from))
      '())))

(defun set-plist-selection (plist selection)
  "Get a list of the :name values from the PLIST. The list is determined by SELECTION"
  (apply #'append
   (loop
     for i from 0 below (length plist)
     collect (list i (numberp (position i selection)) (getf (nth i plist) :name)))))

;; (defun load-repos (org)
;;   (reset-repos)
;;   (setf *repos* (all-repos org 1))
;;   (length *repos*))

;; (defun load-teams (org)
;;   (setf *teams* (all-teams org 1))
;;   (length *teams*))

;; (defun authorized-request (url)
;;   (multiple-value-list (drakma:http-request url :basic-authorization *auth*)))

;; (defun repos (owner params)
;;   (let ((url (format nil "~a/orgs/~a/repos?~a" +github-api+ owner params)))
;;     (authorized-request url)))

;; (defun fetch-all-repos (org page)
;;   (let ((repos (parsed-repos org (format nil "page=~d" page))))
;;     (cond ((= 0 (length repos)) '())
;;           (t (apply #'append (list repos (fetch-all-repos org (incf page))))))))

;; (defun teams (org params)
;;   (let ((url (format nil "~a/orgs/~a/teams?~a" +github-api+ org params)))
;;     (authorized-request url)))

;; (defun all-teams (org page)
;;   (let ((resource-page (fetch-teams org `(:page ,(write-to-string page)))))
;;     (if (= (length resource-page) 0)
;;       '()
;;       (apply #'append (list resource-page (all-teams org (incf page)))))))
;;
;; (defun all-repos (org page)
;;   (let ((resource-page (fetch-repos org `(:page ,(write-to-string page)))))
;;     (if (= (length resource-page) 0)
;;       '()
;;       (apply #'append (list resource-page (all-repos org (incf page)))))))

;; (defun parsed-repos (org params)
;;   (yason:parse (flexi-streams:octets-to-string (car (repos org params)))))

;;(defun select-repos (repos selected)
;;  (let ((current-selection (set-plist-selection repos selected)))
;;    (format t "~:{~2d [~:[ ~;*~]] ~a~%~}" current-selection)
;;    (let ((selection (read-line)))
;;      (cond ((or (equal "quit" selection) (equal "q" selection)) selected)
;;            (t (let* ((parsed-input understand-selection selection)
;;                      (updated-selection (remove-if #'(lambda (x) (numberp (position x parsed-input))) selected)))
;;                 --code--))
;;            (t (select-repos repos (append selected (understand-selection selection))))))))

;;(defun select-repos1 (repos selected)
;;  (let ((current-selection (set-plist-selection repos selected)))
;;    (format t "~:{~2d [~:[ ~;*~]] ~a~%~}" current-selection)
;;    (let ((selection (read-line)))
;;      (cond ((or (equal "quit" selection) (equal "q" selection)) selected)
;;            (t (select-repos repos (append selected (understand-selection selection))))))))

;;(defun print-repos-selected (repos selection)
;;  (let ((hash-selection ())
;;        (named-repos (mapcar (lambda (h) (gethash "name" h)) repos))
;;        (indexed-repos (loop for i from 0 to (length repos) collect )))))

;;(defun select-repos (selected)
;;    (let ((selection (read-line)))
;;        (cond ((or (equal "quit" selection) (equal "q" selection)) selected)
;;          (t (progn (format t "selection: '~a and ~a'~%" selected selection)
;;                    (select-repos (append selected (list selection))))))))

;; (let ((objs (loop for i from 0 to 10 collect `(,i ,(nth (random 2) '(t nil)) a))))(format t "~:{~2d [~:[ ~;*~]] ~a~%~}" objs)(format t "~a" objs))

;;(format nil "~%Ride Repos~{~&|#[ ] ~a ~35T#[ ] ~a ~70T#[ ] ~a~105T|~}" (mapcar (lambda (h) (gethash "name" h)) github-repl:*repos*))

;;(defun all-ride-repos ()
;;  (loop until ()))

;;(defun ride-repos ()
;;  (let ((result (repos "ride")))
;;    (loop until)
;;    (yason:parse (flexi-streams:octets-to-string (car result)))
;;    ))

;;(defun hi (site)
;;  (drakma:http-request site))

;;(let ((result nil))
;;  (loop with page = 1 do (setf result (yason:parse (flexi-streams:octets-to-string (car (github-repl:repos "ride" (format nil "page=~d" page)))))) until (= 0 (length result))))

;;(defun all-ride-repos ()
;;  (let ((result nil))
;;    (apply #'append
;;           (loop with page = 1
;;                 do (setf result (parsed-repos (format nil "page=~d" page)))
;;                 (incf page)
;;                 until (= 0 (length result))
;;                 collect result))))
