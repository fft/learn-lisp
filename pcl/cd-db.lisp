(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a: ~10t~a~%~}~%" cd)))

(defun prompt-read(prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd 
    (prompt-read "title")
    (prompt-read "artist")
    (or (parse-integer (prompt-read "rating") :junk-allowed t) 0)
    (y-or-n-p "ripped [y/n]")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
    (if (not (y-or-n-p "Another? [y/n]")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
		   :direction :output
		   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
    collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun make-settor-expr (field value)
  `(setf (getf row ,field) ,value))

(defun make-settor-list (fields)
  (loop while fields
    collecting (make-settor-expr (pop fields) (pop fields))))

(defmacro in (clauses)
  `#'(lambda (row)
       (when (funcall selector-fn row)
	 ,@(make-settor-list clauses)) row))

(defmacro update (selector-fn &rest clauses)
  `(setf *db*
    (mapcar 
       #'(lambda (row)
	   (when (funcall ,selector-fn row)
	     ,@(make-settor-list clauses))
	   row) *db*)))