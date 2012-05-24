;;; test framework from 'practical common lisp'
(defvar *test-name* nil)

(defmacro with-gensyms ((&rest names) &body body)
  `(let (,@(loop for n in names collect `(,n (gensym))))
     ,@body))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro combine-result (&body forms)
  "Combine the results (as booleans) of 'forms' in order"
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  (format t "~:[fail~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check (&body forms)
  `(combine-result
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(deftest test-+ ()
  (check
   (= (+ 1 2) 3)
   (= (+ -1 -2) -3)
   (= (+ -1 3) 2)
   (= (+ 1 -3) -2)
   (= (+ 0.1 1) 1.1)))

(deftest test-/ ()
  (check
    (= (/ 2 1) 2)
    (= (/ 1 2) 1/2)
    (= (/ -1 2) -1/2)
    (= (/ 2 -1) -2)))

(deftest test-a ()
  (combine-result
    (test-+)
    (test-/)))