;;;;create my own macro for test
(defmacro with-gensym ((&rest name) &body body)
  `(let ,(loop for n in name collect `(,n (gensym)))
     ,@body))

(defun prime (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (prime n) return n))

(defmacro do-primes ((var start end) &body body)
  (with-gensym (end-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,end-name ,end))
	 ((> ,var ,end-name))
       ,@body)))
