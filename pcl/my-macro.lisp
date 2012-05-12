;;;;create my own macro for test
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
	  ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
	     ,@body)))))

(defmacro with-gensym ((&rest name) &body body)
  `(let ,(loop for n in name collect `(,n (gensym)))
     ,@body))

(defun prime (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (prime n) return n))

(defmacro do-primes ((var start end) &body body)
  (once-only (start end)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
	 ((> ,var ,end))
       ,@body)))

(defmacro dp-with-newlines (number lines)
  (once-only (number lines)
    `(let ((n 0))
       (do-primes (p 0 ,number)
	 (when (zerop (mod (setf n (1+ n)) ,lines))
	   (format t "~%"))
	 (format t "~a ~3t" p)))))