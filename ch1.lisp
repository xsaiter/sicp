(defun our-len (lst)
  (if (null lst)
      0
      (+ (our-len (cdr lst)) 1)))

(defun our-len-i (lst)
  (let ((res 0))
    (dolist (x lst)
      (setf res (+ res 1)))
    res))

(defun our-cube (x) (* x x x))

(defun our-square (x) (* x x))


;;; 1.3
(defun our-sum (a b term next)
  (if (> a b)
      0
      (+ (funcall term a)
	 (our-sum (funcall next a) b term next))))

(defun our-self (x) x)

(defun our-inc (x) (+ x 1))

(defun our-sum-integers (a b)
  (our-sum a b #'our-self #'our-inc))

(defun our-sum-cubes ( a b)
  (our-sum a b #'our-cube #'our-inc))
