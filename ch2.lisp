;;;
;;; derivation
;;;

(defun var? (x)
  (symbolp x))

(defun same-vars? (x1 x2)
  (and (var-p x1)
       (var-p x2)
       (equal x1 x2)))

;; add

(defun make-add (x1 x2)
  (list '+ x1 x2))

(defun add? (x)
  (and (consp x)
       (equal (car x) '+)))

(defun add-elt-1 (x)
  (second x))

(defun add-elt-2 (x)
  (third x))

;; mul

(defun make-mul (x1 x2)
  (list '* x1 x2))

(defun mul? (x)
  (and (consp x)
       (equal (car x) '*)))

(defun mul-elt-1 (x)
  (second x))

(defun mul-elt-2 (x)
  (third x))

(defun deriv (exp x)
  (cond ((numberp exp) 0)
	((var? exp)
	 (if (same-vars? exp x) 1 0))
	((add? exp)
	 (make-add (deriv (add-elt-1 exp) x)
		   (deriv (add-elt-2 exp) x)))
	((mul? exp)
	 (make-mul (deriv (mul-elt-1 exp) x)
		   (deriv (mul-elt-2 exp) x)))))	


;;;
;;; set
;;;

(defun contains-set? (x set)
  (cond
    ((null set) nil)
    ((equal x (car set)) x)
    (t (contains-set? x (cdr x)))))
    

(defun add-to-set (x set)
  (if (contains-set? x set)
      set
      (cons x set)))

(defun intersection-set (set1 set2)
  (cond (or (null set1) (null set2) '())
	(contains-set? (car set1) set2)
	(cons (car set1)
	      (intersection-set (cdr set1) set2))
	(t (intersection-set (cdr set1) set2))))
