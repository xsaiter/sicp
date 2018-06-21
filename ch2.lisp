(defun var-p (x)
  (symbolp x))

(defun same-var-p (x1 x2)
  (and (var-p x1)
       (var-p x2)
       (equal x1 x2)))

;; add

(defun make-add (x1 x2)
  (list '+ x1 x2))

(defun add-p (x)
  (and (consp x)
       (equal (car x) '+)))

(defun add-elt-1 (x)
  (second x))

(defun add-elt-2 (x)
  (third x))

;; mul

(defun make-mul (x1 x2)
  (list '* x1 x2))

(defun mul-p (x)
  (and (consp x)
       (equal (car x) '*)))

(defun mul-elt-1 (x)
  (second x))

(defun mul-elt-2 (x)
  (third x))

;; derivation

(defun deriv (exp x)
  (cond ((numberp exp) 0)
	((var-p exp)
	 (if (same-var-p exp x) 1 0))
	((add-p exp)
	 (make-add (deriv (add-elt-1 exp) x)
		   (deriv (add-elt-2 exp) x)))
	((mul-p exp)
	 (make-mul (deriv (mul-elt-1 exp) x)
		   (deriv (mul-elt-2 exp) x)))))	
