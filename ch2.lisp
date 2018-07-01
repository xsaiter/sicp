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

;; unordered

(defun element-of-unordered-set? (x set)
  (cond
    ((null set) nil)
    ((= x (car set)) x)
    (t (element-of-unordered-set? x (cdr x)))))

    
(defun add-to-unordered-set (x set)
  (if (element-of-unordered-set? x set)
      set
      (cons x set)))


(defun intersection-unordered-set (set1 set2)
  (cond
    ((or (null set1) (null set2)) nil)
    ((element-of-unordered-set? (car set1) set2)
     (cons (car set1) (intersection-unordered-set (cdr set1) set2)))
    (t (intersection-unordered-set (cdr set1) set2))))

;; ordered

(defun element-of-ordered-set? (x set)
  (cond
    ((null set) nil)
    ((= x (car set)) set)
    ((< x (car set)) (element-of-ordered-set?  (cdr set)))
    (t nil)))


(defun add-to-ordered-set (x set)
  (cond
    ((null set)
     (cons x nil))    
    ((> x (car set))
     (cons (car set) (add-to-ordered-set x (cdr set))))
    ((< x (car set))
     (cons x set))
    (t set)))
      
(defun intersection-ordered-set (set1 set2)
  (if (or (null set1) (null set2))
      nil
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1
		     (intersection-ordered-set (cdr set1)
					       (cdr set2))))
	      ((< x1 x2)
	       (intersection-ordered-set (cdr set1) set2))
	      ((> x1 x2)
	       (intersection-ordered-set set1 (cdr set2)))))))
      
