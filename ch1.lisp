(defun len (lst)
  (if (null lst)
      0
      (+ (len (cdr lst)) 1)))

(defun len-i (lst)
  (let ((res 0))
    (dolist (x lst)
      (setf res (+ res 1)))
    res))
