;;FUNZIONI PRINCIPALI

;;; make-hash-table e gethash manipolano le hash tables in Common Lisp.
;;; La forma di class-spec è un dettaglio implementativo.
(defparameter *defined-classes* (make-hash-table))

(defun add-class (class-name parents-parts) 
    (setf (gethash class-name *defined-classes*) parents-parts))

(defun get-class-info (class-name) 
    (gethash class-name *defined-classes*))

;;; def-class: definisce la struttura di una classe e la
;;; memorizza in una locazione centralizzata (una variabile globale).
(defun def-class (class-name parents (&rest parts)) (
        (cond ((or (not (stringp class-name)) 
                (equal class-name '()) 
                (null class-name) 
                (not (listp parents))) 
            (error (format nil "Error: Class-name or Parents invalid.")))
        )
        (add-class class-name (list parents parts))
    )
)

(defun make (class-name &rest parts) (
    (cond 
        ((not (is-class class-name)) (error (format nil "Error: Class-name invalid.")))
    )
    (append (list 'instance)
            (list class-name)
            (list parts))
    )
)

(defun is-class (class-name) (
    (if (or 
        ((null (get-class-info class-name))) 
        (not (stringp class-name))) 
            (nil) 
            (t)
    )
))

(defun is-instance (value &optional (class-name t)) (
    (cond(
        (
            (and   ((string= (first value) 'instance)) (equal class-name 't)) 
            (t)
        )
    ))
    (if (null (get-class-info class-name)) (nil) (t))
))
          