;;FUNZIONI PRINCIPALI

;;; make-hash-table e gethash manipolano le hash tables in Common Lisp.
;;; La forma di class-spec è un dettaglio implementativo.
(defparameter *classes-specs* (make-hash-table))
(defun add-class-spec (name class-spec)
    (setf (gethash name *classes-specs*) class-spec))
(defun class-spec (name)
    (gethash name *classes-specs*))
    
;;; def-class: definisce la struttura di una classe e la
;;; memorizza in una locazione centralizzata (una variabile globale).
(defun def-class (class-name parents &rest parts) 
        (cond ((or (stringp class-name)
                (null class-name) 
                (not (listp parents))) 
            (error (format nil "Error [def-class]: Class-name or Parents invalid.")))
            (t (add-class-spec class-name (append (list parents) (list parts))) class-name)
        )
)

;;; make: crea un'istanza di una classe.
(defun make (class-name &rest parts) 
    (cond 
        ((not (is-class class-name))) (error (format nil "Error [make]: Class-name invalid."))
    )
    (append (list 'oolinst)
            (list class-name)
            (list parts)
    )
)

;;; is-class: verifica se un simbolo è il nome di una classe.
(defun is-class (class-name) 
    (if (or 
            (null (class-spec class-name)) 
            (not (stringp class-name))
        )
        nil
        t
    )
)

;;; is-instance: restituisce T se l'oggetto passatogli è l'istanza
;;; di una classe.
(defun is-instance (value &optional (class-name T)) 
    (cond ((and (equal (first value) 'OOLINST) 
                (equal class-name 'T)) T) 
          ((equal (second value) class-name) T) 
          ;; Ereditarietà 
          ;;((member class-name (cadr (get-class-spec (cadr value)))) T)
          )
)
;;; field: restituisce il valore di un campo di un'istanza.
(defun field (instance field-name)
    (cond 
        ((or 
            (not (is-instance instance))
            (null field-name))
            (error "Error [field]: Instance-name or Field-name invalid.")
        )
        (t
            (let 
                ((field (third instance)))
                (extract-field field field-name)
            )
        )
    )
)

;;; extract-field: estrae il valore di un campo //TODO: non funziona 
(defun extract-field (field field-name)
    (cond
        ((null field) (error "Error [extract-field]: Field invalid."))
        ((equal (first (first field)) field-name) (second (first field)))
        (t (extract-field (rest field) field-name))))

;;//TODO: implementare field*

