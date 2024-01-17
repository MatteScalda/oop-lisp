;;;FUNZIONI PRINCIPALI
(defparameter *classes-specs* (make-hash-table))

(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) class-spec))

(defun class-spec (name)
  (gethash name *classes-specs*))

(defun def-class (class-name parents &rest parts) 
  (cond 
   ((or (not (atom class-name))
        (null class-name) 
        (not (listp parents))
        (not (ignore-errors (are-classes parents)))
    ) 
    (error (format nil "[def-class]: Class-name or Parents invalid.")))
   (t  (and (add-class-spec class-name 
                      (append 
                       (list class-name)
                       (list parents) 
                       (list (append (list 'fields) 
                                (add-type (remove-duplicates 
                                           (unify-parts 
                                            (append (get-classes-parts parents) parts) 
                                            'fields) 
                                           :test #'(lambda (x y) 
                                                     (equal (car x) (car y)))))))
                       (list (append (list 'methods) 
                                    (remove-duplicates 
                                     (unify-parts 
                                      (append (get-classes-parts parents) parts)
                                      'methods) 
                                     :test #'(lambda (x y) 
                                               (equal (car x) (car y))))))
                       )
             )
             (let ((metodi (remove-duplicates (unify-parts 
                                    (append (get-classes-parts parents) parts)
                                    'methods) 
                                  :test #'(lambda (x y) 
                                            (equal (car x) (car y)))))) 
                  (create-methods metodi)
              )
        ) class-name)
   )
)

(defun create-methods (methods) 
  (cond ((null methods) nil) 
        (T (process-method (caar methods) (cdar methods)) 
            (create-methods (cdr methods)))
  )
)

;;((name "ciao") (age 21 integer))
(defun add-type (fields) 
  (mapcar 
  #'(lambda (x) 
      (cond 
        ((null (caddr x)) (append x (list 'T)))
        (T x)
      )
    ) fields)
)

(defun make (class-name &rest parts) 
  (cond 
   (t (let ((class-fields (cdaddr (class-spec class-name)))) 
        (instantiate class-name class-fields parts)
      )
   )
  )
)

(defun check-fields-type (red-fields class) 
  (cond 
   ((null red-fields) T)
   ((and (ignore-errors (not (subtypep class 't)) 
        (not (is-instance (cadar red-fields) class))))
    (and (subtypep (caddr (get-class-field class (caar red-fields))) 't) 
        (not (typep (cadar red-fields) (caddr (get-class-field class (caar red-fields))))))
    (error (format nil "[check-fields-type]: Field ~a is not of right type." 
                   (caar red-fields))))
   (T (check-fields-type (cdr red-fields) class))
  )
)

(defun instantiate (class-name class-fields parts)
  (let ((valid-parts (remove-duplicates 
                      (append (check-field-exists class-name class-fields) 
                              (split-in-pairs parts))
                      :test #'(lambda (x y) (equal (car x) (car y))))))
    (cond 
      ((not (is-class class-name)) (error "[instantiate] Class does not exist"))
      ((or 
        (null valid-parts)  (not (check-fields-type valid-parts class-name))
      ) (error (format nil "[instantiate] Fields not valid.")))
     (t (append (list 'oolinst) 
                (list class-name valid-parts)))
     )
   )
)

(defun split-in-pairs (lst)
  (if (null lst)
      nil
      (cons (subseq lst 0 2) (split-in-pairs (subseq lst 2)))))

(defun get-class-field (class field-name) 
  (let ((class-fields (cdr (caddr (class-spec class)))))
    (extract-field class-fields field-name)
  )
)
(defun extract-field (fields field-name)
  (cond ((null fields) nil)
        ((equal (caar fields) field-name) (car fields))
        (T (extract-field (cdr fields) field-name))
  )
)

(defun check-field-exists (class-name fields) 
  (cond ((null fields) nil) 
        ((member (car fields) (cdaddr (class-spec class-name))) 
         (cons (car fields) (check-field-exists class-name (cdr fields))))
        (t (check-field-exists class-name (cdr fields)))
  )
)

(defun is-class (class-name) 
  (cond 
   ((null (class-spec class-name)) (error "[is-class] Class does not exist"))
   (t t)
  )
)
(defun are-classes (classes-names)
  (cond
    ((null classes-names) t)
    ((not (is-class (car classes-names))) nil)
    (t (are-classes (cdr classes-names)))
  )
)
(defun is-instance (value &optional (class-name T)) 
  (cond ((not (listp value)) nil)
        ((and (equal (first value) 'OOLINST) 
              (equal class-name 'T)) T) 
        ((equal (second value) class-name) T) 
        ((member class-name (cadr (class-spec (cadr value)))) T)
        (t (is-child-of (cadr value) class-name))
  )
)

(defun is-child-of (class-child class-parent)
  (cond ((null class-child) nil)
        ((equal class-child class-parent) T)
        ((is-child-of (car (get-parents class-child)) class-parent) T)
  )
)

(defun field (instance field-name)
  (cond ((not (null (get-data instance field-name))) 
         (car (get-data instance field-name))) 
        ((get-data (class-spec (cadr instance)) field-name))
        (t (error 
          (format nil 
                  "[field] no method or field named ~a found." field-name)))
  )
)

(defun field* (instance &rest field-name)
  (cond 
   ((eq (length (if (listp (car field-name)) 
                        (car field-name) 
                        nil)) 1) 
    (field instance (if (listp (car field-name)) 
                        (caar field-name) 
                        (car field-name))))
   (T (field* (field instance (if (listp (car field-name)) 
                                  (caar field-name) 
                                  (car field-name))) 
              (if (listp (car field-name)) 
                         (cdar field-name) 
                         (cdr field-name))))
  )
)

(defun get-data (instance field-name) 
  (cond 
   ((null instance) nil)
   ((atom (car instance)) (get-data (caddr instance) field-name))
   ((and (symbolp (caar instance)) 
         (equal (intern (symbol-name (caar instance))) 
                (intern (symbol-name field-name))))
    (cdar instance))
   (T (get-data (cdr instance) field-name)))
)


(defun get-parents (class) 
  (cond 
   ((null (cadr (class-spec class))) nil) 
   ((remove-duplicates 
     (append 
      (append (get-parents (car (cadr (class-spec class)))) 
              (get-parents (cdr (cadr (class-spec class))))) 
      (cadr (class-spec class))) :from-end t))
  )
)

(defun get-classes-parts (class-names) 
  (cond ((null class-names) nil) 
        ((append (cdr (class-spec (car class-names))) 
                 (get-classes-parts (cdr class-names))))
  )
)

(defun unify-parts (separated-parts part-type)
  (cond ((null separated-parts) nil)
        (t (let ((first-element (car separated-parts))
                 (f-e-type (car (car separated-parts))))
             (cond ((equal f-e-type part-type) 
                    (append (cdr first-element) 
                            (unify-parts (cdr separated-parts) part-type)))
                   (t (unify-parts (cdr separated-parts) part-type)))
            )
        )
  )
)

(defun get-method (instance method-name)
  (let ((methods (cdar (get-methods (cadr instance)))))
    (let ((method-code (caddr (find method-name methods :key #'car))))
      (let ((method-args (append (list 'this) (cadr (find method-name methods :key #'car)))))
        (append (list 'lambda) (list method-args) (list method-code))
      )
    )
  )
)

(defun get-methods (class-name)
  (cond ((null class-name) nil)
        (t (cdddr (class-spec class-name))))
)

;;; process-method: genera il codice necessaria per creare un metodo.
(defun process-method (method-name)
    (setf (fdefinition method-name) 
            (lambda (instance &rest method-args)
                ;; Applica funzione dell'istanza this con i parametri sotto
                (let ((metodo (get-method instance method-name)))
                  (apply (eval metodo) (append (list instance) method-args))
                )
            ))
)

