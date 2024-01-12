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
;;; //TODO: mettere T come type di default ai fields
(defun def-class (class-name parents &rest parts) 
        (cond 
            ((or (not (atom class-name))
                (null class-name) 
                (not (listp parents))) 
                (error (format nil "Error [def-class]: Class-name or Parents invalid.")))
            (t 
                (add-class-spec class-name 
                    (append 
                        (list class-name)
                        (list parents) 
                        parts
                    )
                ) class-name)
        )
)

;;; make: crea un'istanza di una classe.
(defun make (class-name &rest parts) 
    (cond 
    ;;controllo che il primo elemento della terza lista sia il simbolo 'fields
        ((not (equal (caaddr (class-spec class-name)) 'fields)) 
            (let (class-fields (cdaddr (class-spec class-name))) (instantiate class-name class-fields))) 
    ;; altrimenti se il primo elemento della terza lista è il simbolo 'fields
        (t (let ((class-fields (cdaddr (class-spec class-name)))) (instantiate class-name class-fields)))
    )
)

(defun instantiate (class-name class-fields)
    (let ((valid-parts (check-field-exists class-name class-fields)))
        (cond 
            ((not (is-class class-name)) (error "[instatiate] La classe non esiste"))                         
            (t (append (list 'oolinst) 
                    (list class-name 
                            (replace-fields valid-parts class-fields)))
            )
        )
    )
)

;;; replace-fields: sostituisce i ogni campo di class-fields con il
;;; valore associato a field-name nella lista fields, se presente.
(defun replace-fields (fields class-fields)
    (mapcar 
        (lambda (field)
            (if (member (first field) (mapcar #'first fields))
                (assoc (first field) fields)
                field))
                class-fields)
)

;;; get-class-field: estrae il valore dello slot-name specificato dalla
;;; classe desiderata. Se slot-name non è presente nella classe,
;;; viene cercato nei parents della classe.
;;; Se non è presente lo slot-name nella classe o nei parents, la
;;; funzione segnala un errore.
(defun get-class-field (class field-name) 
    (cond ((get-data (class-spec class) field-name)) 
        ;; Se la classe non ha lo fieldname cerca nei padri 
        ((get-parent-field (get-parents class) field-name))
        ((error 
            (format nil 
                "Error [get-class-field]: no method or field named ~a found." field-name))))
)

;;; check-field-exists: controlla se ogni field nella lista di fields passata
;;; come argomento sono presenti nella class specificata.
;;; Se i fields esistono viene restituita una cons contenente tutti i
;;; fields validi, altrimenti la funzione segnala un errore
(defun check-field-exists (class-name fields) 
    (cond ((null fields) nil) 
            ((not (null fields)) 
             (cons fields 
                  (cons (cadr fields) (check-field-exists class-name (cddr fields)))))
            (T (check-field-exists class-name (cddr fields)))
    )
)
;;; (def-class 'person nil '(fields () () ()) '(methods () ())))

;;; is-class: verifica se un simbolo è il nome di una classe.
(defun is-class (class-name) 
    (cond 
        ((null (class-spec class-name)) (error "[is-class] La classe non esiste"))
        (t t)
    )
)

;;; is-instance: restituisce T se l'oggetto passatogli è l'istanza
;;; di una classe.
(defun is-instance (value &optional (class-name T)) 
    (cond ((and (equal (first value) 'OOLINST) 
                (equal class-name 'T)) T) 
          ((equal (second value) class-name) T) 
          ;; Ereditarietà 
          ((member class-name (cadr (get-class-spec (cadr value)))) T)
          )
)

;;; field: estrae il valore di un campo da una classe.
;;; Se field-name non è presente nella classe dell'istanza
;;; viene segnalato un errore.
;;; //TODO: se il field cercato non è passato nella make oppure è un field di una classe padre NON LO TROVA
(defun field (instance field-name)
    ;; Se l'instanza non ha lo fieldname, vedi la sua classe 
        (cond ((not (null (get-data instance field-name))) (car (get-data instance field-name))) 
            ;; Se la classe non ha lo fieldname cerca nei padri 
            ((get-data (class-spec (cadr instance)) field-name))
            ((get-parent-field (get-parents (cadr instance)) field-name))
            ((error 
                (format nil 
                    "Error: no method or field named ~a found." field-name)))
            
        )
)

;;; field*: estrae il valore da una classe percorrendo una catena di attributi.
;;; Il risultato è il valore associato all'ultimo elemento di field-name
;;; nell'ultima istanza.
;;; Se uno degli elementi di field-name non esiste nella classe
;;; dell'istanza, viene segnalato un errore.
(defun field* (instance &rest field-name)
    (cond 
        ((null (is-instance (field instance (if (listp (car field-name)) 
                   (caar field-name) (car field-name))))) 
                       (error "Errore field* non è un'istanza"))
        ((eq (length field-name) 1) 
                 (field instance (if (listp (car field-name)) 
                                  (caar field-name) (car field-name))))
        (T (field* (field instance (if (listp (car field-name)) 
                                 (caar field-name) (car field-name))) 
                (cdr field-name))))
)

;;; get-class-data: estrae il valore dello field-name dall'istanza
;;; passati come parametri.
(defun get-data (instance field-name) 
    (cond 
        ;; Caso base 
        ((null instance) nil)
        ;; Se è un atom 
        ((atom (first instance)) (get-data (third instance) field-name))
        ;; Se è un attributo 
        ((and (symbolp (caar instance)) 
              (equal (intern (symbol-name (caar instance))) 
                     (intern (symbol-name field-name)))) 
         ;; Se è nil ma esistente 
         (if (null (cdar instance)) "undefined" (cdar instance))) 
        ;; Altrimenti 
        (T (get-data (list (cadr instance)) field-name)))
)


;;; get-parents: restituisce una lista che contiene tutte le classi
;;; parents della classe passata come argomento.
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

;;; get-parent-field: restituisce il valore del primo field-name presente
;;; nelle classi parents passate come lista. In pratica se uno field-name
;;; non è presente in uno dei parents, va a cercarlo ed eventualmente
;;; ereditarlo dalla prossima classe della lista parents.
(defun get-parent-field (parents field-name) 
    (cond ((null parents) nil) 
        ((null (get-data (class-spec (car parents)) field-name)) 
            (get-parent-field (cdr parents) field-name))
        ((get-data (class-spec (car parents)) field-name))
    )
)