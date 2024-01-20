# OOΛ in Common Lisp

## Descrizione

Ai tempi di Simula e del primo Smalltalk, molto molto tempo prima di Python,
Ruby, Perl e SLDJ, i programmatori Lisp già producevano una pletora di
linguaggi object oriented. Il progetto consiste nella costruzione di
un’estensione “object oriented” di Common Lisp, chiamata OOΛ, e di
un’estensione “object oriented” di Prolog, chiamata OOΠ.

## Funzionalità Principali

-   `def_class`: definisce una classe con `parents` e `parts` (Fields/Methods)
-   `make`: crea un'istanza di una classe (anche con Field passati)
-   `is_class`: controlla se `class-name` è una classe creata
-   `is_instance`: controlla se `value` è istanza (di `class-name`)
-   `field`: ritorna il valore di un `field` in un'istanza
-   `field*`: ritorna il valore di un `field` seguendo una catena di attributi

## Installazione

Per utilizzare questo progetto, devi avere un interprete Lisp installato sul
tuo sistema.
Puoi scaricare LispWorks Personal da [qui](http://tinyurl.com/4wfc278w).

Dopo aver installato LispWorks, puoi aprire il file `oop.lisp` in LispWorks.

## Utilizzo

Per creare una nuova classe utilizza il predicato `def_class` come segue:

```
(def-class ’person
    '(human)
    ’(fields (name "Eve") (age 21 integer))
    '(methods (talk () (format T "Ciao"))))

```

Per creare un'istanza di una classe utilizza il predicato `make` come segue:

```
(defparameter eve (make ’person 'age 25))
```

Poi utilizza `field` e `field*` per accedere alle istanze
ed i loro campi, per esempio:

```
(field eve ’age)
```

Puoi utilizzare i metodi che hai passato ad una classe
seguendo la sintassi di Common Lisp:

```
(talk eve)
```

## Alcuni Test Effettuati

```
(def-class ’person nil
    ’(fields (name "Eve") (age 21 integer)))

(def-class ’student ’(person)
    ’(fields (name "Eva Lu Ator") (university "Berkeley" string))
    ’(methods
            (talk (&optional (out *standard-output*))
                (format out "My name is ~A~% My age is ~D~%"
                    (field this ’name) (field this ’age)))))

(def-class ’studente-bicocca ’(student)
    ’(methods (talk ()
        (format t "Mi chiamo ~A, e studio alla Bicocca~%"
            (field this ’name))))
    ’(fields (university "UNIMIB")))

(def-class 'prova nil
    '(fields (persona "" string))
    '(methods (talk ()
        (format out "My name is prova"))))

(def-class 'prova2 nil '(fields (prova "" prova)))

(defparameter eve (make ’person))

(defparameter s1 (make ’student ’name "Eduardo De Filippo" ’age 108))

(defparameter s2 (make ’student))

(defparameter p1 (make prova 'persona eve))

(defparameter p2 (make 'prova 'persona s1))

(defparameter p3 (make 'prova2 'prova p1))

(field eve ’age)

(field s2 ’name)

(field* p2 'prova 'persona)
```

## Autori e Informazioni

Questo progetto è stato fatto per l'esame di
Linguaggi di Programmazione dell'Università degli Studi di Milano-Bicocca.

Fatto da Mecenero Matteo [] e Scaldaferri Matteo [912001].
