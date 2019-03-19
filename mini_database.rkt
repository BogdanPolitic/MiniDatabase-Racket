#lang racket
(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces

(define create-table
  (λ (table columns-name)
    (cons (cons table '()) (foldr cons '() (map list columns-name)))))

(define get-name
  (λ (table)
    (car (car table))))

(define get-columns
  (λ (table)
      (map car (cdr table))))

(define get-tables
  (λ (db)
    (if (eq? db null)
        '()
        db)))

(define get-table
  (λ (db table-name)
    (car (filter (λ (table)
              (if (eq? (get-name table) table-name)
                  #t
                  #f)) db))))

(define add-table
  (λ (db table)
    (append db (list table))))

(define remove-table
  (λ (db table-name)
    (remove (get-table db table-name) db)))

(define db0
  (list (create-table "Studenți" '("Număr matricol" "Nume" "Prenume" "Grupă" "Medie")) (create-table "Cursuri" '("Anul" "Semestru" "Disciplină" "Număr credite" "Număr teme"))))

;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================

(define same-as
  (λ (n1 n2)
    (list? (member n1 (list n2)))))

(define insert1
  (λ (db table-name record)
    ((λ (len-field)
      (map (λ (col)
             (if (and (same-as (length (cdr col)) len-field) (not (same-as (car col) table-name)))
                 (append col (list NULL))
                 col)) (foldl (λ (pair acc)
                                (map (λ (column)
                                       (if (same-as (car column) (car pair))
                                           (append column (list (cdr pair)))
                                           column)) acc)) (get-table db table-name) record))) (- (length (car (cdr (get-table db table-name)))) 1))))

(define insert
  (λ (db table-name record)
    (map (λ (table)
           (if (same-as (get-name table) table-name)
               (insert1 db (get-name table) record)
               (get-table db (get-name table)))) db)))

;;(insert db "Studenți" '(("Număr matricol" . 127) ("Nume" . "Iliescu") ("Prenume" . "Moise") ("Grupă" . "329CZ") ("Medie" . "10")))
;;TESTEAZA ASTA!

(define st1
  (insert1 db0 "Studenți" '(("Număr matricol" . 123) ("Nume" . "Ionescu") ("Prenume" . "Gigel") ("Grupă" . "321CA") ("Medie" . 9.82))))
(define db1
  (list st1 (create-table "Cursuri" '("Anul" "Semestru" "Disciplină" "Număr credite" "Număr teme"))))
(define st2
  (insert1 db1 "Studenți" '(("Număr matricol" . 124) ("Nume" . "Popescu") ("Prenume" . "Maria") ("Grupă" . "321CB") ("Medie" . 9.91))))
(define db2
  (list st2 (create-table "Cursuri" '("Anul" "Semestru" "Disciplină" "Număr credite" "Număr teme"))))
(define st3
  (insert1 db2 "Studenți" '(("Număr matricol" . 125) ("Nume" . "Popa") ("Prenume" . "Ionel") ("Grupă" . "321CC") ("Medie" . 9.99))))
(define db3
  (list st3 (create-table "Cursuri" '("Anul" "Semestru" "Disciplină" "Număr credite" "Număr teme"))))
(define st4
  (insert1 db3 "Studenți" '(("Număr matricol" . 126) ("Nume" . "Georgescu") ("Prenume" . "Ioana") ("Grupă" . "321CD") ("Medie" . 9.87))))
(define db4
  (list st4 (create-table "Cursuri" '("Anul" "Semestru" "Disciplină" "Număr credite" "Număr teme"))))


(define cr1
  (insert1 db4 "Cursuri" '(("Anul" . "I") ("Semestru" . "I") ("Disciplină" . "Programarea calculatoarelor") ("Număr credite" . 5) ("Număr teme" . 2))))
(define db5
  (list (get-table db4 "Studenți") cr1))
(define cr2
  (insert1 db5 "Cursuri" '(("Anul" . "II") ("Semestru" . "II") ("Disciplină" . "Paradigme de programare") ("Număr credite" . 6) ("Număr teme" . 3))))
(define db6
  (list (get-table db5 "Studenți") cr2))
(define cr3
  (insert1 db6 "Cursuri" '(("Anul" . "III") ("Semestru" . "I") ("Disciplină" . "Algoritmi paraleli și distribuiți") ("Număr credite" . 5) ("Număr teme" . 3))))
(define db7
  (list (get-table db6 "Studenți") cr3))
(define cr4
  (insert1 db7 "Cursuri" '(("Anul" . "IV") ("Semestru" . "I") ("Disciplină" . "Inteligență artificială") ("Număr credite" . 6) ("Număr teme" . 3))))
(define db8
  (list (get-table db7 "Studenți") cr4))
(define cr5
  (insert1 db8 "Cursuri" '(("Anul" . "I") ("Semestru" . "II") ("Disciplină" . "Structuri de date") ("Număr credite" . 5) ("Număr teme" . 3))))
(define db9
  (list (get-table db8 "Studenți") cr5))
(define cr6
  (insert1 db9 "Cursuri" '(("Anul" . "III") ("Semestru" . "II") ("Disciplină" . "Baze de date") ("Număr credite" . 5) ("Număr teme" . 0))))
(define db10
  (list (get-table db9 "Studenți") cr6))

(define db db10)

;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================

(define simple-select
  (λ (db table-name columns)
    (map (lambda (l)
           (cdr l)) (map (lambda (l)
                           (car l)) (map (λ (column-i)
                                           (filter (λ (column)
                                                     (if (same-as column-i (car column))
                                                         #t
                                                         #f)) (cdr (get-table db table-name)))) columns)))))

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================
(define (fmin l min)
  (if (null? l)
      min
      (if (< (car l) min)
          (fmin (cdr l) (car l))
          (fmin (cdr l) min))))
  
(define min
  (λ (l)
    (if (null? l)
        null
        (fmin (cdr l) (car l)))))

(define (fmax l max)
  (if (null? l)
      max
      (if (> (car l) max)
          (fmax (cdr l) (car l))
          (fmax (cdr l) max))))
  
(define max
  (λ (l)
    (if (null? l)
        null
        (fmax (cdr l) (car l)))))

(define count
  (λ (l)
    (length l)))

(define sum
  (λ (l)
    (apply + l)))

(define avg
  (λ (l)
    (/ (sum l) (length l))))

(define function-list
  (list (cons 'min min) (cons 'max max) (cons 'count count) (cons 'sum sum) (cons 'avg avg)))

(define get-field-index ;; returneaza eroare daca campul dat ca parametru nu exista
  (λ (table field-name idx)
    (if (and (null? table) (same-as idx 0))
        -1
        (if (and (null? table) (> idx 0))
            -1
            (if (same-as (car (car (cdr table))) field-name)
                idx
                (get-field-index (cdr table) field-name (+ idx 1))))))) ;; - o apelez cu ultimul argument egal cu 0 !!!

(define first-appearance-index ;; o apelez cu ultimul argument egal cu 0 !!!
  (λ (l elem idx)
    (if (same-as elem (car l))
        idx
        (first-appearance-index (cdr l) elem (+ idx 1)))))

(define put-on-right-place
  (λ (order-model my-list elem-from-order elem-to-list)
    (if (same-as elem-from-order (car (reverse order-model)))
        (append my-list elem-to-list)
        (if (same-as elem-from-order (car order-model))
            (cons (car elem-to-list) my-list)
            (append (append (reverse (list-tail (reverse my-list) (- (first-appearance-index order-model elem-from-order 0) 1))) elem-to-list) (list-tail my-list (first-appearance-index order-model elem-from-order 0)))))))

(define table-sorted ;; nu returneaza si numele campurilor; tabela returnata este transpusa
  (λ (db table-name columns conditions)
    (letrec ((should-sort (filter (λ (info-pair)
                                    (if (pair? info-pair)
                                        (or (same-as (car info-pair) 'sort-asc) (same-as (car info-pair) 'sort-desc))
                                        #f)) columns)) ;; acest filtru returneaza (daca exista, else null) perechea din columns care contine cerinta sortarii asc sau desc (pair: (sorting-method . field))
             (table (get-table db table-name)))
      (if (null? should-sort)
          (cdr (apply map list (cdr table))) ;; in ambele cazuri returnez apply map list deoarece (either way!!) va fi trebui re-intoarsa lista
          (sort (cdr (apply map list (cdr table))) (λ (recording-x recording-y)
                                         (define sort-func
                                           (if (same-as (car (car should-sort)) 'sort-asc)
                                               (λ (a b)
                                                 (< a b))
                                               (λ (a b)
                                                 (> a b))))
                                         (sort-func (list-ref recording-x (get-field-index table (cdr (car should-sort)) 0)) (list-ref recording-y (get-field-index table (cdr (car should-sort)) 0)))))
          ))))

(define sorted-table
  (λ (db table-name columns conditions)
    (cons (list table-name) (apply map list (cons (car (apply map list (cdr (get-table db table-name)))) (table-sorted db table-name columns conditions))))))

(define table-matching-conditions ;; returneaza lista doar cu inregistrarile match-uite, dar in forma transpusa, fara numele campurilor
  (λ (db table-name columns conditions)
    (if (null? conditions)
        (cdr (apply map list (cdr (sorted-table db table-name columns conditions))))
        (foldl (λ (record-entry acc)
                 (if (same-as (apply + (map (λ (condition-format)
                                              (if ((car condition-format) (list-ref record-entry (get-field-index (get-table db table-name) (cadr condition-format) 0)) (car (reverse condition-format)))
                                                  0
                                                  1)) conditions)) 0)
                     (append acc (list record-entry))
                     acc)) '() (cdr (apply map list (cdr (sorted-table db table-name columns conditions)))))))) ;; [tabela cu campurile deja selectate] - argumentul

(define exact-fields-on-selection
  (λ (columns)
    (map (λ (cl)
           (if (pair? cl)
               (cdr cl)
               cl)) (filter (λ (column)
                              (if (and (pair? column) (or (same-as (car column) 'sort-asc) (same-as (car column) 'sort-desc)))
                                  #t
                                  (not (pair? column)))) columns))))

(define selected-table-without-info ;; returneaza lista selectata in forma normala, dar fara nume si fara numele campurilor
  (λ (db table-name columns conditions)
    (simple-select (map (λ (table)
                          (if (same-as (get-name table) table-name)
                              (cons (list table-name) (apply map list (cons (car (apply map list (cdr (get-table db table-name)))) (table-matching-conditions db (get-name table) columns conditions)))) ;; campuri concatenate, insa fara table-name!!
                              table)) db) table-name (filter (λ (col)
                                                               (not (pair? col))) (exact-fields-on-selection columns))))) ;; - face posibila aplicarea lui simple-select pe db-ul modificat (tabelei respective din db i-am aplicat table-matching-conditions)

(define selected-with-field-names
  (λ (db table-name columns conditions)
    (define selected-sorted (apply map list (cons (exact-fields-on-selection columns) (apply map list (selected-table-without-info db table-name columns conditions))))) ;; normal form, with field names
    (define all-fields-sorted (apply map list (cons (car (apply map list (cdr (get-table db table-name)))) (table-matching-conditions db table-name columns conditions))))
    (foldl (λ (info-pair acc)
             (if (pair? info-pair)
                 (if (not (or (same-as (car info-pair) 'sort-asc) (same-as (car info-pair) 'sort-desc)))
                     (put-on-right-place columns acc info-pair (list ((cdr (car (filter (λ (quote-func-pair)
                                                            (same-as (car info-pair) (car quote-func-pair))) function-list))) (cdr (car (filter (λ (column)
                                                                                                                                                  (same-as (car column) (cdr info-pair))) all-fields-sorted))))))
                     acc)
                 acc)) selected-sorted columns))) ;; [ selected-table-sorted-without-info] - argumentul

(define select
  (λ (db table-name columns conditions)
    (map (λ (field)
           (if (list? field)
               (cdr field)
               field)) (selected-with-field-names db table-name columns conditions))))

;;cand fac selectia, fac cu tot cu campuri, deoarece imi trebuie numele campului!!!!

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================
(define update
  (λ (db table-name values conditions)
    (foldl (λ (entry acc)
             (if (same-as (apply + (filter (λ (condition)
                                             (if ((car condition) (list-ref entry (get-field-index (get-table db table-name) (cadr condition) 0)) (car (reverse condition)))
                                                 0
                                                 1)) conditions)) 0)
                 (append acc (map (λ (value)
                                    (not (null? (filter (λ (val)
                                                          (if (same-as (cdr values) val)
                                                              (car values)
                                                              val)) values)))) (apply map list (append (apply map list (map (λ (elem)
                                                                                                                              (list elem)) entry)) (map (λ (elem)
                                                                                                                                                          (list elem)) (apply map list (cdr (get-table db table-name))))))))
                 acc)) '() (get-table db table-name))))

;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================
(define deleted-no-field-names ;; returneaza lista doar cu inregistrarile match-uite, dar in forma transpusa, fara numele campurilor
  (λ (db table-name conditions)
    (if (null? conditions)
        (cdr (apply map list (cdr (get-table db table-name))))
        (foldl (λ (record-entry acc)
                 (if (> (apply + (map (λ (condition-format)
                                              (if ((car condition-format) (list-ref record-entry (get-field-index (get-table db table-name) (cadr condition-format) 0)) (car (reverse condition-format)))
                                                  0
                                                  1)) conditions)) 0)
                     (append acc (list record-entry))
                     acc)) '() (cdr (apply map list (cdr (get-table db table-name)))))))) ;; [tabela cu campurile deja selectate] - argumentul

(define delete
  (λ (db table-name conditions)
    (define rest-of-table (cons (list table-name) (apply map list (cons (car (apply map list (cdr (get-table db table-name)))) (deleted-no-field-names db table-name conditions)))))
    (map (λ (table)
           (if (same-as (get-name table) table-name)
               rest-of-table
               table)) db)))

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================
(define natural-join
  (λ (db tables columns conditions)
    'your-code-here))

(define fsort-asc
  (λ (l res)
    (if (null? l)
        res
        (fsort-asc (remove (max l) l) (cons (max l) res)))))

(define sort-asc
  (λ (l)
    (fsort-asc l '())))

(define fsort-desc
  (λ (l res)
    (if (null? l)
        res
        (fsort-desc (remove (min l) l) (cons (min l) res)))))

(define sort-desc
  (λ (l)
    (fsort-desc l '())))
