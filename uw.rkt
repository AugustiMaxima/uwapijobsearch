#lang racket

(require "../common/uw-api.rkt")
(provide jobsposting filteredposting)

;;;;;;;;;;;;;;;;
;; INTERFACE: ;;
;;;;;;;;;;;;;;;;
;;(jobsposting) collects all available job postings on the uw api and prints it in a relatively formatted way
;;jobsposting : Void -> Void

;;(filteredposting lr) takes in a list of requirements, formatted in listed pairs like this: '(("type" "Paid") ("site" "writing-centre") ("id" 321)). The function will print out all the possible job postings that suits the requirements. If an empty list '() is sent in, it will return all job postings.

;;Warning;; Any filter options must be passed in in a listed pair form. If you want to look for all paid jobs, '("type" "Paid") wont work. It needs to be '(("type" "Paid"))

;;;;;;;;;;;;;;;;;;;;;
;; IMPLEMENTATION: ;;
;;;;;;;;;;;;;;;;;;;;;
 
 ;;(uw-api "/poi")

 ;;The actual function that finds jobs and collects relevant information. Not visible outside of module
 (define (jobsfinding)
    (let f([j (uw-api "/opportunities")])
      (cond
       [(empty? j) '()]
       [else (cons (list (list-ref (assoc "site" (car j)) 1)
                         (list-ref (assoc "id" (car j)) 1)
                         (list-ref (assoc "type" (car j)) 1)
                         (list-ref (assoc "title" (car j)) 1)
                         )
                   (f(rest j)))]
       )
     )
)
 
 ;;just processing what it finds from jobsfinding
 (define (jobsposting)
   (let f([l (jobsfinding)])
     (cond
      [(empty? l) (writeln "End of Postings")]
      [else (writeln(car l)) (f(rest l))]
      )
    )
   )

 ;;takes a filtered list and returns all possible jobs that fits the requirements
 (define (jobsfilter lr)
   (let f([j (uw-api "/opportunities")])
      (cond
       [(empty? j) '()]
       [(filter lr (car j)) (cons (list (list-ref (assoc "site" (car j)) 1)
                         (list-ref (assoc "id" (car j)) 1)
                         (list-ref (assoc "type" (car j)) 1)
                         (list-ref (assoc "title" (car j)) 1)
                         )
                   (f(rest j)))]
       [else (f(rest j))]
       )
     )
   )
 
 ;;takes in a list of filter/requirements and  a singular job listing, returns true if job listing meets all requirements
 (define (filter lr j)
   (let f([l lr])
     (cond
      [(empty? l) #t]
      [(f(rest l)) (member (car l) j)]
      [else #f]
      )
     )
   )
 
 ;;passes along the req list, formats and prints it
 (define (filteredposting lr)
   (let f([l (jobsfilter lr)])
     (cond
      [(empty? l) (writeln "End of Postings")]
      [else (writeln(car l)) (f(rest l))]
      )
    )
   )
