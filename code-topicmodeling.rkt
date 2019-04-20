#reader(lib"read.ss""wxme")WXME0108 ## 
#|
   This file uses the GRacket editor format.
   Open this file in DrRacket version 7.1 or later to read it.

   Most likely, it was created by saving a program in DrRacket,
   and it probably contains a program with non-text elements
   (such as images or comment boxes).

            http://racket-lang.org/
|#            
#lang racket
(require loudhum)

; Citation:
;   Assignment 8, CSC151, Grinnell College CS Department
;   http://www.cs.grinnell.edu/~hamidfah/courses/csc151spring2019/assignments/assignment08.html

;;; Procedure:
;;;   file->paragraphs
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Extract all of the paragraphs from a file.
;;; Produces:
;;;   paragraphs, a list of strings
(define file->paragraphs
  (lambda (fname)
    (regexp-split #px"\n\n+"
                  (file->string fname))))

;;; Procedure:
;;;   letter
;;; Parameters:
;;;   n, a non-negative integer
;;; Purpose:
;;;   Convert a number to a letter (such as for the name of a topic)
;;; Produces:
;;;   let, a string

(define letter
  (lambda (n)
    (string (integer->char (+ (char->integer #\A) n)))))

; PARAMETER:
; words, a list
; n, a number between 1-26

(define randomly-assign-topics
  (lambda (words n)
    (let* ([topics (make-hash)]
           [add (for-each (lambda (word)
                            (hash-set! topics word (letter (random n)))) words)])
      topics)))


(define trivial-word?
  (lambda (word)
    (and (not (equal? word "i"))
         (not (equal? word "mine"))
         (not (equal? word "you"))
         (not (equal? word "yours"))
         (not (equal? word "he"))
         (not (equal? word "she"))
         (not (equal? word "her"))
         (not (equal? word "hers"))
         (not (equal? word "they"))
         (not (equal? word "their"))
         (not (equal? word "them"))
         (not (equal? word "theirs"))
         (not (equal? word "the"))
         (not (equal? word "a"))
         (not (equal? word "an"))
         (not (equal? word "these"))
         (not (equal? word "this"))
         (not (equal? word "those"))
         (not (equal? word "and"))
         (not (equal? word "but"))
         (not (equal? word "or"))
         (not (equal? word "nor"))
         (not (equal? word "for"))
         (not (equal? word "so"))
         (not (equal? word "yet"))
         (not (equal? word "as"))
         (not (equal? word "both"))
         (not (equal? word "only"))
         (not (equal? word "also"))
         (not (equal? word "either"))
         (not (equal? word "neither"))
         (not (equal? word "whether"))
         (not (equal? word "after"))
         (not (equal? word "although"))
         (not (equal? word "because"))
         (not (equal? word "before"))
         (not (equal? word "though"))
         (not (equal? word "if"))
         (not (equal? word "in"))
         (not (equal? word "rather"))
         (not (equal? word "then"))
         (not (equal? word "than"))
         (not (equal? word "since"))
         (not (equal? word "unless"))
         (not (equal? word "until"))
         (not (equal? word "when"))
         (not (equal? word "whenever"))
         (not (equal? word "where"))
         (not (equal? word "whereas"))
         (not (equal? word "wherever"))
         (not (equal? word "while"))
         (not (equal? word "however"))
         (not (equal? word "therefore"))
         (not (equal? word "moreover"))
         (not (equal? word "nevertheless"))
         (not (equal? word "above"))
         (not (equal? word "across"))
         (not (equal? word "against"))
         (not (equal? word "along"))
         (not (equal? word "among"))
         (not (equal? word "at"))
         (not (equal? word "behind"))
         (not (equal? word "below"))
         (not (equal? word "beneath"))
         (not (equal? word "between"))
         (not (equal? word "beyond"))
         (not (equal? word "by"))
         (not (equal? word "down"))
         (not (equal? word "during"))
         (not (equal? word "except"))
         (not (equal? word "from"))
         (not (equal? word "into"))
         (not (equal? word "like"))
         (not (equal? word "near"))
         (not (equal? word "off"))
         (not (equal? word "out"))
         (not (equal? word "outside"))
         (not (equal? word "over"))
         (not (equal? word "past"))
         (not (equal? word "through"))
         (not (equal? word "to"))
         (not (equal? word "toward"))
         (not (equal? word "under"))
         (not (equal? word "unlike"))
         (not (equal? word "up"))
         (not (equal? word "upon"))
         (not (equal? word "with"))
         (not (equal? word "within"))
         (not (equal? word "without"))
         (not (equal? word "here"))
         (not (equal? word "are"))
         (not (equal? word "was"))
         (not (equal? word "of"))
         (not (equal? word "on"))
         (not (equal? word "that"))
         (not (equal? word "."))
         (not (equal? word ":"))
         (not (equal? word ","))
         (not (equal? word ";"))
         (not (equal? word "!"))
         (not (equal? word "?"))
         (not (equal? word "'"))
         (not (equal? word "‘"))
         (not (equal? word "’"))
         (not (equal? word "-"))
         )))

(define cleanup
  (lambda (str)
    (filter trivial-word?
            (string-split (string-downcase str) #px"[‘\\\n ,.;:?!-()]+"))))

(define remove-duplicates
  (lambda (list-of-words)
    (let ([sorted-lst (sort list-of-words string-ci<?)])
      (cond
        [(null? sorted-lst)
         sorted-lst]
        [(null? (cdr sorted-lst))
         sorted-lst]
        [(equal? (car sorted-lst) (cadr sorted-lst))
         (remove-duplicates (cdr sorted-lst))]
        [else
         (cons (car sorted-lst) (remove-duplicates (cdr sorted-lst)))]))))

(define combine-lists
  (lambda (texts)
    (reduce append texts)))

(define unique-words
  (lambda (texts)
    (remove-duplicates (combine-lists texts))))

(define text-probs
  (lambda (text topics)
    (let* ([result (make-hash)]
           [topic-lst (map (section hash-ref topics <>) text)])
      (letrec ([kernel
                (lambda (so-far remaining)
                  (cond [(null? remaining)
                         result]
                        [(hash-has-key? result (car remaining))
                         (let ([prev (hash-ref result (car remaining))])
                           (kernel (hash-set! result (car remaining) (+ (/ 1 (length topic-lst)) prev)) (cdr remaining)))]
                        [else (kernel (hash-set! result (car remaining) (/ 1 (length topic-lst))) (cdr remaining))]
                        ))])
        (kernel result topic-lst)
        ))))

(define word-appearances
  (lambda (word texts)
    (map
     (lambda (lst) (tally-value lst word))
     texts)))

(define word-percentages
  (lambda (word texts)
    (map
     (lambda (each-apperance)
       (/ each-apperance (tally-value (combine-lists texts) word)))
     (word-appearances word texts))))

(define word-probs
  (lambda (word-percents list-of-text-probs)
    (let ([len (length word-percents)])
      (let kernel ([pos 0])
        (if (>= pos len)
            null
            (cons
             (cons (letter pos)
                   (let kernel! ([lst1 word-percents]
                                 [lst2 list-of-text-probs])
                     (if (or (null? lst1) (null? lst2))
                         0
                         (+ (* (car lst1)
                               (if (hash-has-key? (car lst2) (letter pos))
                                   (hash-ref (car lst2) (letter pos))
                                   0))
                            (kernel! (cdr lst1) (cdr lst2))))))
             (kernel (+ pos 1))))
        ))))

;;; Procedure:
;;;   biased-select
;;; Parameters:
;;;   lst, a non-empty list of value/probability lists
;;; Purpose:
;;;   Select one of the elements in the list, choosing
;;;   the element according to probability.  (This is
;;;   called "biased selection" in the literature.)
;;; Produces:
;;;   value, a value
;;; Preconditions:
;;;   * Each element of lst has the form (val prob).
;;;   * Each probability is a real number.
;;;     That is (all (o real? cadr) lst)
;;;   * Each probability is between 0 and 1, inclusive.
;;;   * The sum of all the probabilities is 1.
;;;     That is, (reduce + (map cadr lst)) = 1.
;;; Postconditions:
;;;   * value is one of the values in the list.  That is
;;;     (member? value (map car lst)).
;;;   * It is difficult to predict which value we get.
;;;   * Suppose the list is of the form ((val1 prob1)
;;;     (val2 prob2) ... (valn probn)).  Over a long
;;;     series of calls, we'll see val1 about prob1
;;;     of the time, val2 about prob2 of the time, and so
;;;     on and so forth.

(define biased-select
  (lambda (lst)
    (let kernel ([r (random)]
                 [remaining lst])
      (let* ([entry (car remaining)]
             [value (car entry)]
             [prob (cdr entry)])
        (cond
          [(null? (cdr remaining))
           value]
          [(< r prob)
           value]
          [else
           (kernel (- r prob)
                   (cdr remaining))])))))

(define select-topic
  (lambda (lst1 lst2)
    (biased-select (word-probs lst1 lst2))))


(define update-probs!
  (lambda (texts probs word oldtopic newtopic)
    (if (hash-has-key? probs oldtopic)
        (hash-set! probs oldtopic (- (hash-ref probs oldtopic) (/ (tally-value texts word) (length texts))))
        (hash-set! probs oldtopic (/ (tally-value texts word) (length texts))))
    (if (hash-has-key? probs newtopic)
        (hash-set! probs newtopic (+ (hash-ref probs newtopic) (/ (tally-value texts word) (length texts))))
        (hash-set! probs newtopic (/ (tally-value texts word) (length texts)))
        )))

;Parameter
; topics: a hash table randomly assigned words to topic
; texts: list of texts (text = list of words)
; list-of-probs: a hash table of a topic and its percentage in each text

(define improve-model!
  (lambda (topics texts list-of-probs)
    (let ([chosen-word (list-ref (combine-lists texts) (random (length (combine-lists texts))))]) ; randomly selects a word
      (let kernel! ([pos 0])
        (when (< pos (length list-of-probs))
          (update-probs!
           (list-ref texts pos)
           (list-ref list-of-probs pos)
           chosen-word
           (hash-ref topics chosen-word)
           (select-topic (word-percentages chosen-word texts) list-of-probs))
          (kernel! (+ pos 1)))))))

(define display-topics-helper
  (lambda (lst)
    (display (string-append "Topic " (car lst) ": ")) (display (cdr lst)) (newline)))  
    
(define display-topics
  (lambda (topics)
    (for-each display-topics-helper (sort (hash->list (invert-topics topics)) sort-lst?))))

(define sort-lst?
  (lambda (lsta lstb)
    (string-ci<? (car lsta) (car lstb))))
 
(define invert-topics
  (lambda (hash)
    (let* ([inverse (make-hash)]
           [hash! (lambda (pair)
                    (if (hash-has-key? inverse (cdr pair))
                        (let ([lst (hash-ref inverse (cdr pair))])
                          (hash-set! inverse (cdr pair) (cons (car pair) lst)))
                        (hash-set! inverse (cdr pair) (list (car pair))))
                    )]
           [lst (hash->list hash)])      
      (letrec ([kernel
                (lambda (so-far remaining)
                  (if (null? remaining)
                      inverse
                      (kernel (hash! (car remaining)) (cdr remaining))
                      ))])
        (kernel (list) lst))
      )))

(define topic-model
  (lambda (list-of-strings num-topics num-iterations)
    (let* ([texts (map cleanup list-of-strings)]
           [topics (randomly-assign-topics (unique-words texts) num-topics)]
           [probs (map (section text-probs <> topics) texts)])
      (letrec ([kernel (lambda (a count)
                         (if (= (+ 1 count) num-iterations)
                             (improve-model! topics texts probs)
                             (kernel (improve-model! topics texts probs) (+ count 1))))])
        (kernel 0 0))
      topics)))
