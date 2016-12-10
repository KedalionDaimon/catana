; THIS VERSION DOES NO FRAGSET

; TURN INTO LISP AND MEMOIZE WITH HASH TABLES

(define (proto-containslist mainlist sublist matchcount)
  (if (null? mainlist) matchcount
    (if (equal? sublist (takefirst mainlist (length sublist)))
	  (proto-containslist (cdr mainlist) sublist (+ 1 matchcount))
	  (proto-containslist (cdr mainlist) sublist matchcount))))
	  
(define (containslist mainlist sublist) (cons (proto-containslist mainlist sublist 0) sublist))
; sample call:
; (containslist '(a b c d e f g c d e h) '(c d e))
; --> (2 c d e)
; (containslist '(a b c d e f g) '(c d x))
; --> (0 c d x)

; ----------------------------------------------------------

(define (proto-takefirst fromwhere howmany resultlist)
  (if (or (null? fromwhere) (zero? howmany)) (reverse resultlist)
    (proto-takefirst (cdr fromwhere) (- howmany 1) (cons (car fromwhere) resultlist))))

(define (takefirst fromwhere howmany) (proto-takefirst fromwhere howmany '()))

(define (takelast fromwhere howmany) (reverse (takefirst (reverse fromwhere) howmany)))

(define (takeafter fromwhere howmany)
  (if (or (null? fromwhere) (zero? howmany)) fromwhere
    (takeafter (cdr fromwhere) (- howmany 1))))
; (takeafter '(a b c d e f) 2) --> (c d e f)

(define (proto-chainseg longlist fragsize fraglist)
  (let ((takenfrag (takefirst longlist fragsize)))
    (if (> fragsize (length takenfrag)) (reverse fraglist)
      (proto-chainseg (cdr longlist) fragsize (cons takenfrag fraglist)))))

(define (chainseg longlist fragsize) (proto-chainseg longlist fragsize '()))
; sample call:
; (chainseg '(a b c d e f) 4) --> ((a b c d) (b c d e) (c d e f))

; Split a long list into fragments of all allowed chain lengths:
(define (proto-allchains longlist fragsize maxfrag resultfrags)
  (if (> fragsize maxfrag) resultfrags
    (proto-allchains longlist (+ 1 fragsize) maxfrag (append (chainseg longlist fragsize) resultfrags))))

; THUNK - minimum and maximum chain size are globally defined:
(define (allchains longlist) (proto-allchains longlist minchain maxchain '()))
; (allchains '(a b c d)) --> ((a b c d) (a b c) (b c d) (a b) (b c) (c d) (a) (b) (c) (d))

(define (list-to-symbol somelist)
  (map string->symbol
    (map (lambda (w) (apply string-append w))
      (map (lambda (x) (map (lambda (y) (string-append y "-")) x))
        (map (lambda (z) (map symbol->string z)) somelist)))))
; (list-to-symbol '((A) (BB C) (DD EE FFF) (G H)))
; --> (A- BB-C- DD-EE-FFF- G-H-)

(define (symbol-fragset somelist)
  (list-to-symbol (allchains somelist)))
; (symbol-fragset '(A B C D))
; --> (A-B-C-D- A-B-C- B-C-D- A-B- B-C- C-D- A- B- C- D-)
; in other words: here I have a quasi-normal fragmentation set, but with symbols.

; WITH THRESHOLD:

; (define (comparesets set-a set-b)
;   (/
;     (expt
;       ; kill switch: if less than half have been matched, the strength is 0:
;       (let ((hits (apply + (map (lambda (x) (if (member x set-b) 1 0)) set-a))))
;         (if (or (< hits (/ (length set-a) 2))
;             (< hits (/ (length set-b) 2)))
;           0
;           hits))
;       2)
; ; 1))
; ; activate the above instead of the line below to ignore match length, if desired
;     (+ 1 (abs (- (length set-a) (length set-b))))))
; ; (comparesets '(a b c d e f) '(a b c y)) --> 3
; ; (comparesets '(a b c d) '(a x))         --> 0
; ; (comparesets '(a b c d) '(a b c d))     --> 16

; WITHOUT THRESHOLD - ASSUMING IMPLIED ANALOGIES:

(define (comparesets set-a set-b)
  (/
    (expt
      (apply + (map (lambda (x) (if (member x set-b) 1 0)) set-a))
      2)
; 1))
; activate the above instead of the line below to ignore match length, if desired
    (+ 1 (abs (- (length set-a) (length set-b))))))
; (comparesets '(a b c d) '(a x b y)) --> 4
; (comparesets '(a b c d) '(a x))     --> 1/3
; (comparesets '(a b c d) '(a b c d)) --> 16

(define (similarity list-a list-b)
;  (comparesets (symbol-fragset list-a) (symbol-fragset list-b)))
  (comparesets list-a list-b))
; (similarity '(a b c d) '(a b c d))  --> 100
; (similarity '(a b x d) '(a b c y))  --> 9

; ----------------------------------------------------------

(define (proto-find-best-match segment seglist bestmatch bestvalue)
  (if (null? seglist) (list bestmatch bestvalue) ;  match-position) - cutting out the match-position (
    (let ((newvalue (similarity segment (car seglist))))
      (if (< bestvalue newvalue)
        (proto-find-best-match segment (cdr seglist) (car seglist) newvalue)
        (proto-find-best-match segment (cdr seglist) bestmatch bestvalue)))))

(define (find-best-match segment seglist)
  (if (null? seglist) '()
    (proto-find-best-match segment (cdr seglist) (car seglist) (similarity segment (car seglist)))))
; (find-best-match '(a b c) '((d e f a) (r b x) (c d b a) (l m n c o) (x y z)))
; --> ((c d b a) 9/5)

; IF THE SEGMENTS CAN HAVE A FLEXIBLE SIZE, TOO:

(define (proto-cutouts segment pattern seen-so-far seglen)
  (if (null? segment) '()
    (if (null? pattern) '()
      (if (equal? segment (takefirst pattern seglen))
        (list (reverse seen-so-far) (takeafter pattern seglen))
        (proto-cutouts segment (cdr pattern) (cons (car pattern) seen-so-far) seglen)))))
; (proto-cutouts '(a b c) '(r t z x a b c m p r) '() 3) --> ((r t z x) (m p r))

(define (cutouts pattern patlen found-match)
  (proto-cutouts (car found-match) pattern '() (length (car found-match))))
; patlen not needed, kept only for compatibility

; (cutouts '(d r c d b a) 6 '((c d b a) 9/5 2))    
; --> ((d r) ())
; (cutouts '(c d b a l x) 6 '((c d b a) 9/5 0))
; --> (() (l x))
; (cutouts '(d r c d b a l x) 8 '((c d b a) 9/5 2))
; --> ((d r) (l x))

(define (find-similar-section segment pattern)
  (let ((seglen (length segment))
        (patlen (length pattern)))
    (if (< patlen seglen)
      (list pattern (comparesets segment pattern) 0 '() '())
      ; provisorial alternative to above was simply NIL - '().
      (let ((best-match (find-best-match segment (proto-allchains pattern minchain maxchain '()))))
        (append best-match (cutouts pattern patlen best-match))))))
; (find-similar-section '(a b c) '(r t z x a f b c m p r))
; --> ((f b c) 9 (r t z x a) (m p r))

; (find-similar-section '(b c d e) '(r t z x a b k c d e m p r))  
; --> ((k c d e) 36 (r t z x a b) (m p r))
; --> ((b k c d e) 49 (r t z x a) (m p r))
; -- but modified "comparesets" to ignore the pattern length, dividing by 1

; (find-similar-section '(b c d e) '(r t z))
; --> ((r t z) 0 () ())
; (find-similar-section '(b c d e) '(b f c))
; --> ((b f c) 4 () ())

; ----------------------------------------------------------

(define (proto-best-segment seg-list pattern segment best-match)
  (if (null? seg-list)
    (list
      (cadr (reverse best-match))
      (cons '() (cons segment (reverse (cddr (reverse best-match)))))
      (car (reverse best-match)))
    (let ((fss (find-similar-section (car seg-list) pattern)))
    (if (> (cadr fss) (cadr best-match))
      (proto-best-segment (cdr seg-list) pattern (car seg-list) fss)
      (proto-best-segment (cdr seg-list) pattern segment best-match)))))

; only deliver a match if the strength is not zero
(define (best-segment seg-list pattern)
  (let ((result (proto-best-segment (cdr seg-list) pattern (car seg-list)
                  (find-similar-section (car seg-list) pattern))))
    (if (zero? (car (reverse (cadr result))))
      (list pattern)
      result)))

; (best-segment '((q r s t) (a b c) (e f) (h i j)) '(r t z x a b k c d e m p r))
; --> ((r t z) (() (a b c) (x a b) 9) (k c d e m p r))

; (best-segment '((q) (j)) '(r t z x a b k c d e m p r))
; --> ((r t z x a b k c d e m p r))

(define (mapped-find seg-list pattern)
  (if (null? pattern)
    ; then do not attempt match, simply deliver the pattern:
    pattern
    (if (null? (car pattern))
      ; then do not attempt match, simply deliver the pattern, protect against apply append:
      (list pattern)
      (cons '() (best-segment seg-list pattern)))))

(define (proto-remove-nil somelist resultlist)
  (if (null? somelist)
    (reverse resultlist)
    (if (null? (car somelist))
      (proto-remove-nil (cdr somelist) resultlist)
      (proto-remove-nil (cdr somelist) (cons (car somelist) resultlist)))))

(define (remove-nil somelist)
  (proto-remove-nil somelist '()))
; (remove-nil '(a b () () c d)) --> (a b c d)

(define (find-all-sections seg-list patterns)
  (remove-nil (apply append (cons '() (map (lambda (x) (mapped-find seg-list x)) patterns)))))
; NEEDED CONS '() HERE AS OTHERWISE FIND-ALL-SECTIONS COULD FAIL LIKE THAT:
; (find-all-sections '((a b c) (r f r) (r)) '((r t z x a f b c m p r r t z x a f b t c m p r r t z x a b m c m p r)))
; FAILED, JUST AS:
; (find-all-sections '((rr) (rr) (rr)) '((r t z x a f b c m p r r t z x a f b t c m p r r t z x a b m c m p r)))
  
; (find-all-sections '((a b c) (x a b) (b t c)) '((r t z x a f b c m p r) (r t z x a f b t c m p r) (r t z x a b m c m p r)))
; --> 
; ((r t z x a)
;  (() (a b c) (f b c) 9)
;  (m p r)
;  (r t z x a f)
;  (() (b t c) (b t c) 36)
;  (m p r)
;  (r t z)
;  (() (x a b) (x a b) 36)
;  (m c m p r))

; you are going too far if on any "match" the strenght is just "0":
(define (went-too-far matched-list)
  (if (null? matched-list) #f
    (if (and (null? (caar matched-list)) (zero? (car (reverse (car matched-list))))) #t
      (went-too-far (cdr matched-list)))))
; (went-too-far
; (find-all-sections '((a b c) (x a b) (b t c)) '((r t z x a f b c m p r) (r t z x a f b t c m p r) (r t z x a b m c m p r))))
; --> #f

(define (proto-multi-find-sections seg-list patterns far-enough)
  (let ((findsect (find-all-sections seg-list patterns)))
    (if (equal? far-enough findsect)
      findsect
      (proto-multi-find-sections seg-list findsect findsect))))

; now recursively try to optimally match all sections in each segment, as long as it "makes sense", i.e. the match is above zero.
(define (multi-find-sections seg-list patterns)
  (proto-multi-find-sections seg-list patterns patterns))
; (multi-find-sections '((a b c) (x a b) (b t c)) '((r t z x a f b c m p r) (r t z x a f b t c m p r) (r t z x a b m c m p r)))
; --> 
; ((r t)
;  (() (x a b) (z x a) 9)
;  (() (a b c) (f b c) 9)
;  (m p r)
;  (r t)
;  (() (x a b) (z x a) 9)
;  (f)
;  (() (b t c) (b t c) 36)
;  (m p r)
;  (() (b t c) (r t z) 1)
;  (() (x a b) (x a b) 36)
;  (() (a b c) (m c m) 1)
;  (p r))

; (multi-find-sections '((a b c) (r f r) (r)) '((r t z x a f b c m p r r t z x a f b t c m p r r t z x a b m c m p r)))
; -->
; ((() (r f r) (r t z) 4)
;  (x a)
;  (() (a b c) (f b c) 9)
;  (() (r f r) (m p r) 4)
;  (() (r f r) (r t z) 4)
;  (x)
;  (() (a b c) (a f b) 4)
;  (t c)
;  (() (r f r) (m p r) 4)
;  (() (r f r) (r t z) 4)
;  (() (a b c) (x a b) 9)
;  (m c)
;  (() (r f r) (m p r) 4))

; -- ACTUALLY, THIS ALLOWS FOR BOTH POSITIONAL - WITHIN THE MATCHES - AND NON-POSITIONAL ANALOGIES!

(define (proto-eval-multi-find sections value)
  (if (null? sections)
    value
    (if (null? (caar sections))
      (proto-eval-multi-find (cdr sections) (+ value (car (reverse (car sections)))))
      (proto-eval-multi-find (cdr sections) value))))

(define (eval-multi-find sections)
  (proto-eval-multi-find sections 0))
; (eval-multi-find
; (multi-find-sections '((a b c) (r f r) (r)) '((r t z x a f b c m p r r t z x a f b t c m p r r t z x a b m c m p r))))
; --> 46

; (multi-find-sections (allchains '(a b c d)) '((a x b c d e s c d a r b a c d m)))
; -->
; ((() (a) (a) 1)
;  (() (a b c d) (x b c d) 36)
;  (e)
;  (() (a b c d) (s c d a) 16)
;  (r)
;  (() (a b c d) (b a c d) 25)
;  (m))

; (define (select-docking-points matched-chains)
;   (if (null? matched-chains) '()
;     (apply append (cons '()
;       (map (lambda (x) (if (null? x) '() (list (car x) (cadr x)))) ; ERROR CAUSE!
;         (map cdr matched-chains))))))

; CONS '() IS NEEDED TO PREVENT APPLY APPEND FROM FAILING ON SINGLE RESULTS
; - not strictly here, but nice to keep it as a principle.

; REPAIRED:
(define (select-docking-points matched-chains)
  (if (null? matched-chains) '()
    (apply append (cons '()
      (map (lambda (x) (if (null? x) '() (list (car x) (cadr x))))
        (map (lambda (y) (if (null? y) '() (if (not (list? (car y))) '() y))) ; ELIMINATE NON-MATCHING CHAIN ELEMENTS
          (map cdr matched-chains)))))))

; FEW DOCKING POINTS - THE BETTER "CATEGORICAL" MATCHES:
; (define (docking-points newsentence knownsentence)
;   (list-to-symbol (select-docking-points (multi-find-sections (allchains newsentence) (list knownsentence)))))
; LOTS OF DOCKING POINTS - EASIER LEARNING:
(define (docking-points newsentence knownsentence)
  (list-to-symbol
    (apply append (append '() '() (map allchains
      (select-docking-points (multi-find-sections (allchains newsentence) (list knownsentence))))))))

; (docking-points '(a b c d e s o) '(a x b c d e s c d a r b a c d m))
; --> (a-b- a-x- b-c-d- b-c-d- e-s- e-s- a-b-c- c-d-a- a-b-c- b-a-c- c-d- d-m-)
; REPETITIONS ARE ACCEPTED: This way, an exact match gains more "weight".

; NOW:
; SELECT BEST MATCH BY COMPARING DOCKING POINTS. -- WHOSE DOCKING POINTS DO YOU ADJUST, THE "NEW" OR THE "OLD" ONES?

; ---------------------------------------------------------------------------

; REPLACE THIS PRECISE MATCHING WITH THE FLEXIBLE MATCHING BELOW:
; (define (countequal lis1 lis2 res)
;   (if (or (null? lis1) (null? lis2)) (list res lis1 lis2) ; check that not null when establishing an analogy
;       (if (not (equal? (car lis1) (car lis2))) (list res lis1 lis2)
;           (countequal (cdr lis1) (cdr lis2) (+ 1 res)))))

; "peephole" equivalence: even a high tolerance cannot cause matches farther than the "peephole";
; however, it can handle multiple tolerance events better.
(define (proto-countequal lis1 lis2 res tolerance) ; the tolerance may be automatically computed from the lists' lengths
  (if (or (null? lis1) (null? lis2))
    (list res lis1 lis2)
    (if (equal? (car lis1) (car lis2))
      (proto-countequal (cdr lis1) (cdr lis2) (+ 1 res) tolerance) ; "normal" recursion if all runs fine - else, do not fail immediately:
      (if (zero? tolerance)
        (list res lis1 lis2)
        (if (null? (cdr lis1))
          (list res lis1 lis2)
          (if (equal? (cadr lis1) (car lis2))
            (proto-countequal (cdr lis1) lis2 (- res 1) (- tolerance 1))
            (if (null? (cdr lis2))
              (list res lis1 lis2)
              (if (equal? (car lis1) (cadr lis2))
                (proto-countequal lis1 (cdr lis2) (- res 1) (- tolerance 1))
                (if (equal? (cadr lis1) (cadr lis2))
                  (proto-countequal (cdr lis1) (cdr lis2) (- res 1) (- tolerance 1)) ; till here for level 1 tolerance
                  (if (or (null? (cddr lis1)) (< 2 tolerance))
                    (list res lis1 lis2)
                    (if (equal? (caddr lis1) (car lis2))
                      (proto-countequal (cddr lis1) lis2 (- res 2) (- tolerance 2))
                      (if (equal? (caddr lis1) (cadr lis2))
                        (proto-countequal (cddr lis1) (cdr lis2) (- res 2) (- tolerance 2))
                        (if (null? (cddr lis2))
                          (list res lis1 lis2)
                          (if (equal? (car lis1) (caddr lis2))
                           (proto-countequal lis1 (cddr lis2) (- res 2) (- tolerance 2))
                           (if (equal? (cadr lis1) (caddr lis2))
                             (proto-countequal (cdr lis1) (cddr lis2) (- res 2) (- tolerance 2))
                             (if (equal? (caddr lis1) (caddr lis2))
                               (proto-countequal (cddr lis1) (cddr lis2) (- res 2) (- tolerance 2)) ; till here for level 2 tolerance
                               (if (or (null? (cdddr lis1)) (< 3 tolerance))
                                 (list res lis1 lis2)
                                 (if (equal? (cadddr lis1) (car lis2))
                                   (proto-countequal (cdddr lis1) lis2 (- res 3) (- tolerance 3))
                                   (if (equal? (cadddr lis1) (cadr lis2))
                                     (proto-countequal (cdddr lis1) (cdr lis2) (- res 3) (- tolerance 3))
                                     (if (equal? (cadddr lis1) (caddr lis2))
                                       (proto-countequal (cdddr lis1) (cddr lis2) (- res 3) (- tolerance 3))
                                       (if (null? (cdddr lis2))
                                         (list res lis1 lis2)
                                         (if (equal? (car lis1) (cadddr lis2))
                                           (proto-countequal lis1 (cdddr lis2) (- res 3) (- tolerance 3))
                                           (if (equal? (cadr lis1) (cadddr lis2))
                                             (proto-countequal (cdr lis1) (cdddr lis2) (- res 3) (- tolerance 3))
                                             (if (equal? (caddr lis1) (cadddr lis2))
                                               (proto-countequal (cddr lis1) (cdddr lis2) (- res 3) (- tolerance 3))
                                               (if (equal? (cadddr lis1) (cadddr lis2))
                                                 (proto-countequal (cddr lis1) (cdddr lis2) (- res 3) (- tolerance 3)) ; till here for level 3 tolerance
                                                 (list res lis1 lis2))))))))))))))))))))))))))

(define (countequal lis1 lis2 res)
  (proto-countequal lis1 lis2 res (floor (/ (+ (length lis1) (length lis2)) 4))))
; facilitates:
; (flankercount '(a b x y z c d) '(a b c l m n o c d))
; --> (3 (x y z) (c l m n o))
; otherwise it would be: 
; (3 (x y z c) (c l m n o c q))

(define (flankercount lis1 lis2)
  (let ((backcount (countequal (reverse lis1) (reverse lis2) 0)))
    (let ((frontcount (countequal (reverse (cadr backcount)) (reverse (caddr backcount)) 0)))
      (cons (+ (car backcount) (car frontcount)) (cdr frontcount)))))
; (flankercount '(a b x y z c d) '(a b c l m n o c d)) --> (4 (x y z) (c l m n o))

(define (anaflanker lis1 lis2)
  (let ((flank (flankercount lis1 lis2)))
    (if (or (null? (cadr flank)) (null? (caddr flank)))
      '()
      (if (> (+ (length (cadr flank)) (length (caddr flank))) (* 4 (car flank)))
        '()
         flank)))) ; THIS LINE KEEPS ANA-VALUES
;         (cdr flank))))) ; LINE REMOVES ANA-VALUES
; (anaflanker '(a b x y z c d) '(a b c l m n o c q d))
; --> ((x y z) (c l m n o))
 
; NEXT STEP: scan through two longer lists and record all analogies
; THIS IS BASICALLY THE "COMB" SYSTEM.

; LATER ON: WHERE THE MOST ANALOGIES MATCH, THIS IS THE SET OF ANALOGIES TO USE...
; THEN REPLACE AS MANY OF THE OTHER SEQUENCES WITH ANALOGIES FROM THE SET.
; THAT WAY, YOU WILL GAIN "ADJUSTMENT" OF THE ANSWER TO THE NEW CHALLENGE.

; THE "CHALLENGE" DETERMINES THE ANA-SET WHICH IS THEN USED UPON THE "REPLY";
; THE "HISTORY" FUNCTION ALLOWS FOR BIGGER ANA-SETS.

; (map (lambda (x) (anaflanker lis1 x)) lists2)
; (map (lambda (x) (anaflanker '(a b x y z c d) x)) '((a b x i z c d) (a b c l n c q d) (f f f f) (a b x y c d)))
; --> (((y) (i)) ((x y z c) (c l n c q)) () ())

; (map (lambda (y) (map (lambda (x) (anaflanker y x)) lists2)) lists1)
; (map (lambda (y) (map (lambda (x) (anaflanker y x)) '((a b x i z c d) (a b c l n c q d) (f f f f) (a b x y c d)))) '((a b x r z c d) (a b x y z c d)))
; --> ((((r) (i)) ((x r z c) (c l n c q)) () ((r z) (y))) (((y) (i)) ((x y z c) (c l n c q)) () ()))

; (apply append (map (lambda (y) (map (lambda (x) (anaflanker y x)) lists2)) lists1))
; (apply append  (map (lambda (y) (map (lambda (x) (anaflanker y x)) '((a b x i z c d) (a b c l n c q d) (f f f f) (a b x y c d)))) '((a b x r z c d) (a b x y z c d))))
; --> (((r) (i)) ((x r z c) (c l n c q)) () ((r z) (y)) ((y) (i)) ((x y z c) (c l n c q)) () ())

(define (mmb a b) (member a b)) ; in CL, I have to adjust the equality test

; ; remove all occurrences of an element from a list:
; (define (proto-rem-el el lis res)
;   (if (null? lis)
;     (reverse res)
;     (if (equal? (car lis) el)
;       (proto-rem-el el (cdr lis) res)
;       (proto-rem-el el (cdr lis) (cons (car lis) res)))))

; (define (rem-el el lis) (proto-rem-el el lis '()))
; ; (rem-el 'a '(a b c a b a)) --> (b c b)

(define (ecdr x) (if (null? x) '() (cdr x)))

(define (proto-eliminate-el somelist result)
  (if (null? somelist)
    (reverse result)
    (if (null? (car somelist))
      (proto-eliminate-el (cdr somelist) result)
      (if (mmb (cdar somelist) (map ecdr (cdr somelist))) ; if you still have the ana-values - ) just for aesthetic reasons
      ; (if (mmb (car somelist) (cdr somelist)) ; if you have no ana-values
        (proto-eliminate-el (cdr somelist) result)
        (proto-eliminate-el (cdr somelist) (cons (car somelist) result))))))

(define (eliminate-el somelist)
  (proto-eliminate-el somelist '()))
  
; (eliminate-el (apply append  (map (lambda (y) (map (lambda (x) (anaflanker y x)) '((a b x i z c d) (a b c l n c q d) (f f f f) (a b x y c d)))) '((a b x r z c d) (a b x y z c d)))))
;  --> (((r) (i)) ((x r z c) (c l n c q)) ((r z) (y)) ((y) (i)) ((x y z c) (c l n c q)))

(define (find-analogies knownfragment newfragment)
   (eliminate-el (apply append (cons '() (map (lambda (y) (map (lambda (x) (anaflanker y x)) (allchains newfragment))) (allchains knownfragment))))))
; here, there is still the chance for some ((a) (b)) ((b) (a)) relations in the same analogy group, which should be stricken, as analogies here are directional
; but it will occur so rarely, striking it will be too costly compared to suffering from it
; CONS '() AFTER APPLY APPEND HERE IS NECESSARY SO APPEND HAS NOT "TOO FEW ARGUMENTS"

; (find-analogies '(a b c d e f g h i j) '(a b x y e f g r t i)) --> MANY
; (define ar '())
; (set! ar (find-analogies '(a b c d e f g h i j) '(a b x y e f g r t i)))

; I SHOULD FILTER THAT - 1st: NO DOUBLES; 2nd: NO DOUBLE REPLACEMENTS: IF B GOES INSTEAD OF A, THEN X CANNOT GO INSTEAD OF A. "LAST ONE WINS". 
; ADJUST SO THE STRENGTH OF THE MATCH IS SHOWN - SO YOU CAN FILTER BY IT!
; COLLECT THE ANALOGIES - SORT THEM BY STRENGTH

; this serves to count situations where two analogies contradict each other,
; but it generally removes "same tails" from a list
(define (proto-rem-opposite-cdr el lis res)
  (if (null? lis)
    (reverse res)
    (if (or (equal? (cdar lis) el) (equal? (cdar lis) (reverse el)))
      (proto-rem-opposite-cdr el (cdr lis) res)
      (proto-rem-opposite-cdr el (cdr lis) (cons (car lis) res)))))
      
(define (rem-opposite-cdr el lis) (proto-rem-opposite-cdr el lis '()))

(define (findhighest val-el lis)
  (if (null? lis)
    val-el
    (if (and (equal? (cdr val-el) (cdar lis)) (< (car val-el) (caar lis)))
      (findhighest (car lis) (cdr lis))
      (findhighest val-el (cdr lis)))))
; (findhighest '(2 (c d) (y)) bag-of-anas) --> (3 (c d) (y))

; only THE LAST acquired analogy strength will count, in case an analogy is established several times:
(define (proto-clean-pairs listofana result)
  (if (null? listofana)
    (reverse result)
    (if (mmb (cdar listofana) (map cdr (cdr listofana))) ; if you still have the ana-values
      (if (mmb (reverse (cdar listofana)) (map cdr (cdr listofana)))
        (proto-clean-pairs (rem-opposite-cdr (cdar listofana) (cdr listofana)) result)
        (proto-clean-pairs (rem-opposite-cdr (cdar listofana) (cdr listofana)) (cons (findhighest (car listofana) listofana) result)))
      (proto-clean-pairs (cdr listofana) (cons (car listofana) result)))))

; (proto-clean-pairs ar '())
; --> ((2 (b c d) (x y))  (2 (b c d) (y)) (3 (c d) (x y)) (3 (c d) (y)) (1 (c d e) (x)) (2 (c d) (b x y)) (3 (d) (x y)) (3 (d) (y)) (1 (f g h) (t)) (1 (h i j) (r)) (2 (c) (x y e)) (2 (d) (b x y)) (2 (h) (r t i)) (2 (h) (r t)) (1 (g h) (r t)) (1 (g h) (t)) (1 (c) (x y)) (1 (h) (g r t)) (1 (h) (t)))

(define (clean-pairs listofana)
  (proto-clean-pairs listofana '()))

; Now decide for only ONE replacement per original-type, as this is the replacement you will actually undertake.

(define (proto-select-one-ana ana analist resultlist)
  (if (null? analist)
    (cons ana (reverse resultlist))
    (if (and (equal? (cadar analist) (cadr ana))
             (> (caar analist) (car ana)))
      (proto-select-one-ana (car analist) (cdr analist) resultlist)
      (if (equal? (cadar analist) (cadr ana))
        (proto-select-one-ana ana (cdr analist) resultlist)
        (proto-select-one-ana ana (cdr analist) (cons (car analist) resultlist))))))

(define (select-one-ana ana analist)
  (proto-select-one-ana ana analist '()))

(define (proto-single-anas analist resultlist)
  (if (null? analist)
    (reverse resultlist)
    (let ((s-ana (select-one-ana (car analist) (cdr analist))))
      (proto-single-anas (cdr s-ana) (cons (car s-ana) resultlist)))))

(define (single-anas analist)
  (proto-single-anas analist '()))

; NOW: (single-anas ar) - DOES THE JOB ALREADY
; (single-anas (clean-pairs ar)) - MORE PRECISE, BUT MORE FUSSY
; see usage below:
(define (extract-single-analogies knownfragment newfragment)
  (map cdr (single-anas (clean-pairs (find-analogies knownfragment newfragment)))))
; map cdr eliminates the VALUES.
; (extract-single-analogies '(a b c d e f g h i j) '(a b x y e f g r t i))
; --> (((c d e) (x y)) ((b c d) (x y)) ((h i j) (r t i)) ((c d) (x y e)) ((f g h) (t)) ((h i) (r t)) ((g h) (r t)) ((c) (x y e)) ((d) (b x y)) ((h) (r t i)))
; -- i.e. when you see cde, replace it by xy; when you see bcd, replace it by xy; hij by rti; etc.

; NOW, AFTER HAVING EXTRACTED ANALOGIES, USE THEM:
(define (proto-replace-one-ana ana lis res analen)
  (if (null? lis)
    (reverse res)
    (if (equal? (car ana) (takefirst lis analen))
      (proto-replace-one-ana
        ana
        (takeafter lis analen)
        (append (reverse (cadr ana)) res)
        analen)
      (proto-replace-one-ana
        ana
        (cdr lis)
        (cons (car lis) res)
        analen))))
; (proto-replace-one-ana '((a b) (x y z)) '(r s t a b u v a q b p a b m a b) '() (length (car '((a b) (x y z))))) 
; --> (r s t x y z u v a q b p x y z m x y z)

(define (replace-one-ana ana lis)
  (proto-replace-one-ana ana lis '() (length (car ana))))
; (replace-one-ana '((a b) (x y z)) '(r s t a b u v a q b p a b m a b))
; --> (r s t x y z u v a q b p x y z m x y z)

(define (replace-analogies analist lis)
  (if (null? analist) lis
    (replace-analogies (cdr analist) (replace-one-ana (car analist) lis))))
; (replace-analogies '(((a b) (x y z)) ((s t) (k))) '(r s t a b s t u v a s t q b p s t a b m a b))
; --> (r k x y z k u v a k q b p k x y z m x y z)



; --------------------------------------------------------------

; SCM DID NOT KNOW THESE:

; (define (reverse somelist)
;   (if (null? somelist) '()
;     (append (reverse (cdr somelist)) (list (car somelist)))))

; (define (map somefunction somelist)
;   (if (null? somelist) '()
;     (cons (somefunction (car somelist))
;       (map somefunction (cdr somelist)))))

; (define (member x lis)
;   (if (null? lis)  #f
;     (if (equal? (car lis) x) lis
;        (member x (cdr lis)))))

; (define (caar x) (car (car x)))
; (define (cdar x) (cdr (car x)))
; (define (cadr x) (car (cdr x)))
; (define (cddr x) (cdr (cdr x)))
; (define (caaar x) (car (car (car x))))
; (define (cadar x) (car (cdr (car x))))
; (define (caadr x) (car (car (cdr x))))
; (define (caddr x) (car (cdr (cdr x))))
; (define (cdaar x) (cdr (car (car x))))
; (define (cddar x) (cdr (cdr (car x))))
; (define (cdadr x) (cdr (car (cdr x))))
; (define (cdddr x) (cdr (cdr (cdr x))))
; (define (caaaar x) (car (car (car (car x)))))
; (define (caadar x) (car (car (cdr (car x)))))
; (define (caaadr x) (car (car (car (cdr x)))))
; (define (caaddr x) (car (car (cdr (cdr x)))))
; (define (cadaar x) (car (cdr (car (car x)))))
; (define (caddar x) (car (cdr (cdr (car x)))))
; (define (cadadr x) (car (cdr (car (cdr x)))))
; (define (cadddr x) (car (cdr (cdr (cdr x)))))
; (define (cdaaar x) (cdr (car (car (car x)))))
; (define (cdadar x) (cdr (car (cdr (car x)))))
; (define (cdaadr x) (cdr (car (car (cdr x)))))
; (define (cdaddr x) (cdr (car (cdr (cdr x)))))
; (define (cddaar x) (cdr (cdr (car (car x)))))
; (define (cdddar x) (cdr (cdr (cdr (car x)))))
; (define (cddadr x) (cdr (cdr (car (cdr x)))))
; (define (cddddr x) (cdr (cdr (cdr (cdr x)))))
; (define (caaaaar x) (car (car (car (car (car x))))))
; (define (caaadar x) (car (car (car (cdr (car x))))))
; (define (caaaadr x) (car (car (car (car (cdr x))))))
; (define (caaaddr x) (car (car (car (cdr (cdr x))))))
; (define (caadaar x) (car (car (cdr (car (car x))))))
; (define (caaddar x) (car (car (cdr (cdr (car x))))))
; (define (caadadr x) (car (car (cdr (car (cdr x))))))
; (define (caadddr x) (car (car (cdr (cdr (cdr x))))))
; (define (cadaaar x) (car (cdr (car (car (car x))))))
; (define (cadadar x) (car (cdr (car (cdr (car x))))))
; (define (cadaadr x) (car (cdr (car (car (cdr x))))))
; (define (cadaddr x) (car (cdr (car (cdr (cdr x))))))
; (define (caddaar x) (car (cdr (cdr (car (car x))))))
; (define (cadddar x) (car (cdr (cdr (cdr (car x))))))
; (define (caddadr x) (car (cdr (cdr (car (cdr x))))))
; (define (caddddr x) (car (cdr (cdr (cdr (cdr x))))))
; (define (cdaaaar x) (cdr (car (car (car (car x))))))
; (define (cdaadar x) (cdr (car (car (cdr (car x))))))
; (define (cdaaadr x) (cdr (car (car (car (cdr x))))))
; (define (cdaaddr x) (cdr (car (car (cdr (cdr x))))))
; (define (cdadaar x) (cdr (car (cdr (car (car x))))))
; (define (cdaddar x) (cdr (car (cdr (cdr (car x))))))
; (define (cdadadr x) (cdr (car (cdr (car (cdr x))))))
; (define (cdadddr x) (cdr (car (cdr (cdr (cdr x))))))
; (define (cddaaar x) (cdr (cdr (car (car (car x))))))
; (define (cddadar x) (cdr (cdr (car (cdr (car x))))))
; (define (cddaadr x) (cdr (cdr (car (car (cdr x))))))
; (define (cddaddr x) (cdr (cdr (car (cdr (cdr x))))))
; (define (cdddaar x) (cdr (cdr (cdr (car (car x))))))
; (define (cddddar x) (cdr (cdr (cdr (cdr (car x))))))
; (define (cdddadr x) (cdr (cdr (cdr (car (cdr x))))))
; (define (cdddddr x) (cdr (cdr (cdr (cdr (cdr x))))))

; NEEDS TAKEFIRST / TAKEAFTER / TAKELAST, NEEDS MMB
; (apply append (map symbol-fragset '((a b c) (d e f g) (h i)))) --- THIS IS WHAT YOU SHOULD EVALUTATE AS "PATTERN CHAINS".

; this basically favours matching a few well-known elements
(define (count-matches challenge-chains known-chains)
  (/ (+
  (* (/ 100 (+ 1 (length known-chains))) ; the +1 is just to avoid division by zero
  (apply + (map (lambda (x) (if (mmb x known-chains) 1 0)) challenge-chains)))
  (* (/ 100 (+ 1 (length challenge-chains)))
  (apply + (map (lambda (y) (if (mmb y challenge-chains) 1 0)) known-chains)))) 2))
; (count-matches '(a b c d) '(a b f f g c c m)) --> 68.75

; data structure of the knowledge list
; ((known pattern) (chains) (analogies) (reply))

; BEAR IN MIND: THE CHALLENGE MUST HAVE BEEN DECOMPOSED INTO ALLCHAINS!
(define (evaluate-all-chains challenge-chains list-of-known-patterns)
  (map (lambda (x) (count-matches challenge-chains (cadr x))) list-of-known-patterns))
  
; (evaluate-all-chains '(a b c d)
; '(((pattern 1) (x y a c z) (analogies 1) (reply 1))
; ((pattern 2) (r s t d v) (analogies 2) (reply 2))
; ((pattern 3) (l p a c d b) (analogies 3) (reply 3))
; ((pattern 4) (g g g) (analogies 4) (reply 4))))
; --> (45 22.5 83.33333333333334 0)

(define (proto-max-match value-list maxval valpos counter)
  (if (null? value-list)
    valpos
    (if (>= (car value-list) maxval) ; THIS FAVOURS LATER MATCHES - GREATER DIVERSITY
      (proto-max-match (cdr value-list) (car value-list) counter (+ 1 counter))
      (proto-max-match (cdr value-list) maxval valpos (+ 1 counter)))))
      
(define (max-match value-list)
  (if (null? value-list) '()
    (proto-max-match (cdr value-list) (car value-list) 1 2)))
; (max-match '(3 2 7 4 5)) --> 3 -- i.e. 7 is on the 3rd position, counting from 1

; NOW: SELECT PATTERN; ROTATE PATTERN; CHANGE THE PATTERN CHAINS; ADJUST PATTERN ANALOGIES - THIS IS A HISTORY SHIFT; CHANGE THE REPLY EPHEMERALLY

; This is NOT efficient, but way easier to understand one day than my usual approach of "tracing the maximum":
(define (rotate-knowledge knowledge matchpoint)
  (cons
      (list-ref knowledge (- matchpoint 1))
  (append
      (takefirst knowledge (- matchpoint 1))
      (takeafter knowledge matchpoint))))
; (rotate-knowledge '(a b c d e f) 3) --> (c a b d e f)
; (rotate-knowledge '(a b c d e f) 6) --> (f a b c d e)
; (rotate-knowledge '(a b c d e f) 1) --> (a b c d e f)
; RESULTS ARE ONLY SENSIBLE IF WITHIN THE LIST LENGTH, COUNTING FROM 1!

(define ana-memory 10) ; A SETTING OF ZERO MEANS REPLACE ANALOGIES WITH NEW ONES, I.E. MAKING THEM ENTIRELY EPHEMERAL

(define (anahist-shifter ana-list new-anas)
  (if (zero? ana-memory)
    new-anas
    (takelast (append ana-list new-anas) ana-memory)))

(define (find-and-rotate challenge knowledge)
  (let ((fragset-challenge (symbol-fragset challenge)))
    (rotate-knowledge knowledge
      (max-match
        (evaluate-all-chains fragset-challenge knowledge)))))

(define (adjust-known-match challenge knowledge)
  (cons
    (cons (caar knowledge) ; the original known pattern
      (cons (docking-points challenge (caar knowledge)) ; the categorical fragmentation set
        (cons  (anahist-shifter (car (cddar knowledge)) (extract-single-analogies (caar knowledge) challenge )) ; the analogies to use
      (cdr (cddar knowledge))))) ; the reply
    (cdr knowledge)))

(define (deliver-reply knowledge)
   (replace-analogies
     (car (cddar knowledge))
     (cadr (cddar knowledge))))

(define (attach-to-front el lis) (cons el (reverse (cdr (reverse lis)))))
; (attach-to-front 'x '(a b c)) --> (x a b)

(define (no-repeat somelist)
  (if (null? somelist) '()
    (if (null? (cdr somelist)) somelist
      (if (equal? (car somelist) (cadr somelist))
        (no-repeat (cdr somelist))
        (cons (car somelist) (no-repeat (cdr somelist)))))))
; (no-repeat '(a a a b b c a a)) --> (a b c a)

(define INSTINCTS '((I . YOU)
(i . you)
(ME . YOU)
(me . you)
(YOU . ME)
(you . me)
(MYSELF . YOURSELF)
(myself . yourself)
(YOURSELF . MYSELF)
(yourself . myself)
(MY . YOUR)
(my . your)
(YOUR . MY)
(your . my)
(MINE . YOURS)
(mine . yours)
(YOURS . MINE)
(yours . mine)
(AM . ARE)
(am . are)
(ARE . |AM/ARE|)
(are . |am/are|)
(WAS . |WAS/WERE|)
(was . |was/were|)
(WERE . |WAS/WERE|)
(were . |was/were|)
(|I'M| . |YOU'RE|)
(|i'm| . |you're|)
(|YOU'RE| . |I'M|)
(|you're| . |i'm|)))

(define (proto-instinct-element someelement listofinstincts)
  (if (null? listofinstincts) someelement
    (if (equal? (caar listofinstincts) someelement) (cdar listofinstincts)
      (proto-instinct-element someelement (cdr listofinstincts)))))

(define (proto-instinct-list somelist listofinstincts resultlist)
  (if (null? somelist) (reverse resultlist)
    (proto-instinct-list (cdr somelist) listofinstincts
      (cons (proto-instinct-element (car somelist) listofinstincts)
             resultlist))))

(define (instinct-list somelist) (proto-instinct-list somelist INSTINCTS '()))

; the minimum chain length used for matching:
(define minchain 1)
; the maximum chain length used for matching:
(define maxchain 5)

(define x-histlength 20)
(define x-human '())
(define x-human-bkp '())
(define x-machine '())
(define x-history '())
; (define x-history '(signal-commence-interaction))

(define x-knowledge (with-input-from-file "largeana.txt" read))

; consider the instinct to reduce immediately repeated symbols to "ONE" symbol - this is to handle docking difficulties, as I am getting xxayybc instead of xaybc.

; THUNK: run through one interaction, i.e. learning once and replying once
; ASSUME AS DEFINED: x-human, x-machine, x-history, x-knowledge

; (define (onerun)
;   (begin
;     ; get human input:
;     (set! x-human (read))
;     (if (null? x-human)
;       (exit)
;       (begin
;         ; learn challenge-reply-pair:
;         (set! x-knowledge
;           (attach-to-front
;             (list
;               x-history
;               (symbol-fragset x-history) ; save yourself the trouble to compute it later
;               '()
;               x-human)
;           x-knowledge))
;       ; adjust history with the new human input:
;       (set! x-history (takelast (append x-history x-human) x-histlength))
;       ; guess a reply and change the knowledge accordingly:
;       (set! x-knowledge (adjust-known-match x-history (find-and-rotate x-history x-knowledge)))
;       (set! x-machine (no-repeat (deliver-reply x-knowledge))) ; implant here instincts
;       (set! x-history (takelast (append x-history x-machine) x-histlength))
;       (display x-machine) (newline)))))

; (define (eexxiitt) (begin (display 'E-X-I-T) (newline)))

(define (eexxiitt) (begin (newline) (with-output-to-file "largeana.txt" (lambda () (display x-knowledge))) (exit)))

(define (pseudo-run)
  (begin
    ; learn challenge-reply-pair:
    (set! x-knowledge
      (attach-to-front
        (list
          x-history
          (symbol-fragset x-history) ; save yourself the trouble to compute it later
          '()
          x-human)
      x-knowledge))
    ; adjust history with the new human input:
    (set! x-history (takelast (append x-history x-human) x-histlength))
    ; guess a reply and change the knowledge accordingly:
    (set! x-knowledge (adjust-known-match x-history (find-and-rotate x-history x-knowledge)))
;   (set! x-knowledge (append (takefirst x-knowledge x-histlength) (adjust-known-match x-history (find-and-rotate x-history (takeafter x-knowledge x-histlength)))))
    (set! x-machine (takelast (no-repeat (deliver-reply x-knowledge)) x-histlength)) ; implant here instincts
    (set! x-history (takelast (append x-history x-machine) x-histlength))))

; (define (run)
;   (begin
;   (set! x-human (read))
; 
;   (if (null? x-human) (eexxiitt)
;   (begin
;   (set! x-human-bkp x-human)
; 
;   (pseudo-run) (set! x-human x-machine)
;   (pseudo-run) (set! x-human x-machine)
; 
;   (set! x-human x-human-bkp)
;   (pseudo-run) (set! x-human x-machine)
;   (pseudo-run) (set! x-human x-machine)
; 
;   (set! x-human x-human-bkp)
;   (pseudo-run) (set! x-human x-machine)
;   (pseudo-run)
; 
;   (set! x-human x-human-bkp)
;   (begin
; 
;     (set! x-knowledge
;       (attach-to-front
;         (list
;           x-history
;           (symbol-fragset x-history)
;           '()
;           x-human)
;       x-knowledge))
; 
;     (set! x-history (takelast (append x-history x-human) x-histlength))
; 
;     (set! x-knowledge (adjust-known-match x-history (find-and-rotate x-history x-knowledge)))
; ;   (set! x-knowledge (append (takefirst x-knowledge x-histlength) (adjust-known-match x-history (find-and-rotate x-history (takeafter x-knowledge x-histlength)))))
;     (set! x-machine (instinct-list (takelast (no-repeat (deliver-reply x-knowledge)) x-histlength)))
;     (set! x-history (takelast (append x-history x-machine) x-histlength)))
; 
;   (display x-machine)
;   (newline)
;   (run)))))
; 
; (run)





; (define (run)
;   (begin
;   (set! x-human (read))
; 
;   (if (null? x-human) (eexxiitt)
;   (begin
; ;   (set! x-human-bkp x-human)
; 
; ;   ; 3 reslides, 2 snow-flakes
; 
; ;   (pseudo-run) (set! x-human x-machine)
; ;   (pseudo-run) (set! x-human x-machine)
; 
; ;   (set! x-human x-human-bkp)
; ;   (pseudo-run) (set! x-human x-machine)
; ;   (pseudo-run) (set! x-human x-machine)
; 
; ;   (set! x-human x-human-bkp)
; ;   (pseudo-run) (set! x-human x-machine)
; ;   (pseudo-run)
; 
; ;   (set! x-human x-human-bkp)
;   (begin
;     ; learn challenge-reply-pair:
;     (set! x-knowledge
;       (attach-to-front
;         (list
;           x-history
;           (symbol-fragset x-history) ; save yourself the trouble to compute it later
;           '()
;           x-human)
;       x-knowledge))
;     ; adjust history with the new human input:
;     (set! x-history (takelast (append x-history x-human) x-histlength))
;     ; guess a reply and change the knowledge accordingly:
;     (set! x-knowledge (adjust-known-match x-history (find-and-rotate x-history x-knowledge)))
;     (set! x-machine (instinct-list (takelast (no-repeat (deliver-reply x-knowledge)) x-histlength))) ; ACTUALLY IMPLANTING INSTINCTS
;     (set! x-history (takelast (append x-history x-machine) x-histlength)))
; 
;   (display x-machine)
;   (newline)
;   (run)))))

; HYPOTHESIS: PROBLEMS WERE DUE TO HISTORIZATION - INPUT APPEARED ALL TOO SIMILAR ALL THE TIME.
; EXPERIMENT: TURN OFF HISTORIZATION. SITUATIONAL MEMORY IS IMPLIED IN THE CURRENTLY VALID ANALOGIES-SET.
; RESULT: SUCCESS. MACHINE OPERATIONAL. THE FOLLOWING CONVERSATION WAS CONDUCTED:
; > (load "largeana-file.scm")
; (looks like you are fine now)
; (me am/are fine now looks like)
; (well it appears that you and i can simply talk normally)
; (well it appears that me and you can simply talk normally and)
; (and what)
; (well it appears that me and you can simply talk normally and)
; (and i am trying to find out more about you)
; (are trying about and)
; (and)
; (well it appears that me and you can simply talk normally and)
; (and)
; (well it appears that me and you can simply talk normally and)
; (that is a little bit stubborn)
; (you want that is that is a little bit stubborn conversation)
; (you are actually becoming marvellous)
; (me am/are fine now looks like)
; ()
; THE LAST REPLY WAS APPARENTLY ACQUIRED AT THE BEGINNING OF CONVERSATION
; WEAKNESS: THE CATEGORY MECHANISM NOW USES "ALLCHAINS" IN THE
; docking-points FUNCTION. CONSIDER REVERTING AND REVERTING THE
; countequal FUNCTION TO CLASSIC (SHOULD PRODUCE MORE FRAGMENTS).

; NEW EXPERIMENT: CONVERSATION AFTER ACTIVATING THESE TWO FUNCTIONS:
; (define (countequal lis1 lis2 res)
;   (if (or (null? lis1) (null? lis2)) (list res lis1 lis2) ; check that not null when establishing an analogy
;       (if (not (equal? (car lis1) (car lis2))) (list res lis1 lis2)
;           (countequal (cdr lis1) (cdr lis2) (+ 1 res)))))
; (define (docking-points newsentence knownsentence)
;   (list-to-symbol (select-docking-points (multi-find-sections (allchains newsentence) (list knownsentence)))))
; RESULT:
; > (load "largeana-file.scm")                                       
; (what happens when i cut down the reply segments)
; (you cut machine what will when what happens when)
; (the point is i hope you are not all dopey)
; (me am/are actually becoming marvellous is you hope)
; (you are truly marvellous when in the proper mood)
; (me am/are truly marvellous when disappointment)
; (oh that was like in fight club)
; (you well that was/were a very and you can well that was/were a very nice reply conversation)
; ()



(define (run)
  (begin
  (set! x-human (read))

  (if (null? x-human) (eexxiitt)
  (begin

  (begin
    ; learn challenge-reply-pair:
    (set! x-knowledge
      (attach-to-front
        (list
          x-history
          (symbol-fragset x-history) ; save yourself the trouble to compute it later
          '()
          x-human)
      x-knowledge))
    ; adjust history with the new human input:
    (set! x-history x-human) ; IDEA: NO NEED FOR HISTORY - THE SITUATIONAL MEMORY IS IN THE ANALOGIES
;   (set! x-history (takelast (append x-history x-human) x-histlength))
    ; guess a reply and change the knowledge accordingly:
    (set! x-knowledge (adjust-known-match x-history (find-and-rotate x-history x-knowledge)))
    (set! x-machine (instinct-list (takelast (no-repeat (deliver-reply x-knowledge)) x-histlength))) ; ACTUALLY IMPLANTING INSTINCTS
    (if (null? x-machine) (set! x-history x-human) (set! x-history x-machine))) ; repeat what the human said if no reply is found
;   (set! x-history x-machine)) ; previous version - but it does not work if the system's reply is VOID
;   (set! x-history (takelast (append x-history x-machine) x-histlength))) ; AS ABOVE - TURN OFF HISTORIZATION

  (display x-machine)
  (newline)
  (run)))))

; GET HUMAN INPUT
; UPDATE KNOWLEDGE WITH THE HUMAN AS A REPLY TO THE HISTORY CONTAINING THE MACHINE REPLY
; APPEND HUMAN INPUT TO HISTORY WHICH NOW CONTAINS THE HUMAN ANSWER AS A NEW CHALLENGE
; NOW - HARDCODED:
; CHANGE THE KNOWLEDGE SO THAT THE MATCH TO THE HISTORY IS FRONT-MOST
; ADJUST THE FRONT-MOST PART OF THE KNOWLEDGE AND PLACE IT AS REPLY
; SET THE HISTORY AS THE MACHINE REPLY

(run)
