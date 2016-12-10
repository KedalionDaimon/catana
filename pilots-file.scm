; the "cheklimit" and "reslimit" can be up to (length lists)
(define (proto-collect-pointers elem position lists checklimit resultlimit result)
  (if (or (null? lists) (or (zero? checklimit) (zero? resultlimit)))
    (reverse result)
    (if (<= (length (caar lists)) position)
      (proto-collect-pointers elem position (cdr lists) checklimit resultlimit result)
      (if (equal? elem (list-ref (caar lists) position))
        (proto-collect-pointers elem position (cdr lists) (- checklimit 1) (- resultlimit 1) (cons (cadar lists) result))
        (proto-collect-pointers elem position (cdr lists) (- checklimit 1) resultlimit result)))))
        
(define (collect-pointers elem position knowledge)
  (let ((chklim (length knowledge))) ; NOT NECESSARILY
    (let ((reslim chklim)) ; NOT NECESSARILY, POTENTIALLY MUCH SHORTER
      (proto-collect-pointers elem position knowledge chklim reslim '()))))

; (collect-pointers 'x 2 '(((a b c d e) f)  ((p q x b q) r) ((k c l e c l) s) ((v w x y) z) ((m) n) ((x x x y y y) z)))
; --> (r z z)

(define (genlist len)
  (if (zero? len) '()
    (cons (- len 1) (genlist (- len 1)))))
; (genlist 4) --> (3 2 1 0)

; make list contain only one exemplar of each element
(define (uni-elem lis)
  (if (null? lis) '()
    (if (member (car lis) (cdr lis))
      (uni-elem (cdr lis))
      (cons (car lis) (uni-elem (cdr lis))))))
; (uni-elem '(a a b c a d b)) --> (c a d b)

; (define (collect-all-pointers elements knowledge)
;   (uni-elem (apply append (cons '()
;     (map (lambda (x y) (collect-pointers x y knowledge)) elements (genlist (length elements)))))))
; (collect-all-pointers '(x p) '(((p a b c d e) f)  ((p q x b q) r) ((k c l e c l) s) ((o w x y) z) ((m) n) ((x x x y y y) z)))
; --> (z f r)
; (collect-all-pointers '(none) '(((p a b c d e) f)  ((p q x b q) r) ((k c l e c l) s) ((o w x y) z) ((m) n) ((x x x y y y) z)))
; --> ()
; YOU NEED NO EXTRA FUNCTION, FUNCTIONALITY IMPLIED BELOW

(define (elim-el el lis)
  (if (null? lis) '()
    (if (equal? (car lis) el)
      (elim-el el (cdr lis))
      (cons (car lis) (elim-el el (cdr lis))))))
; (elim-el 'a '(x y z a b c a b w)) --> (x y z b c b w)

(define (antitheses challenge-reply-pair knowledge)
  (let ((proto-res
    (list (cadr challenge-reply-pair)
      (elim-el (cadr challenge-reply-pair)
        (uni-elem (apply append (cons '()
          (map (lambda (x y) (collect-pointers x y knowledge)) (car challenge-reply-pair) (genlist (length (car challenge-reply-pair)))))))))))
    (if (null? (cadr proto-res)) '() proto-res)))

; (antitheses '((x p) winner) '(((p a b c d e) f)  ((p q x b q) r) ((k c l e c l) s) ((o w x y) z) ((m) n) ((x x x y y y) z)))
; --> (winner (z f r)) -- i.e. "winner" beats z, f and r as consequences

(define (cleanse-replacements newelement known-replacements)
  (map
    (lambda (x)
      (if (and (member (car x) (cadr newelement))
               (member (car newelement) (cadr x)))
      (list (car x) (elim-el (car newelement) (cadr x)))
      x))
    known-replacements))
; (cleanse-replacements '(x (t r)) '((f (p a b c d e))  (r (p q x b q)) (s (k c l e c l)) (z (o w x y)) (n (m)) (t (x x x y y y))))
; -->
; ((f (p a b c d e))
; (r (p q b q)) ; CLEANSED
; (s (k c l e c l))
; (z (o w x y)) ; NOT CLEANSED - z is not part of the "overruled" atoms
; (n (m))
; (t (y y y))) ; CLEANSED

; NEXT STEP: MERGE IN KNOWN LIST. THEN - FIND ANTITHESES, OF THESE CLEANSE THE REPLACEMENTS, THIS KNOWLEDGE BASE THEN USE TO IMPLANT THE NEW WINNER LIST

(define (proto-find-and-merge newelement known-replacements checked)
  (if (null? known-replacements)
    (cons newelement (reverse (cdr checked)))
    (if (equal? (caar known-replacements) (car newelement))
      (append (cons 
                (list (car newelement) (uni-elem (append (cadr newelement) (cadar known-replacements))))
                (reverse checked))
              (cdr known-replacements))
      (proto-find-and-merge newelement (cdr known-replacements) (cons (car known-replacements) checked)))))
;  (proto-find-and-merge '(n (t r)) '((f (p a b c d e))  (r (p q x b q)) (s (k c l e c l)) (z (o w x y)) (n (m)) (t (x x x y y y))) '())
; -->
; ((n (t r m))
;  (f (p a b c d e))
;  (r (p q x b q))
;  (s (k c l e c l))
;  (z (o w x y))
;  (t (x x x y y y)))

; (proto-find-and-merge '(x (t r)) '((f (p a b c d e))  (r (p q x b q)) (s (k c l e c l)) (z (o w x y)) (n (m)) (t (x x x y y y))) '())
; -->
; ((x (t r))
;  (f (p a b c d e))
;  (r (p q x b q))
;  (s (k c l e c l))
;  (z (o w x y))
;  (n (m)))

(define (find-and-merge newelement known-replacements)
  (proto-find-and-merge newelement known-replacements '()))

(define (update-prevalences challenge-reply-pair knowledge known-replacements)
  (let ((new-prevalence (antitheses challenge-reply-pair knowledge)))
    (if (null? new-prevalence)
      known-replacements
      (find-and-merge new-prevalence
        (cleanse-replacements new-prevalence known-replacements)))))
; (update-prevalences
; '((x p) winner)
; '(((p a b c d e) f) ((p q x b q) r) ((k c l e c l) s) ((o w x y) z) ((m) n) ((x x x y y y) z))
; '((f (p a b c winner e)) (r (p q x b q)) (s (winner c l e c l)) (z (o w winner y)) (n (m)) (t (x x x y y y)))); -->
; ((winner (z f r))
;  (f (p a b c e))
;  (r (p q x b q)) ; unaffected - did not contain winner, though in (z f r)
;  (s (winner c l e c l)) ; unaffected - not in (z f r)
;  (z (o w y))
;  (n (m)))

; the "prevalences" basically show which atom would prevail against which other atoms. 

; NEXT STEP: USE THE PREVALENCES.
; When a "challenge" has been matched to "knowledge", there is a "preliminary reply".
; This can be changed by "prevalences".
; If an element of the "challenge" is different from an element of the (car knowledge),
; then look where last time it was matched in its position in the knowledge.
; Record that "other reply". Compare whether the "preliminary reply" is killed by the "other reply".
; If yes, replace as new "preliminary reply". [This is a bit of a simplification. There can by multi-element cycles.]
; Otherwise, do not replace.
; Go through all un-matching elements and try to see whether there are replacements.
; Finally, the reply is the "preliminary reply", whatever it then currently is.

(define (last-seen-at-position el pos knowledge)
  (if (null? knowledge)
    '()
    (if (and (> (length (caar knowledge)) pos) (equal? el (list-ref (caar knowledge) pos)))
      (cadar knowledge)
      (last-seen-at-position el pos (cdr knowledge)))))

; (last-seen-at-position 'w 1 '(((p a b c d e) f) ((p q x b q) r) ((k c l e c l) s) ((o w x y) z) ((m) n) ((x x x y y y) z)))
; --> z -- because "w" is seen at position "1" (counting from 0) in ((o w x y) z), and z is the reply to that.

(define (check-prevalence reply known-replacements)
  (if (null? known-replacements)
    '()
    (if (equal? (caar known-replacements) reply)
      (cadar known-replacements)
      (check-prevalence reply (cdr known-replacements)))))
; (check-prevalence 'z '((f (p a b c winner e)) (r (p q x b q)) (s (k c l e c l)) (z (o w winner y)) (n (m)) (t (x x x y y y))))
; --> (o w winner y)

(define (suggested-prevalence el pos knowledge known-replacements)
  (let ((seen (last-seen-at-position el pos knowledge)))
    (if (null? seen) '()
      (list seen (check-prevalence seen known-replacements)))))
; (suggested-prevalence 'w 1 
; '(((p a b c d e) f) ((p q x b q) r) ((k c l e c l) s) ((o w x y) z) ((m) n) ((x x x y y y) z))
; '((f (p a b c winner e)) (r (p q x b q)) (s (k c l e c l)) (z (o w winner y)) (n (m)) (t (x x x y y y))))
; --> (z (o w winner y))

(define (ecadr x) (if (null? x) '() (cadr x)))

(define (check-reply challenge match pos preliminary-reply knowledge known-replacements)
  (if (null? challenge)
    preliminary-reply
    (if (equal? (car challenge) (car match))
      (check-reply (cdr challenge) (cdr match) (+ 1 pos) preliminary-reply knowledge known-replacements)
      (let ((old-prevalence (suggested-prevalence (car challenge) pos knowledge known-replacements)))
        (if (not (member preliminary-reply (ecadr old-prevalence)))
          (check-reply (cdr challenge) (cdr match) (+ 1 pos) preliminary-reply knowledge known-replacements) ; same as above
          (check-reply (cdr challenge) (cdr match) (+ 1 pos) (car old-prevalence) knowledge known-replacements)))))) ; CHANGE the reply

; (check-reply '(a b w) '(a b c) 0 'reply
; '(((p a b c d e) f) ((p q x b q) r) ((k c l e c l) s) ((o w x y) z) ((m) n) ((x x x y y y) z))
; '((f (p a b c winner e)) (r (p q x b q)) (s (k c l e c l)) (z (o w winner y)) (n (m)) (t (x x x y y y))))
; --> reply

; (check-reply '(a w) '(a b) 0 'winner
; '(((p a b c d e) f) ((p q x b q) r) ((k c l e c l) s) ((o w x y) z) ((m) n) ((x x x y y y) z))
; '((f (p a b c winner e)) (r (p q x b q)) (s (k c l e c l)) (z (o w winner y)) (n (m)) (t (x x x y y y))))
; --> z -- because "w" leads to "z", and "z" kills "winner"

; ------------------------------------------------------------

(define (proto-rotate-or-shift challenge-reply-pair knowledge seen)
  (if (null? knowledge)
    (cons challenge-reply-pair (reverse (cdr seen))) ; pair not found - forget last element, learn new pair
    (if (equal? (caar knowledge) (car challenge-reply-pair)) ; challenge-part of the pair is found
      (cons challenge-reply-pair (append (reverse seen) (cdr knowledge))) ; re-learn pair (possibly with new consequence)
      (proto-rotate-or-shift challenge-reply-pair (cdr knowledge) (cons (car knowledge) seen)))))

(define (rotate-or-shift challenge-reply-pair knowledge)
  (proto-rotate-or-shift challenge-reply-pair knowledge '()))

; (rotate-or-shift '((m) k) '(((p a b c d e) f) ((p q x b q) r) ((k c l e c l) s) ((o w x y) z) ((m) n) ((x x x y y y) z)))
; -->
; (((m) k) ((p a b c d e) f) ((p q x b q) r) ((k c l e c l) s) ((o w x y) z) ((x x x y y y) z))

(define (proto-takefirst fromwhere howmany resultlist)
  (if (or (null? fromwhere) (zero? howmany)) (reverse resultlist)
    (proto-takefirst (cdr fromwhere) (- howmany 1) (cons (car fromwhere) resultlist))))

(define (takefirst fromwhere howmany) (proto-takefirst fromwhere howmany '()))

(define (takelast fromwhere howmany) (reverse (takefirst (reverse fromwhere) howmany)))

(define (takeafter fromwhere howmany)
  (if (or (null? fromwhere) (zero? howmany)) fromwhere
    (takeafter (cdr fromwhere) (- howmany 1))))
; (takeafter '(a b c d e f) 2) --> (c d e f)

(define (proto-pilotize window historie pairlist)
  (if (null? historie) (reverse pairlist)
    (proto-pilotize
      (append (cdr window) (list (car historie)))
      (cdr historie)
      (cons (list window (car historie)) pairlist))))

; historie is assumed to contain SIGNAL-TERMINATOR at the end.
; OR - SIGNAL-TERMINATOR CAN BE '()
(define (pilotize window historie)
  (proto-pilotize window historie '()))
; (pilotize '(a b c) '(d e f g h))      
; --> (((a b c) d) ((b c d) e) ((c d e) f) ((d e f) g) ((e f g) h))

; it is assumed that the lists are of same length:
(define (compare-lists lis1 lis2)
  (apply + (map (lambda (x y) (if (equal? x y) 1 0)) lis1 lis2)))
; (compare-lists '(a b c d) '(a b x d)) --> 3

(define (check-all-matches challenge knowledge)
  (map (lambda (x) (compare-lists challenge (car x))) knowledge))
; (check-all-matches '(a b c) '(((a x y) z) ((r p x) b) ((a b q) l) ((q b c) v)))
; --> (1 0 2 2)

(define (proto-select-maximum lis vallis valcand candidate)
  (if (null? lis)
    (if (zero? valcand) ; a "candidate" without any match would be nonsense
      '()
      candidate)
    (if (>= (car vallis) valcand)
      (proto-select-maximum (cdr lis) (cdr vallis) (car vallis) (car lis))
      (proto-select-maximum (cdr lis) (cdr vallis) valcand candidate))))
; no knowledge rotation upon selection of reply, but only - if at all - during observation

; assume non-zero knowledge
(define (select-maximum lis vallis)
    (proto-select-maximum (cdr lis) (cdr vallis) (car vallis) (car lis)))
; (select-maximum '(a b c d) '(1 3 2 0)) --> b

(define (find-best-match window knowledge)
  (select-maximum knowledge (check-all-matches window knowledge)))
; (find-best-match '(a b c) '(((a x y) z) ((r p x) b) ((a b q) l) ((q b c) v)))
; --> ((q b c) v) -- ((a b q) l) would be the alternative for "early" matching
; (find-best-match '(g g g) '(((a x y) z) ((r p x) b) ((a b q) l) ((q q c) v)))
; --> () -- no match at all

; WITHOUT REPLACEMENTS BY ANALOGY:
; (define (proto-free-flight window knowledge maxlen currlen result)
;   (let ((match (find-best-match window knowledge)))
;     (if (or (null? match) (> currlen maxlen))
;       (reverse (cons '() result)); initially: (reverse result) - but I want a uniform reply type!
;       (if (null? (cadr match))
;         (reverse (cons '() result)) ; attach the terminator to the result
;         (proto-free-flight
;           (append (cdr window) (list (cadr match)))
;           knowledge
;           maxlen
;           (+ 1 currlen)
;           (cons (cadr match) result))))))

; WITH REPLACEMENTS BY ANALOGY:
(define (proto-free-flight window knowledge known-replacements maxlen currlen result)
  (let ((match (find-best-match window knowledge)))
    (if (or (null? match) (> currlen maxlen))
      (reverse (cons '() result)); initially: (reverse result) - but I want a uniform reply type!
;     (reverse result) ; i.e. a () is NOT appended to the machine reply
      (if (null? (cadr match))
        (reverse (cons '() result)) ; attach the terminator to the result
;       reverse result) ; i.e. a () is NOT appended to the machine reply
        (proto-free-flight
          (append (cdr window) (list (cadr match)))
          knowledge
          known-replacements
          maxlen
          (+ 1 currlen)
          (cons
            (check-reply window (car match) 0 (cadr match) knowledge known-replacements)
            result))))))

; (define (free-flight window knowledge)
;   (proto-free-flight window knowledge x-max-reply-length 0 '()))

(define (free-flight window knowledge known-replacements)
  (proto-free-flight window knowledge known-replacements x-max-reply-length 0 '()))

; WITHOUT REPLACEMENTS BY ANALOGY:
; (free-flight '(a b c)
; '(((c y r) b) ((i i n) g) ((a p c) d) ((b c s) e) ((c d d) f)
; ((d x f) g) ((y y g) h) ((f g h) ()) ((g h ()) i) ((h () i) j)))
; --> (d e f g h ())
;
; WITHOUT REPLACEMENTS BY ANALOGY:
; REALISTICALLY - challenges will end in a terminator:
; (free-flight '(a b ())
; '(((c y r) b) ((i i n) g) ((a p c) d) ((b c s) e) ((c d d) f)
; ((d x f) g) ((y y g) h) ((f g h) ()) ((g h ()) i) ((h () i) j)))
; --> (i j g h ())

; WITH REPLACEMENTS BY ANALOGY:
; REALISTIC SCENARIO - NO REPLACEMENTS IN THIS CASE:
;  (free-flight '(a b ())
; '(((c y r) b) ((i i n) g) ((a p c) d) ((b c s) e) ((c d d) f)
; ((d x f) g) ((y y g) h) ((f g h) ()) ((g h ()) i) ((h () i) j))
; '((f (p a b c winner e)) (r (p q x b q)) (s (k c l e c l)) (z (o w winner y)) (n (m)) (t (x x x y y y))))
; --> (i j g h ())
;  (free-flight '(a b c)
; '(((c y r) b) ((i i n) g) ((a p c) d) ((b c s) e) ((c d d) f)
; ((d x f) g) ((y y g) h) ((f g h) ()) ((g h ()) i) ((h () i) j))
; '((f (p a b c winner e)) (r (p q x b q)) (s (k c l e c l)) (z (o w winner y)) (n (m)) (t (x x x y y y))))
; --> (d f f g h ()) - A REPLACEMENT HAS BEEN APPLIED

; ------------------------------------------------------------

(define (adjust-knowledge-by-pairs challenge-reply-pairs knowledge known-replacements)
  (if (null? challenge-reply-pairs)
    (list knowledge known-replacements) ; i.e. the result is in car & cadr
    (adjust-knowledge-by-pairs
      (cdr challenge-reply-pairs)
      (rotate-or-shift (car challenge-reply-pairs) knowledge)
      (update-prevalences (car challenge-reply-pairs) knowledge known-replacements))))

; (adjust-knowledge-by-pairs
; '(((a b c) d) ((b c d) e) ((c d e) f))
; '(((a x y) z) ((r p x) b) ((a b q) l) ((q b c) v))
; '((f (p a b c winner e)) (r (p q x b q)) (s (k c l e c l)) (z (o w winner y)) (n (m)) (t (x x x y y y))))
; -->
; ((((e f g) h) ((d e f) g) ((c d e) f) ((b c d) e))
;  ((h (f))
;   (g (e))
;   (f (d p a b c winner e))
;   (d (l v))
;   (r (p q x b q))
;   (s (k c l e c l))))

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

(define x-human '())
(define x-human-bkp '())
(define x-machine '())
(define x-history '(() () () () () ())) ; as the window
(define x-max-reply-length 20)
(define x-window 6) ; smaller or equal to x-max-reply-length

(define x-knowledge (with-input-from-file "pilots1.txt" read))

; prototype for four -- x-window:
; ((() () () ()) ())
; prototype for eight -- x-window:
; ((() () () () () () () ()) ())

(define x-known-replacements (with-input-from-file "pilots2.txt" read))

; (define (onerun)
;   (begin
;     ; get human input:
;     (set! x-human (append (read) '(()))) ; human input ends on a terminator
;     (if (equal? '(()) x-human) ; human said nothing?
;       (print 'exit)
;       (begin
;         ; learn series of challenge-reply-pairs
;         (let ((adjustment
;                (adjust-knowledge-by-pairs
;                  (pilotize x-history x-human)
;                  x-knowledge
;                  x-known-replacements)))
;           (begin
;             (set! x-knowledge (car adjustment))
;             (set! x-known-replacements (cadr adjustment))))
;         (set! x-history (takelast (append x-history x-human) x-window))
;         (set! x-machine (free-flight x-history x-knowledge x-known-replacements))
;         (set! x-history (takelast (append x-history x-machine) x-window))
;         (display x-machine) (newline)))))

(define (pseudo-run) ; no reply printing, no reading of x-human
  (begin
    ; learn series of challenge-reply-pairs
    (let ((adjustment
           (adjust-knowledge-by-pairs
             (pilotize x-history x-human)
             x-knowledge
             x-known-replacements)))
      (begin
        (set! x-knowledge (car adjustment))
        (set! x-known-replacements (cadr adjustment))))
    (set! x-history (takelast (append x-history x-human) x-window))
    (set! x-machine (free-flight x-history x-knowledge x-known-replacements))))

; (define (eexxiitt) (begin (display 'E-X-I-T) (newline)))

(define (eexxiitt)
(begin (newline)
(with-output-to-file "pilots1.txt" (lambda () (display x-knowledge)))
(with-output-to-file "pilots2.txt" (lambda () (display x-known-replacements)))
(exit)))

(define (run)
  (begin
  (set! x-human (append (read) '(())))

  (if (equal? '(()) x-human) (eexxiitt)
  (begin
  (set! x-human-bkp x-human)

  ; 3 reslides, 2 snow-flakes

  (pseudo-run) (set! x-human x-machine)
  (pseudo-run) (set! x-human x-machine)

  (set! x-human x-human-bkp)
  (pseudo-run) (set! x-human x-machine)
  (pseudo-run) (set! x-human x-machine)

  (set! x-human x-human-bkp)
  (pseudo-run) (set! x-human x-machine)
  (pseudo-run)

  (set! x-human x-human-bkp)
  (pseudo-run)
  (set! x-machine (instinct-list x-machine))
  (set! x-history (takelast (append x-history x-machine) x-window))
  (display x-machine)
  (newline)
  (run)))))

; (run)
; (a b c d)
; ...
; ()
; E-X-I-T

(run)


; MIND x-history!!!!!!!!

