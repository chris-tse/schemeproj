;returns the list after removing first n items
(define (nthcdr n list)
	(if (= n 0)
		list
		(nthcdr (- n 1) (cdr list))))

;returns the first n items of the list
(define (nthcar n lst)
	(if (= n 1)
		(list (car lst))
		(cons (car lst) (nthcar (- n 1) (cdr lst)))))

(define (poly_add pola polb)
	(if (>= (length pola) (length polb))
		(append (map (lambda (x y) (+ x y)) (nthcar (length polb) pola) polb) (nthcdr (length polb) pola))
		(poly_add polb pola)))

(define (poly_sub pola polb)
	(if (> (length pola) (length polb))
		(append (map (lambda (x y) (- x y)) (nthcar (length polb) pola) polb) (nthcdr (length polb) pola))
		(map (lambda (x) (* x -1)) (poly_sub polb pola))))

(define (poly_der pol)
	(map (lambda (x y) (* x y))(cdr pol) (genlist (- (length pol) 1))))

(define (poly_mul pola polb)
	(define flen (- (+ (length pola) (length polb)) 1))
	(if (= (length polb) 0)
		`()
		(poly_add (multil (car polb) pola) (cons 0 (poly_mul pola (cdr polb))))))
		

(define (poly_mod rpola rpolb)
	(define pola (reverse rpola))
	(define polb (reverse rpolb))
	(if (< (length pola) (length polb))
		rpola
		(if (= (length pola) (length polb))
			(reverse (map (lambda (x) (* -1 x)) (cdr (map (lambda (x y) (- y x)) pola (multil (/ (car pola) (car polb)) polb)))))
			(poly_mod (reverse (cdr (map (lambda (x y) (- x y)) pola (padr (- (length pola) (length polb)) (multil (/ (car pola) (car polb)) polb))))) (reverse polb)))))

; creates a list from 1 to num
(define (genlist num)
	(if (= num 0)
		(list)
		(append (genlist (- num 1)) (list num))))

;insert n zeroes to the right of list
(define (padr n l)
	(if (= n 0)
		l
		(padr (- n 1) (append l `(0)))))

;multiply elements in l by a
(define (multil a l)
	(if (null? l)
		`()
		(cons (* a (car l)) (multil a (cdr l)))))

(define fata `(1 2 3))
(define fatb `(3 4 5 6))
