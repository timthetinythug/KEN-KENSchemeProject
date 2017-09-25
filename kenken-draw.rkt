(module a10 scheme
  (require racket/gui/base)
  (require racket/draw)
  
  (define (get-size puzz)
    (vector-ref (struct->vector puzz) 1))
  
  (define (get-board puzz)
    (vector-ref (struct->vector puzz) 2))
  
  (define (get-constraints puzz)
    (vector-ref (struct->vector puzz) 3))
  
  (define (get-guess-symbol gue)
    (vector-ref (struct->vector gue) 1))
  
  (define (get-guess-number gue)
    (vector-ref (struct->vector gue) 2))
  
  (define font-exp (make-object font% 5 'default))
  
  (define font-val (make-object font% 40 'default))
  
  (define square-size 50)
  
  (define blank-board false)
  
  (define blank-constraints false) 
  
  (define (first-occur ch lst)
    (cond
      [(empty? lst) false]
      [(symbol=? ch (first lst)) 0]
      [else (add1 (first-occur ch (rest lst)))]))
  
  (define (flatten list)
    (cond
      [(empty? list) empty]
      [else (append (first list) (flatten (rest list)))]))
  
  (define (get-grid-x sec grid)
    (remainder (first-occur sec (flatten grid))
               (length grid)))
  
  (define (get-grid-y sec grid)
    (quotient (first-occur sec (flatten grid))
              (length grid)))
  
  (define (gen-exp func total)
    (cond
      [(symbol=? '* func) (string-append (number->string total) "x")]
      [(symbol=? '= func) (number->string total)]
      [else (string-append (number->string total)
                           (symbol->string func))]))
  
  (define (invert-grid grid)
    (cond
      [(empty? (first grid)) empty]
      [else (cons (foldr (lambda (x y) (cons (first x) y)) empty grid)
                  (invert-grid (map rest grid)))]))
  
  
  (define board-frame (new frame% [label "Solve-KenKen"]
                           [width (* 8 square-size)]
                           [height (+ 22 (* 8 square-size))]
                           [x 50]
                           [y 50]))
  
  (define board-canvas (new canvas% [parent board-frame]))
  
  (define board-dc (send board-canvas get-dc))
  
  (define (resize-frame new-size)
    (begin (show-board false)
           (set! board-frame (new frame% [label "Solve-KenKen"]
                                  [width (* 2 new-size square-size)]
                                  [height (+ 22 (* 2 new-size square-size))]
                                  [x 50]
                                  [y 50]))
           (set! board-canvas (new canvas% [parent board-frame]))
           (set! board-dc (send board-canvas get-dc))
           (send board-dc set-scale 2 2)
           (show-board true)))
  
  (define (puzzle-setup puzz draw-option)
    (cond 
      [(not (symbol=? draw-option 'off))
       (begin
         ;create the best aproximation of an empty board
         (set! blank-board (map (lambda (x) (map (lambda (y)
                                                   (cond 
                                                     [(struct? y) (get-guess-symbol y)]
                                                     [(number? y) 'empty]
                                                     [else y])) x))
                                (get-board puzz)))
         (set! blank-constraints (get-constraints puzz))
         (resize-frame (get-size puzz)))]))
  
  
  (define (show-board bool)
    (send board-frame show bool))
  
  (define (draw-board puzz draw-option)
    (cond
      [(not (symbol=? draw-option 'off))
       (local
         [(define (draw puzz)
            (begin
              (send board-dc clear)
              (send board-dc set-pen "black" 6 'solid)
              (local
                ;draw the separating lines for a single row
                [(define (draw-row-vert row x-coord y-coord)
                   (cond
                     [(empty? (rest row)) (begin
                                            (send board-dc set-pen "black" 3 'solid)
                                            (send board-dc draw-line x-coord y-coord x-coord (+ square-size y-coord)))]
                     [(symbol=? (first row) (first (rest row))) (begin 
                                                                  (send board-dc set-pen "grey" 0.2 'solid)
                                                                  (send board-dc draw-line x-coord y-coord x-coord (+ square-size y-coord))
                                                                  (draw-row-vert (rest row) (+ square-size x-coord) y-coord))]
                     [else (begin
                             (send board-dc set-pen "black" 2 'solid)
                             (send board-dc draw-line x-coord y-coord x-coord (+ square-size y-coord))
                             (draw-row-vert (rest row) (+ square-size x-coord) y-coord))]))
                 
                 ;draw separating lines for each row
                 (define (draw-grid-vert grid y-coord)
                   (cond
                     [(empty? grid) (begin
                                      (send board-dc set-pen "black" 3 'solid)
                                      (send board-dc draw-line 0 (* (get-size puzz) square-size)
                                            (* (get-size puzz) square-size) (* (get-size puzz) square-size)))]
                     [else (begin
                             (draw-row-vert (first grid) square-size y-coord)
                             (draw-grid-vert (rest grid) (+ square-size y-coord)))]))]
                
                (draw-grid-vert blank-board 0))
              
              (local
                [;switch the rows and columns so that columns can be easilly worked with
                 (define invert-puzzle (invert-grid blank-board))
                 ;draw separating lines for a column
                 (define (draw-col-horz row x-coord y-coord)
                   (cond
                     [(empty? (rest row)) (begin
                                            (send board-dc set-pen "black" 3 'solid)
                                            (send board-dc draw-line x-coord y-coord (+ square-size x-coord)  y-coord))]
                     [(symbol=? (first row) (first (rest row))) (begin 
                                                                  (send board-dc set-pen "grey" 0.2 'solid)
                                                                  (send board-dc draw-line x-coord y-coord (+ square-size x-coord) y-coord)
                                                                  (draw-col-horz (rest row) x-coord (+ square-size y-coord)))]
                     [else (begin
                             (send board-dc set-pen "black" 2 'solid)
                             (send board-dc draw-line x-coord y-coord (+ square-size x-coord) y-coord)
                             (draw-col-horz (rest row) x-coord (+ square-size y-coord)))]))
                 
                 ;draw separating lines for each column
                 (define (draw-grid-horz grid x-coord)
                   (cond
                     [(empty? grid)]
                     [else (begin
                             (draw-col-horz (first grid)  x-coord square-size)
                             (draw-grid-horz (rest grid) (+ square-size x-coord)))]))]
                
                (draw-grid-horz invert-puzzle 0))
              
              (send board-dc set-font font-exp)
              
              ;fill in the expressions in the top left of squares
              (map (lambda (reg) 
                     (send board-dc draw-text (gen-exp (third reg) (second reg)) 
                           (+ 2 (* square-size (get-grid-x (first reg) blank-board)))
                           (+ 2 (* square-size (get-grid-y (first reg) blank-board))))) blank-constraints)
              
              (send board-dc set-font font-val)
              
              ;fill in a row with Numeric values
              (define (fill-grid-row row x-coord y-coord board-dc)
                (cond
                  [(empty? row)]
                  [(number? (first row)) (begin
                                           (send board-dc draw-text (number->string (first row)) x-coord y-coord)
                                           (fill-grid-row (rest row) (+ square-size x-coord) y-coord board-dc))]
                  [(struct? (first row)) (begin
                                           (send board-dc draw-text (number->string (get-guess-number (first row))) x-coord y-coord)
                                           (fill-grid-row (rest row) (+ square-size x-coord) y-coord board-dc))]
                  [else (fill-grid-row (rest row) (+ square-size x-coord) y-coord board-dc)]))
              
              ;fill in all Numeric values
              (define (fill-grid grid y-coord)
                (cond
                  [(empty? grid)]
                  [else (begin (fill-grid-row (first grid) 8 y-coord (send board-canvas get-dc))
                               (fill-grid (rest grid) (+ square-size y-coord)))]))
              
              ;add in a delay to make each step readable
              (fill-grid (get-board puzz) 0)
              (cond 
                [(symbol=? 'slow draw-option) (sleep 0.5)]
                [(symbol=? 'fast draw-option) (sleep 0.025)]
                [else (sleep 0.1)])
              true))]
         (draw puzz))]))
  
  (provide  draw-board puzzle-setup))