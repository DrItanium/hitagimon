(deffunction color565
             (?r ?g ?b)
             (display:color565 ?r
                               ?g
                               ?b))
(deftemplate triangle
             (slot id
                   (type SYMBOL)
                   (default-dynamic (gensym*)))
             (slot x0
                   (type INTEGER)
                   (default ?NONE))
             (slot y0
                   (type INTEGER)
                   (default ?NONE))
             (slot x1
                   (type INTEGER)
                   (default ?NONE))
             (slot y1
                   (type INTEGER)
                   (default ?NONE))
             (slot x2
                   (type INTEGER)
                   (default ?NONE))
             (slot y2
                   (type INTEGER)
                   (default ?NONE))
             (slot color
                   (type INTEGER)
                   (default ?NONE)))

(defrule draw-triangle
 (declare (salience 1))
         ?f <- (triangle (x0 ?x0)
                         (y0 ?y0)
                         (x1 ?x1)
                         (y1 ?y1)
                         (x2 ?x2)
                         (y2 ?y2)
                         (color ?c)
                         (id ?id))
         =>
         (assert (move-triangle ?id))
         (display:draw-triangle ?x0 ?y0 ?x1 ?y1 ?x2 ?y2 ?c))

(defrule move-triangle-off
         ?f <- (move-triangle ?id)
         ?k <- (triangle (id ?id)
                         (x0 ?x0)
                         (y0 ?y0)
                         (x1 ?x1)
                         (y1 ?y1)
                         (x2 ?x2)
                         (y2 ?y2)
                         (color ?c))
         =>
         (retract ?f)
         (modify ?k
                 (x0 (+ ?x0 1)) (y0 (+ ?y0 1))
                 (x1 (+ ?x1 1)) (y1 (+ ?y1 1))
                 (x2 (+ ?x2 1)) (y2 (+ ?y2 1)) ))

(deffacts triangles
          (triangle (x0 5) (y0 5)
                    (x1 10) (y1 10)
                    (x2 15) (y2 15)
                    (color (color565 255 0 0)))
          (triangle (x0 25) (y0 25)
                    (x1 30) (y1 30)
                    (x2 35) (y2 35)
                    (color (color565 0 255 0)))
          (triangle (x0 40) (y0 40)
                    (x1 45) (y1 45)
                    (x2 50) (y2 50)
                    (color (color565 0 0 255)))
          )



