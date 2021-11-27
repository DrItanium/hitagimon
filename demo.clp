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
          (triangle (x0 0) (y0 0)
                    (x1 1) (y1 1)
                    (x2 2) (y2 2)
                    (color (color565 255 0 0)))
          (triangle (x0 3) (y0 3)
                    (x1 4) (y1 4)
                    (x2 5) (y2 5)
                    (color (color565 0 255 0)))
          (triangle (x0 6) (y0 6)
                    (x1 7) (y1 7)
                    (x2 8) (y2 8)
                    (color (color565 0 0 255))))



