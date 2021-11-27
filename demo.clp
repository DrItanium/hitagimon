(deffunction color565
             (?r ?g ?b)
             (display:color565 ?r
                               ?g
                               ?b))
(deffunction check-time
             (?fn $?rest)
             (bind ?start-time (rtc:unixtime))
             (bind ?result 
                   (funcall ?fn (expand$ ?rest)))
             (bind ?end-time (rtc:unixtime))
             (printout t "(" ?fn " " (expand$ ?rest) ") took: " 
                       (- ?end-time ?start-time) crlf)
             ?result)
(deffunction clear-screen
             ()
             (display:fill-screen (color565 0 0 0)))

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

(defrule draw-triangle-generic
         (check triangle ?fn)
         (triangle (x0 ?x0)
                   (y0 ?y0)
                   (x1 ?x1)
                   (y1 ?y1)
                   (x2 ?x2)
                   (y2 ?y2)
                   (color ?c))
         =>
         (clear-screen)
         (check-time ?fn
                     ?x0 
                     ?y0 
                     ?x1 
                     ?y1 
                     ?x2 
                     ?y2 
                     ?c))

(deffacts triangles
          (check triangle display:draw-triangle)
          (check triangle display:draw-triangle32)
          (check triangle display:draw-triangle64)
          (triangle (x0 50) (y0 50)
                    (x1 100) (y1 100)
                    (x2 150) (y2 150)
                    (color (color565 255 0 0))))



