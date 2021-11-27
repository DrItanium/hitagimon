; the following declarations are only meant for debugging on my desktop
;(defgeneric display:draw-rect)
;(defgeneric display:fill-rect)
;(defgeneric display:draw-circle)
;(defgeneric display:fill-circle)
;(defgeneric display:draw-pixel)
;(defgeneric display:fill-screen)
;(defgeneric rtc:unixtime)
;(deffunction display:color565 (?r ?g ?b) (random 0 65536))
;(deffunction display:height () 320)
;(deffunction display:width () 240)
(defglobal MAIN
           ?*display-width* = (display:width)
           ?*display-height* = (display:height)
           ?*color-black* = (display:color565 0 0 0)
           ?*color-white* = (display:color565 255 255 255)
           ?*color-red* = (display:color565 255 0 0)
           ?*color-green* = (display:color565 0 255 0)
           ?*color-blue* = (display:color565 0 0 255)
           ?*color-purple* = (display:color565 255 0 255)
           ?*color-yellow* = (display:color565 0 255 255))
(deffunction check-time
             (?fn $?rest)
             (bind ?start-time (rtc:unixtime))
             (bind ?result 
                   (funcall ?fn (expand$ ?rest)))
             (bind ?end-time (rtc:unixtime))
             (printout t "(" ?fn " " (expand$ ?rest) ") took: " 
                       (- ?end-time ?start-time) crlf)
             ?result)
(defgeneric clear-screen)
(defmethod clear-screen
  ((?value INTEGER))
  (display:fill-screen ?value))
(defmethod clear-screen
  ((?red INTEGER)
   (?green INTEGER)
   (?blue INTEGER))
  (display:fill-screen 
    (display:color565 ?red 
                      ?green 
                      ?blue)))

(defmethod clear-screen
  ()
  (clear-screen ?*color-black*))
(deffunction generate-random-color-selection
             (?count)
             (bind ?qcount
                   (div ?count 4))
             (bind ?colors 
                   (create$))
             (printout t "Generating " ?count " random colors!" crlf)
             (loop-for-count ?qcount do
                             (bind ?colors
                                   ?colors
                                   ; just generate a random 16-bit number
                                   (random 0 65536)
                                   (random 0 65536)
                                   (random 0 65536)
                                   (random 0 65536)))
             (printout t "Done" crlf)
             ?colors)
(deffunction clear-screen-test
             ()
             (clear-screen)
             (printout t "Clear screen test!" crlf)
             (progn$ (?color (generate-random-color-selection ?*display-height*)) do
                     (clear-screen ?color)))
(deffunction draw-rect-test
             ()
             (clear-screen)
             (printout t "draw rect test!" crlf)
             (bind ?colors
                   (generate-random-color-selection ?*display-height*))
             (loop-for-count (?x 0 ?*display-width*) do
                             (bind ?x-end 
                                   (- ?*display-width* ?x))
                             (loop-for-count (?y 0 ?*display-height*) do
                                             (bind ?y-end
                                                   (- ?*display-height* ?y))
                                             (display:draw-rect ?x ?y
                                                                ?x-end 
                                                                ?y-end
                                                                (nth$ (integer (mod (+ 1 ?y ?x) ?*display-height*))
                                                                      ?colors)))))
(deffunction fill-rect-test 
             ()
             (printout t "fill rect test!" crlf)
             (clear-screen)
             (bind ?colors
                   (generate-random-color-selection ?*display-height*))
             (loop-for-count (?x 0 ?*display-width*) do
                             (bind ?x-end 
                                   (- ?*display-width* ?x))
                             (loop-for-count (?y 0 ?*display-height*) do
                                             (bind ?y-end
                                                   (- ?*display-height* ?y))
                                             (display:fill-rect ?x ?y
                                                                ?x-end 
                                                                ?y-end
                                                                (nth$ (integer (mod (+ 1 ?y ?x) ?*display-height*))
                                                                      ?colors)))))
(deffunction draw-circle-test 
             ()
             (printout t "draw circle test!" crlf)
             (clear-screen)
             (bind ?colors
                   (generate-random-color-selection ?*display-height*))
             (loop-for-count (?radius 16 (random 17 64)) do
                             (display:draw-circle (random 0 ?*display-width*)
                                                  (random 0 ?*display-height*)
                                                  ?radius
                                                  (nth$ (random 0 ?*display-height*) 
                                                        ?colors))))

(deffunction fill-circle-test 
             ()
             (printout t "fill circle test!" crlf)
             (clear-screen)
             (bind ?colors
                   (generate-random-color-selection ?*display-height*))
             (loop-for-count (?radius 16 (random 17 64)) do
                             (display:fill-circle (random 0 ?*display-width*)
                                                  (random 0 ?*display-height*)
                                                  ?radius
                                                  (nth$ (random 0 ?*display-height*) 
                                                        ?colors))))

(deffunction pixel-test
             ()
             (printout t "pixel test!" crlf)
             (clear-screen)
             (bind ?colors
                   (generate-random-color-selection ?*display-height*))
             (loop-for-count (?x 0 ?*display-width*) do
                             (loop-for-count (?y 0 ?*display-height*) do
                                             (display:draw-pixel ?x 
                                                                 ?y
                                                                (nth$ (integer (mod (+ 1 ?y ?x) ?*display-height*))
                                                                      ?colors)))))
