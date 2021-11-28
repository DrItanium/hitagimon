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
(deffunction update-seed
             ()
             (seed (rtc:unixtime)))
(defglobal MAIN
           ?*display-width* = 128
           ?*display-height* = 160
           ?*color-black* = (display:color565 0 0 0)
           ?*color-white* = (display:color565 255 255 255)
           ?*color-red* = (display:color565 255 0 0)
           ?*color-green* = (display:color565 0 255 0)
           ?*color-blue* = (display:color565 0 0 255)
           ?*color-purple* = (display:color565 255 0 255)
           ?*color-yellow* = (display:color565 0 255 255)
           ?*color-red-green* = (display:color565 255 255 0)
           )
(deffunction check-time
             (?fn $?rest)
             (bind ?start-time (rtc:unixtime))
             (bind ?result 
                   (funcall ?fn (expand$ ?rest)))
             (bind ?end-time 
                   (rtc:unixtime))
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
             (update-seed)
             (bind ?qcount
                   (- (div ?count 
                           8) 
                      8))
             ; always provide these colors to be on the safe side
             (bind ?colors 
                   ?*color-black*
                   ?*color-white*
                   ?*color-red*
                   ?*color-green*
                   ?*color-blue*
                   ?*color-purple*
                   ?*color-yellow*
                   ?*color-red-green*)
             (loop-for-count ?qcount do
                             (bind ?colors
                                   ?colors
                                   (random 0 65536)
                                   (random 0 65536)
                                   (random 0 65536)
                                   (random 0 65536)
                                   (random 0 65536)
                                   (random 0 65536)
                                   (random 0 65536)
                                   (random 0 65536)))
             ?colors)
(deffunction get-display-height-number-of-colors
             ()
             (clear-screen)
             (bind ?result 
                   (generate-random-color-selection ?*display-height*))
             (update-seed)
             ?result)

(deffunction clear-screen-test
             ()
             (clear-screen)
             (printout t "Clear screen test!" crlf)
             (progn$ (?color (generate-random-color-selection ?*display-height*)) do
                     (clear-screen ?color)))
(deffunction unified-offset
             (?max ?advance ?x ?y ?modulo)
             (max ?max 
                  (integer 
                    (mod 
                      (+ ?advance 
                         ?x 
                         ?y) 
                      ?modulo))))
(deffunction draw-rect-test
             ()
             (printout t "draw rect test!" crlf)
             (bind ?colors
                   (get-display-height-number-of-colors))
             (loop-for-count (?x 0 ?*display-width*) do
                             (bind ?x-end 
                                   (- ?*display-width* ?x))
                             (loop-for-count (?y 0 ?*display-height*) do
                                             (bind ?y-end
                                                   (- ?*display-height* ?y))
                                             (display:draw-rect ?x ?y
                                                                ?x-end 
                                                                ?y-end
                                                                (nth$ (unified-offset 1 1 ?y ?x ?*display-height*)
                                                                      ?colors)))))
(deffunction fill-rect-test 
             ()
             (printout t "fill rect test!" crlf)
             (bind ?colors
                   (get-display-height-number-of-colors))
             (loop-for-count (?x 0 ?*display-width*) do
                             (bind ?x-end 
                                   (- ?*display-width* ?x))
                             (loop-for-count (?y 0 ?*display-height*) do
                                             (bind ?y-end
                                                   (- ?*display-height* 
                                                      ?y))
                                             (display:fill-rect ?x ?y
                                                                ?x-end 
                                                                ?y-end
                                                                (nth$ (unified-offset 1 1 ?y ?x ?*display-height*)
                                                                      ?colors)))))
(deffunction draw-circle-test 
             ()
             (printout t "draw circle test!" crlf)
             (bind ?colors
                   (get-display-height-number-of-colors))
             (loop-for-count (?radius 16 256) do
                             (display:draw-circle (random 0 ?*display-width*)
                                                  (random 0 ?*display-height*)
                                                  ?radius
                                                  (nth$ (random 1 ?*display-height*) 
                                                        ?colors))))

(deffunction fill-circle-test 
             ()
             (printout t "fill circle test!" crlf)
             (bind ?colors
                   (get-display-height-number-of-colors))
             (loop-for-count (?radius 16 (random 17 64)) do
                             (display:fill-circle (random 0 ?*display-width*)
                                                  (random 0 ?*display-height*)
                                                  ?radius
                                                  (nth$ (random 1 ?*display-height*) 
                                                        ?colors))))

(deffunction pixel-test
             ()
             (printout t "pixel test!" crlf)
             (bind ?colors
                   (get-display-height-number-of-colors))
             (loop-for-count (?x 0 ?*display-width*) do
                             (loop-for-count (?y 0 ?*display-height*) do
                                             (display:draw-pixel ?x 
                                                                 ?y
                                                                 (nth$ (unified-offset 1 1 ?y ?x ?*display-height*)
                                                                       ?colors)))))

(deffunction draw-line-test
             () 
             (printout t "line test!" crlf)
             (bind ?colors
                   (get-display-height-number-of-colors))
             (loop-for-count (?x 0 
                                 ?*display-width*) do
                             (loop-for-count (?y 0 ?*display-height*) do
                                             (loop-for-count (?x0 ?x ?*display-width*) do
                                                             (loop-for-count (?y0 ?y ?*display-height*) do
                                                                             (display:draw-line ?x 
                                                                                                ?y 
                                                                                                ?x0 
                                                                                                ?y0 
                                                                                                (nth$ (unified-offset 1 
                                                                                                                      1 
                                                                                                                      ?y 
                                                                                                                      ?x 
                                                                                                                      ?*display-height*)
                                                                                                      ?colors)))))))
