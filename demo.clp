; the following declarations are only meant for debugging on my desktop
;(defgeneric display:draw-rect)
;(defgeneric display:fill-rect)
;(defgeneric display:draw-circle)
;(defgeneric display:fill-circle)
;(defgeneric display:draw-pixel)
;(defgeneric display:fill-screen)
;(defgeneric display:draw-line)
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
           ?*random-colors0* = (create$))
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
(deffunction enter-test-routine 
             (?router $?message)
             (printout ?router (expand$ ?message) crlf)
             (clear-screen)
             (if (= (length$ ?*random-colors0*) 0) then
               (bind ?*random-colors0*
                     (generate-random-color-selection ?*display-height*))
               (update-seed)))

(deffunction clear-screen-test
             ()
             (enter-test-routine t "Clear screen test!")
             (progn$ (?color ?*random-colors0*) do
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
(deffunction get-unified-offset-color
             (?y ?x)
             (nth$ (unified-offset 1 1 ?y ?x ?*display-height*)
                   ?*random-colors0*))
(deffunction get-random-color
             ()
             (nth$ (random 1 ?*display-height*)
                   ?*random-colors0*))

(deffunction draw-rect-test
             ()
             (enter-test-routine t "draw rect test!")
             (loop-for-count (?x 0 ?*display-width*) do
                             (bind ?x-end 
                                   (- ?*display-width* ?x))
                             (loop-for-count (?y 0 ?*display-height*) do
                                             (bind ?y-end
                                                   (- ?*display-height* ?y))
                                             (display:draw-rect ?x ?y
                                                                ?x-end 
                                                                ?y-end
                                                                (get-unified-offset-color ?y ?x)))))
(deffunction fill-rect-test 
             ()
             (enter-test-routine t "fill rect test!")
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
                                                                (get-unified-offset-color ?y ?x)))))
(deffunction draw-circle-test 
             ()
             (enter-test-routine t "draw circle test!")
             (loop-for-count (?radius 16 256) do
                             (display:draw-circle (random 0 ?*display-width*)
                                                  (random 0 ?*display-height*)
                                                  ?radius
                                                  (get-random-color))))

(deffunction fill-circle-test 
             ()
             (enter-test-routine t "fill circle test!")
             (loop-for-count (?radius 16 (random 17 64)) do
                             (display:fill-circle (random 0 ?*display-width*)
                                                  (random 0 ?*display-height*)
                                                  ?radius
                                                  (get-random-color))))

(deffunction pixel-test
             ()
             (enter-test-routine t "pixel test!")
             (loop-for-count (?x 0 ?*display-width*) do
                             (loop-for-count (?y 0 ?*display-height*) do
                                             (display:draw-pixel ?x 
                                                                 ?y
                                                                 (get-unified-offset-color ?y ?x)))))

(deffunction draw-line-test
             () 
             (enter-test-routine t "line test!")
             (loop-for-count (?x 0 ?*display-width*) do
                             (loop-for-count (?y 0 ?*display-height*) do
                                             (bind ?target-color 
                                                   (get-unified-offset-color ?y ?x))
                                             (loop-for-count (?x0 ?x ?*display-width*) do
                                                             (loop-for-count (?y0 ?y ?*display-height*) do
                                                                             (display:draw-line ?x 
                                                                                                ?y 
                                                                                                ?x0 
                                                                                                ?y0 
                                                                                                ?target-color))))))
