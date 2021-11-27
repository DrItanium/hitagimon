(defglobal MAIN
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
             (bind ?colors 
                   (create$))
             (printout t "Generating " ?count " random colors!" crlf)
             (loop-for-count ?count do
                             (bind ?colors
                                   ?colors
                                   (display:color565 (random 0 255)
                                                     (random 0 255)
                                                     (random 0 255))))
             (printout t "Done" crlf)
             ?colors)
(deffunction clear-screen-test
             ()
             (clear-screen)
             (printout t "Clear screen test!" crlf)
             (progn$ (?color (generate-random-color-selection 320)) do
                     (clear-screen ?color)))
(deffunction draw-rect-test
             ()
             (clear-screen)
             (printout t "draw rect test!" crlf)
             (bind ?colors
                   (generate-random-color-selection 324))
             (loop-for-count (?x 0 240) do
                             (bind ?x-end 
                                   (- 240 ?x))
                             (loop-for-count (?y 0 320) do
                                             (bind ?y-end
                                                   (- 320 ?y))
                                             (display:draw-rect ?x ?y
                                                                ?x-end 
                                                                ?y-end
                                                                (nth$ (+ 1 ?y) 
                                                                      ?colors)))))
(deffunction fill-rect-test 
             ()
             (printout t "fill rect test!" crlf)
             (clear-screen)
             (bind ?colors
                   (generate-random-color-selection 324))
             (loop-for-count (?x 0 240) do
                             (bind ?x-end 
                                   (- 240 ?x))
                             (loop-for-count (?y 0 320) do
                                             (bind ?y-end
                                                   (- 320 ?y))
                                             (display:fill-rect ?x ?y
                                                                ?x-end 
                                                                ?y-end
                                                                (nth$ (+ 1 ?y)
                                                                      ?colors)))))
(deffunction draw-circle-test 
             ()
             (printout t "draw circle test!" crlf)
             (clear-screen)
             (bind ?colors
                   (generate-random-color-selection 324))
             (loop-for-count (?radius 16 (random 17 64)) do
                             (display:draw-circle (random 0 240)
                                                  (random 0 320)
                                                  ?radius
                                                  (nth$ (random 0 320) 
                                                        ?colors))))

(deffunction fill-circle-test 
             ()
             (printout t "fill circle test!" crlf)
             (clear-screen)
             (bind ?colors
                   (generate-random-color-selection 128))
             (loop-for-count (?radius 16 (random 17 64)) do
                             (display:fill-circle (random 0 240)
                                                  (random 0 320)
                                                  ?radius
                                                  (nth$ (random 0 128) 
                                                        ?colors))))

(deffunction pixel-test
             ()
             (printout t "pixel test!" crlf)
             (clear-screen)
             (bind ?colors
                   (generate-random-color-selection 324))
             (loop-for-count (?x 0 240) do
                             (loop-for-count (?y 0 320) do
                                             (display:draw-pixel ?x 
                                                                 ?y
                                                                 (nth$ (+ 1 ?y)
                                                                       ?colors)))))
