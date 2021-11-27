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
             (bind ?colors (create$))
             (loop-for-count (?x 0 ?count) do
                             (bind ?colors
                                   ?colors
                                   (display:color565 (random 0 255)
                                                     (random 0 255)
                                                     (random 0 255))))
             ?colors)
(deffunction draw-rect-test
             ()
             (bind ?colors
                   (generate-random-number-selection 324))
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
             (bind ?colors
                   (generate-random-number-selection 324))
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
(deffunction circle-overlay0
             ()
             (bind ?colors
                   (generate-random-number-selection 324))
             (loop-for-count (?radius 16 (random 17 64)) do
                             (display:draw-circle (random 0 240)
                                                  (random 0 320)
                                                  ?radius
                                                  (nth$ (random 0 320) 
                                                        ?colors))))

(deffunction pixel-test
             ()
             (bind ?colors
                   (generate-random-number-selection 324))
             (loop-for-count (?x 0 240) do
                             (loop-for-count (?y 0 320) do
                                             (display:draw-pixel ?x 
                                                                 ?y
                                                                 (nth$ (+ 1 ?y)
                                                                       ?colors)))))
