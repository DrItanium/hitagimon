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

