;maya
;Copyright (c) 2012-2023, Joshua Scoggins
;All rights reserved.
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions are met:
;    * Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;    * Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
;ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
(include cortex/main-decl.clp)
(include cortex/has-parent.clp)
(include cortex/has-title.clp)
(include cortex/has-description.clp)
(include cortex/container.clp)
(include cortex/stage.clp)

(defclass atomic-value
  (is-a has-parent)
  (slot kind
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot value
        (storage local)
        (visibility public)
        (default ?NONE)))

(defclass file-container
  (is-a container)
  (slot file-name
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE)))



(defclass parser
  (is-a USER)
  (slot path
        (type LEXEME)
        (storage local)
        (visibility public)
        (access initialize-only)
        (default ?NONE))
  (slot id
        (type SYMBOL)
        (storage local)
        (visibility public)
        (access read-only)
        (default-dynamic (gensym*)))
  (slot top-element
        (type INSTANCE)
        (storage local)
        (visibility public))
  (slot current-element
        (type INSTANCE)
        (storage local)
        (visibility public))
  (slot valid
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public))
  (slot parsed 
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public))
  (slot parsing 
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public))
  (multislot current-token
             (storage local)
             (visibility public))
  (message-handler init after))
(defmessage-handler parser init after
                    ()
                    (bind ?self:valid
                          (open ?self:path
                                ?self:id
                                "r"))
                    (bind ?self:parsing
                          ?self:valid)
                    (bind ?self:top-element
                          (make-instance of file-container
                                         (parent FALSE)
                                         (file-name ?self:path)))
                    (bind ?self:current-element
                          ?self:top-element))

(deffunction MAIN::request-file-parse
             (?path)
             (assert (parse file ?path)))

(deffacts parsing-stages
          (stage (current generate-files)
                 (rest process-file
                       sanity-check
                       hoisting
                       identify-structures)))
(defrule generate-file-container
         (stage (current generate-files))
         ?f <- (parse file ?path)
         =>
         (retract ?f)
         (make-instance of parser
                        (path ?path)))

(defrule read-token
         (stage (current process-file))
         ?f <- (object (is-a parser)
                       (current-token)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id))
         =>
         (modify-instance ?f 
                          (current-token (next-token ?id))))

(defrule stop-parsing-file
         "If we see a stop token then just terminate immediately!"
         (stage (current process-file))
         ?f <- (object (is-a parser)
                       (current-token STOP ?)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id))
         =>
         (close ?id)
         (modify-instance ?f 
                          (current-token)
                          (parsing FALSE)
                          (parsed TRUE)))

(defrule make-new-target-container
         (stage (current process-file))
         ?f <- (object (is-a parser)
                       (current-token LEFT_PARENTHESIS ?)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id)
                       (current-element ?target))
         ?k <- (object (is-a container)
                       (name ?target)
                       (contents $?prior))
         =>
         (bind ?ncurr
               (make-instance of container
                              (parent ?target)))
         (modify-instance ?f 
                          (current-token)
                          (current-element ?ncurr))
         (modify-instance ?k
                          (contents ?prior
                                    ?ncurr)))
(defrule leave-current-element:valid
         (stage (current process-file))
         ?f <- (object (is-a parser)
                       (current-token RIGHT_PARENTHESIS ?)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id)
                       (current-element ?target))
         (object (is-a container)
                 (name ?target)
                 (parent ?parent&~FALSE))
         =>
         ; at this point, if we want to actually send this element off for processing then it would make a lot of sense!
         (modify-instance ?f
                          (current-token)
                          (current-element ?parent)))

(defrule leave-current-element:invalid-no-parent
         (stage (current process-file))
         ?f <- (object (is-a parser)
                       (current-token RIGHT_PARENTHESIS $?)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id)
                       (path ?path)
                       (current-element ?target)
                       (name ?name))
         (object (is-a container)
                 (name ?target)
                 (parent FALSE))
         =>
         (printout stderr
                   "ERROR: mismatched parens, found a right paren without a matching left paren" crlf
                   "Target file: " ?path crlf
                   "Target parser: " ?name crlf)
         (halt))
(defrule found-lparen-mismatch-at-end
         (stage (current sanity-check))
         ?f <- (object (is-a parser)
                       (parsing FALSE)
                       (valid TRUE)
                       (parsed TRUE)
                       (path ?path)
                       (top-element ?top)
                       (current-element ?v&~?top)
                       (name ?name))
         =>
         (printout stderr
                   "ERROR: mismatched parens, found a left paren without a matching right paren after finishing parsing" crlf
                   "Target File: " ?path crlf
                   "Target parser: " ?name crlf
                   "Mismatched container: " ?v crlf)
         (halt))

(defrule make-atomic-value 
         (stage (current process-file))
         ?f <- (object (is-a parser)
                       (current-token ?kind&~LEFT_PARENTHESIS&~RIGHT_PARENTHESIS&~STOP ?value)
                       (parsing TRUE)
                       (valid TRUE)
                       (parsed FALSE)
                       (id ?id)
                       (current-element ?target))
         ?k <- (object (is-a container)
                       (name ?target)
                       (contents $?prior))
         =>
         (modify-instance ?f 
                          (current-token))
         (modify-instance ?k
                          (contents ?prior
                                    (make-instance of atomic-value
                                                   (parent ?target)
                                                   (kind ?kind)
                                                   (value ?value)))))

