;maya
;Copyright (c) 2012-2025, Joshua Scoggins
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
; A CLIPS parser written in CLIPS
(defmodule MAIN
           (export ?ALL))
(deffunction MAIN::begin
             ()

             )

; core concepts
(defclass MAIN::has-parent
  "allows a parent/child relationship from the child to the parent"
  (is-a USER)
  (slot parent
        (type INSTANCE
              SYMBOL)
        (allowed-symbols FALSE)
        (storage local)
        (visibility public)))
(defclass MAIN::has-children
  "allows a parent/child relationship from the parent to its children"
  (is-a USER)
  (multislot children
             (storage local)
             (visibility public)))
; The next thing to define is the idea of an "expression"
(defclass MAIN::expression
  "An expression is a collection of things that has a parent/child relationship"
  (is-a has-parent
        has-children))

; A translation unit is a special case of an expression
(defclass MAIN::translation-unit
  "A special case of an expression where the parent is always false"
  (is-a expression)
  (slot path
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot parent
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default FALSE)))


(defclass MAIN::atom
  "If it isn't an expression, then it is an atom"
  (is-a has-parent)
  (slot value
        (storage local)
        (visibility public)
        (default ?NONE)))

; now define the parser
(defclass MAIN::parser
  (is-a has-parent) ; in this case, the parent is the translation unit
  (slot router-id
        (type SYMBOL)
        (storage local)
        (visibility public)
        (access initialize-only)
        (default-dynamic (gensym*)))
  (slot path
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
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
(defmessage-handler MAIN::parser init after
                    ()
                    (bind ?self:parent
                          (bind ?self:current-element
                                (make-instance of translation-unit
                                               (path ?self:path))))
                    (bind ?self:parsing
                          (bind ?self:valid
                                (open ?self:path
                                      ?self:id
                                      "r"))))






(deftemplate MAIN::parse-request
             (slot path
                   (type LEXEME)
                   (default ?NONE)))

(deftemplate MAIN::stage
             (slot current
                   (type SYMBOL)
                   (default ?NONE))
             (multislot rest
                        (type SYMBOL)
                        (default ?NONE)))

(deffacts MAIN::parse-stages
          (stage (current startup)
                 (rest generate-translation-unit
                       parse-translation-unit)))
(defrule MAIN::next-stage
         (declare (salience -10000))
         ?f <- (stage (rest ?next $?rest))
         =>
         (modify ?f
                 (current ?next)
                 (rest ?rest)))

(defrule MAIN::construct-translation-unit
         (stage (current generate-translation-unit))
         ?f <- (parse-request (path ?path))
         =>
         (retract ?f)
         (make-instance of parser
                        (path ?path)))





