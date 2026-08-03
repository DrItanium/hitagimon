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

(defclass top-level-expression
  (is-a has-title
        has-description))
(defclass defrule-expression
  (is-a top-level-expression)
  (slot declare 
        (type SYMBOL
              INSTANCE)
        (allowed-symbols FALSE)
        (storage local)
        (visibility public))
  (slot salience
        (type INTEGER
              INSTANCE)
        (range -10000 10000)
        (storage local)
        (visibility public)
        (default-dynamic 0))
  (slot auto-focus 
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE)
        (storage local)
        (visibility public)
        (default-dynamic FALSE))
  (multislot left-hand-side
             (storage local)
             (visibility public))
  (multislot right-hand-side
             (storage local)
             (visibility public)))

(defclass deffunction-expression 
  (is-a top-level-expression)
  (slot arguments
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot body
             (storage local)
             (visibility public)))
(defclass defclass-expression 
  (is-a top-level-expression)
  (multislot inherits
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot role
        (type SYMBOL)
        (allowed-symbols concrete
                         abstract)
        (storage local)
        (visibility public))
  (slot pattern-match
        (type SYMBOL)
        (allowed-symbols reactive
                         non-reactive)
        (storage local)
        (visibility public))
  (multislot body
             (storage local)
             (visibility public)))

(defclass deftemplate-expression 
  (is-a top-level-expression)
  (multislot body
             (storage local)
             (visibility public)))
(defclass slot-expression 
  (is-a has-title
        has-parent)
  (role abstract)
  (pattern-match non-reactive)
  (slot visibility
        (type SYMBOL)
        (allowed-values private
                        public)
        (storage local)
        (visibility public))
  (slot storage 
        (type SYMBOL)
        (allowed-values local 
                        shared)
        (storage local)
        (visibility public))
  (multislot facets
             (storage local)
             (visibility public)
             (default ?NONE)))
(defclass single-slot-expression 
  (is-a slot-expression)
  (role concrete)
  (pattern-match reactive))
(defclass multislot-expression 
  (is-a slot-expression)
  (role concrete)
  (pattern-match reactive))
(defclass assigned-conditional-element
  (is-a has-parent)
  (slot parent
        (source composite)
        (default ?NONE))
  (slot alias
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot pattern-ce
        (storage local)
        (visibility public)
        (default ?NONE)))

(defclass assertion-statement
  (is-a container))

(defclass fact-assertion
  (is-a has-parent)
  (slot first
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot rest 
             (visibility public)
             (storage local)))

(defclass defgeneric-expression
  (is-a top-level-expression))

(defrule generate-defrule
         (stage (current identify-structures))
         ?f <- (object (is-a container)
                       (parent ~FALSE)
                       (name ?name)
                       (contents defrule ?rule-name $?lhs => $?rhs))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defrule-expression
                        (title ?rule-name)
                        (description "")
                        (left-hand-side ?lhs)
                        (right-hand-side ?rhs)))
(defrule move-doc-string-out
         (stage (current identify-structures))
         ?f <- (object (is-a defrule-expression)
                       (left-hand-side ?first $?rest))
         ?k <- (object (is-a atomic-value)
                       (name ?first)
                       (kind STRING)
                       (value ?doc-string))
         =>
         (modify-instance ?f
                          (left-hand-side ?rest)
                          (description ?doc-string))
         (unmake-instance ?k))
(defrule move-expression-out-of-rule-body
         (stage (current identify-structures))
         ?f <- (object (is-a defrule-expression)
                       (name ?rule)
                       (declare FALSE)
                       (left-hand-side ?declaration $?rest))
         (object (is-a container)
                 (name ?declaration)
                 (contents declare
                           $?))
         =>
         (modify-instance ?f
                          (declare ?declaration)
                          (left-hand-side $?rest)))
(defrule record-salience-for-rule
         (stage (current identify-structures))
         ?f <- (object (is-a defrule-expression)
                       (name ?rule)
                       (declare ?declaration))
         (object (is-a container)
                 (name ?declaration)
                 (contents declare
                           $?
                           ?salience
                           $?))
         (object (is-a container)
                 (name ?salience)
                 (contents salience
                           ?value))
         =>
         (modify-instance ?f
                          (salience ?value)))

(defrule record-auto-focus-for-rule
         (stage (current identify-structures))
         ?f <- (object (is-a defrule-expression)
                       (name ?rule)
                       (declare ?declaration))
         (object (is-a container)
                 (name ?declaration)
                 (contents declare
                           $?
                           ?auto-focus
                           $?))
         (object (is-a container)
                 (name ?auto-focus)
                 (contents auto-focus
                           ?value))
         =>
         (modify-instance ?f
                          (auto-focus ?value)))

(defrule generate-deffunction-with-docstring
         (stage (current identify-structures))
         ?f <- (object (is-a container)
                       (parent ~FALSE)
                       (name ?name)
                       (contents deffunction
                                 ?func-name
                                 ?doc-string
                                 ?args
                                 $?body))
         ?f4 <- (object (is-a atomic-value)
                        (name ?doc-string)
                        (kind STRING)
                        (value ?str))
         (object (is-a container)
                 (name ?args))
         =>
         (unmake-instance ?f ?f4)
         (make-instance ?name of deffunction-expression
                        (title ?func-name)
                        (description ?str)
                        (arguments ?args)
                        (body $?body)))

(defrule generate-deffunction-without-docstring
         (stage (current identify-structures))
         ?f <- (object (is-a container)
                       (parent ~FALSE)
                       (name ?name)
                       (contents deffunction
                                 ?func-name
                                 ?args
                                 $?body))
         (object (is-a container)
                 (name ?args))
         =>
         (unmake-instance ?f)
         (make-instance ?name of deffunction-expression
                        (title ?func-name)
                        (description "")
                        (arguments ?args)
                        (body $?body)))



(defrule generate-defclass-decl-without-docstring
         (stage (current identify-structures))
         ?f <- (object (is-a container)
                       (parent ~FALSE)
                       (name ?name)
                       (contents defclass
                                 ?class-name
                                 ?is-a
                                 $?rest))
         ?f2 <- (object (is-a container)
                        (name ?is-a)
                        (contents is-a 
                                  $?kinds))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?name of defclass-expression
                        (title ?class-name)
                        (description "")
                        (inherits $?kinds)
                        (body ?rest)))

(defrule generate-defclass-decl-with-docstring
         (stage (current identify-structures))
         ?f <- (object (is-a container)
                       (parent ~FALSE)
                       (name ?name)
                       (contents defclass
                                 ?class-name
                                 ?docstr
                                 ?is-a
                                 $?rest))
         ?f3 <- (object (is-a atomic-value)
                        (name ?docstr)
                        (kind STRING)
                        (value ?str))
         ?f2 <- (object (is-a container)
                        (name ?is-a)
                        (contents is-a 
                                  $?kinds))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (make-instance ?name of defclass-expression
                        (description ?str)
                        (title ?class-name)
                        (inherits $?kinds)
                        (body ?rest)))

(defrule assume-class-role
         (stage (current identify-structures))
         ?f <- (object (is-a defclass-expression)
                       (body ?first
                             $?rest))
         ?f2 <- (object (is-a container)
                        (name ?first)
                        (contents role ?role))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f
                          (role ?role)
                          (body $?rest)))


(defrule assume-class-pattern-match
         (stage (current identify-structures))
         ?f <- (object (is-a defclass-expression)
                       (body ?first
                             $?rest))
         ?f2 <- (object (is-a container)
                        (name ?first)
                        (contents pattern-match ?role))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f
                          (pattern-match ?role)
                          (body $?rest)))



(defrule generate-deftemplate-decl-without-docstring
         (stage (current identify-structures))
         ?f <- (object (is-a container)
                       (parent ~FALSE)
                       (name ?name)
                       (contents deftemplate
                                 ?class-name
                                 $?rest))
         =>
         (unmake-instance ?f)
         (make-instance ?name of deftemplate-expression
                        (title ?class-name)
                        (description "")
                        (body ?rest)))

(defrule generate-deftemplate-decl-with-docstring
         (stage (current identify-structures))
         ?f <- (object (is-a container)
                       (parent ~FALSE)
                       (name ?name)
                       (contents deftemplate
                                 ?class-name
                                 ?docstr
                                 $?rest))
         ?f3 <- (object (is-a atomic-value)
                        (name ?docstr)
                        (kind STRING)
                        (value ?str))
         =>
         (unmake-instance ?f ?f3)
         (make-instance ?name of deftemplate-expression
                        (description ?str)
                        (title ?class-name)
                        (body ?rest)))

(defrule associate-parent-class-types
         (stage (current identify-structures))
         ?f <- (object (is-a defclass-expression)
                       (inherits $?a ?curr $?b))
         (object (is-a defclass-expression)
                 (title ?curr)
                 (name ?name))
         =>
         (modify-instance ?f 
                          (inherits ?a ?name ?b)))


(defrule generate-single-field-slot
         (stage (current identify-structures))
         ?f <- (object (is-a container)
                       (parent ?parent&~FALSE)
                       (name ?name)
                       (contents slot|single-slot
                                 ?title
                                 $?facets))
         =>
         (unmake-instance ?f)
         (make-instance ?name of single-slot-expression
                        (parent ?parent)
                        (title ?title)
                        (facets $?facets)))

(defrule generate-multislot 
         (stage (current identify-structures))
         ?f <- (object (is-a container)
                       (parent ?parent&~FALSE)
                       (name ?name)
                       (contents multislot 
                                 ?title
                                 $?facets))
         =>
         (unmake-instance ?f)
         (make-instance ?name of multislot-expression 
                        (parent ?parent)
                        (title ?title)
                        (facets $?facets)))



(defrule associate-single-slot-facet:storage
         (stage (current identify-structures))
         ?f <- (object (is-a slot-expression)
                       (name ?name)
                       (facets $?a ?facet $?b))
         ?f2 <- (object (is-a container)
                        (name ?facet)
                        (contents storage ?kind))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f
                          (storage ?kind)
                          (facets $?a $?b)))

(defrule associate-single-slot-facet:visibility
         (stage (current identify-structures))
         ?f <- (object (is-a slot-expression)
                       (name ?name)
                       (facets $?a ?facet $?b))
         ?f2 <- (object (is-a container)
                        (name ?facet)
                        (contents visibility ?kind))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f
                          (visibility ?kind)
                          (facets $?a $?b)))


(defrule make-assigned-conditional-element
         (stage (current identify-structures))
         ?f <- (object (is-a defrule-expression)
                       (name ?name)
                       (left-hand-side $?a ?b <- ?c $?d))
         ?f2 <- (object (is-a atomic-value)
                        (name ?b)
                        (kind SF_VARIABLE)
                        (value ?value))
         ?f3 <- (object (is-a container)
                        (name ?c)
                        (parent ?name))
         =>
         (unmake-instance ?f2)
         (bind ?container
               (make-instance of assigned-conditional-element
                              (parent ?name)
                              (alias ?value)
                              (pattern-ce ?c)))
         (modify-instance ?f3 
                          (parent (instance-name ?container)))
         (modify-instance ?f 
                          (left-hand-side ?a 
                                          ?container
                                          ?d)))



(defrule detect-assert-statements
         (stage (current identify-structures))
         ?f <- (object (is-a container)
                       (contents assert 
                                 $?containers)
                       (name ?stmt)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?stmt of assertion-statement
                        (parent ?parent)
                        (contents $?containers)))

(defrule construct-fact-assertion-detection
         (stage (current identify-structures))
         (object (is-a assertion-statement)
                 (contents $? ?container $?))
         ?k <- (object (is-a container)
                       (name ?container)
                       (contents ?first $?rest)
                       (parent ?parent))
         =>
         (unmake-instance ?k)
         (make-instance ?container of fact-assertion
                        (parent ?parent)
                        (first ?first)
                        (rest $?rest)))


(defrule construct-defgeneric-with-doc-string
         (stage (current identify-structures))
         ?f <- (object (is-a container)
                       (name ?name)
                       (contents defgeneric ?title 
                                 ?str))
         ?f2 <- (object (is-a atomic-value)
                        (name ?str)
                        (kind STRING)
                        (value ?value))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?name of defgeneric-expression
                        (title ?title)
                        (description ?value)))



(defrule construct-defgeneric-without-doc-string
         (stage (current identify-structures))
         ?f <- (object (is-a container)
                       (name ?name)
                       (contents defgeneric ?title))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defgeneric-expression
                        (title ?title)))


(defclass deftemplate-expression
  (is-a top-level-expression)
  (multislot slots
             (type INSTANCE)
             (storage local)
             (visibility public)))

(defclass ordered-fact
  (is-a container)
  (slot first
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot rest 
             (visibility public)
             (storage local)))

