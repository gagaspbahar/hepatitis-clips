; Node structure for the decision tree
(deftemplate node
  (slot name)
  (slot question)
  (slot positive-node)
  (slot negative-node)
  (slot final-answer)
)

(deffacts tree
  (node (name HBsAg-positive) (question "HBs Ag positive?") (positive-node anti-HDV-negative) (negative-node anti-HBs-positive-1))
  (node (name anti-HBs-positive-1) (question "Anti-HBs positive?") (positive-node anti-HBc-positive-1) (negative-node anti-HBc-positive-2))
  (node (name anti-HBc-positive-1) (question "Anti-HBc positive?") (positive-node cured) (negative-node vaccinated))
  (node (name anti-HBc-positive-2) (question "Anti-HBc positive?") (positive-node unclear) (negative-node healthy))
  (node (name cured) (final-answer "Cured"))
  (node (name vaccinated) (final-answer "Vaccinated"))
  (node (name healthy) (final-answer "Healthy not vaccinated or suspicious"))
  (node (name unclear) (final-answer "Unclear (possible resolved)"))
  

  (node (name anti-HDV-negative) (question "Anti-HDV negative?") (positive-node hepatitis) (negative-node anti-HBc-positive-3))
  (node (name anti-HBc-positive-3) (question "Anti-HBc positive?") (positive-node anti-HBs-positive-2) (negative-node uncertain))
  (node (name anti-HBs-positive-2) (question "Anti-HBs positive?") (positive-node uncertain) (negative-node lgm-anti-HBc-positive))
  (node (name lgm-anti-HBc-positive) (question "lgm Anti-HBc positive?") (positive-node acute) (negative-node chronic))
  (node (name hepatitis) (final-answer "Hepatitis B+D"))
  (node (name uncertain) (final-answer "Uncertain configuration"))
  (node (name acute) (final-answer "Acute infection"))
  (node (name chronic) (final-answer "Chronic infection"))
)

(defrule initialize
  (and (not (exists (current-node ?))) (not (exists(final-answer ?))))
=> 
  (assert (current-node HBsAg-positive))
)

(deffunction ask (?question)
  (printout t ?question " (positive or negative) " crlf)
  (bind ?answer (read))
  (while (and (not (eq ?answer positive))
              (not (eq ?answer negative)))
    (printout t "Please answer positive or negative" crlf)
    (bind ?answer (read)))
  (return ?answer)
)

(defrule ask-current-node-question 
  ?node <- (current-node ?name)
  (node (name ?name)
        (question ?question)
        (positive-node ~nil)
        (negative-node ~nil))
  (not (answer ?))
  =>
  (assert (answer (ask ?question)))
)


(defrule proceed-to-positive
  ?node <- (current-node ?name)
  (node (name ?name)
        (question ?question)
        (positive-node ?positive-node)
  )
  ?answer <- (answer positive)
  =>
  (retract ?node ?answer)
  (assert (current-node ?positive-node))
)

(defrule proceed-to-negative
  ?node <- (current-node ?name)
  (node (name ?name)
        (question ?question)
        (negative-node ?negative-node)
  )
  ?answer <- (answer negative)
  =>
  (retract ?node ?answer)
  (assert (current-node ?negative-node))
)

(defrule final-answer
  (declare (salience 10))
  ?node <- (current-node ?name)
  (node (name ?name)
        (final-answer ?answer)
        (question nil)
        (positive-node nil)
        (negative-node nil)
  )
  =>
  (assert (final-answer ?answer))
  (printout t "The final answer is " ?answer crlf)
  (retract ?node)
)