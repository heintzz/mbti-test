(defglobal ?*ie-file* = IETEST.txt)
(defglobal ?*sn-file* = SNTEST.txt)
(defglobal ?*tf-file* = TFTEST.txt)
(defglobal ?*jp-file* = JPTEST.txt)

(deffunction read-from-ie-file (?file)
    (open ?file questions)
      
    (bind ?stop FALSE) 
    (bind ?ie-score 0)

    (while (not ?stop)
        (bind ?valid FALSE)  
        (bind ?text (readline questions)) 
        (bind ?cat (sub-string 0 3 ?text))
        (bind ?question (sub-string 5 (length ?text) ?text))
        (if (eq ?text EOF) 
            then (bind ?stop TRUE) 
            (printout t crlf)
        else 
        (while (not ?valid)          
          (printout t ?question)        
          (bind ?answer (read))     
          (if (eq ?answer yes)
            then 
            (bind ?ie-score (if (eq ?cat "EXT") 
                                then (+ ?ie-score 1) 
                                else (- ?ie-score 1)))
            (bind ?valid TRUE)            
            else if (eq ?answer no)
              then 
              (bind ?ie-score (if (eq ?cat "EXT") 
                                  then (- ?ie-score 1) 
                                  else (+ ?ie-score 1)))
              (bind ?valid TRUE)
          )
          (if (and (neq ?answer yes) (neq ?answer no))
            then (printout t "Invalid response. Please enter 'yes' or 'no'." crlf)
            (printout t ?question)  
             (bind ?answer (read))     
          )
        ))        
    )   

    (close)
    (assert (IESCORE ?ie-score))
    (assert (analysis sn))
    (printout t "ISTIRAHAT DULU GAK SIH" crlf crlf)
)

(deffunction read-from-sn-file (?file)
    (open ?file questions)
      
    (bind ?stop FALSE) 
    (bind ?sn-score 0)

    (while (not ?stop)
        (bind ?valid FALSE)  
        (bind ?text (readline questions)) 
        (bind ?cat (sub-string 0 3 ?text))
        (bind ?question (sub-string 5 (length ?text) ?text))
        (if (eq ?text EOF) 
            then (bind ?stop TRUE) 
            (printout t crlf)
        else 
        (while (not ?valid)          
          (printout t ?question)        
          (bind ?answer (read))     
          (if (eq ?answer yes)
            then 
            (bind ?sn-score (if (eq ?cat "SNS") 
                                then (+ ?sn-score 1) 
                                else (- ?sn-score 1)))
            (bind ?valid TRUE)
            else if (eq ?answer no)
              then 
              (bind ?sn-score (if (eq ?cat "SNS") 
                                  then (- ?sn-score 1) 
                                  else (+ ?sn-score 1)))
              (bind ?valid TRUE)
          )
          (if (and (neq ?answer yes) (neq ?answer no))
            then (printout t "Invalid response. Please enter 'yes' or 'no'." crlf)
            (printout t ?question)  
             (bind ?answer (read))     
          )
        ))        
    )   

    (close)
    (assert (SNSCORE ?sn-score))
    (printout t "ISTIRAHAT DULU GAK SIH" crlf crlf)
    (assert (analysis tf))
)

(deffunction read-from-tf-file (?file)
    (open ?file questions)
      
    (bind ?stop FALSE) 
    (bind ?tf-score 0)

    (while (not ?stop)
        (bind ?valid FALSE)  
        (bind ?text (readline questions)) 
        (bind ?cat (sub-string 0 3 ?text))
        (bind ?question (sub-string 5 (length ?text) ?text))
        (if (eq ?text EOF) 
            then (bind ?stop TRUE) 
            (printout t crlf)   
        else 
        (while (not ?valid)          
          (printout t ?question)        
          (bind ?answer (read))  
          (if (eq ?answer yes)
            then 
            (bind ?tf-score (if (eq ?cat "TNK") 
                                then (+ ?tf-score 1) 
                                else (- ?tf-score 1)))
            (bind ?valid TRUE)
            else if (eq ?answer no)
              then 
              (bind ?tf-score (if (eq ?cat "TNK") 
                                  then (- ?tf-score 1) 
                                  else (+ ?tf-score 1)))
              (bind ?valid TRUE)
          )
          (if (and (neq ?answer yes) (neq ?answer no))
            then (printout t "Invalid response. Please enter 'yes' or 'no'." crlf)
            (printout t ?question)  
             (bind ?answer (read))     
          )
        ))        
    )   

    (close)
    (assert (TFSCORE ?tf-score))
    (printout t "ISTIRAHAT DULU GAK SIH" crlf crlf)
    (assert (analysis jp))
)

(deffunction read-from-jp-file (?file)
    (open ?file questions)
      
    (bind ?stop FALSE) 
    (bind ?jp-score 0)

    (while (not ?stop)
        (bind ?valid FALSE)  
        (bind ?text (readline questions)) 
        (bind ?cat (sub-string 0 3 ?text))
        (bind ?question (sub-string 5 (length ?text) ?text))
        (if (eq ?text EOF) 
            then (bind ?stop TRUE) 
            (printout t crlf)   
        else 
        (while (not ?valid)          
          (printout t ?question)        
          (bind ?answer (read))  
          (if (eq ?answer yes)
            then 
            (bind ?jp-score (if (eq ?cat "JDG") 
                                then (+ ?jp-score 1) 
                                else (- ?jp-score 1)))
            (bind ?valid TRUE)
            else if (eq ?answer no)
              then 
              (bind ?jp-score (if (eq ?cat "JDG") 
                                  then (- ?jp-score 1) 
                                  else (+ ?jp-score 1)))
              (bind ?valid TRUE)
          )
          (if (and (neq ?answer yes) (neq ?answer no))
            then (printout t "Invalid response. Please enter 'yes' or 'no'." crlf)
            (printout t ?question)  
             (bind ?answer (read))     
          )
        ))        
    )   

    (close)
    (assert (JPSCORE ?jp-score))
)

; Introvert-Extrovert
(defrule infer-extrovert
  ?score <- (IESCORE ?ie-score&:(> ?ie-score 0))
  =>
  (assert (extrovert TRUE))
  (retract ?score)
)

(defrule infer-introvert
  ?score <- (IESCORE ?ie-score&:(< ?ie-score 0))
  =>  
  (assert (introvert TRUE))
  (retract ?score)
)

(defrule introvert-extrovert
  ?score <- (IESCORE ?ie-score&:(eq ?ie-score 0))
  =>  
  (assert (extrovert TRUE))
  (assert (introvert TRUE))
  (retract ?score)
)

; Sensing-Intuitive
(defrule infer-sensing
  ?score <- (SNSCORE ?sn-score&:(> ?sn-score 0))
  =>
  (assert (sensing TRUE))
  (retract ?score)
)

(defrule infer-intuitive
  ?score <- (SNSCORE ?sn-score&:(< ?sn-score 0))
  =>  
  (assert (intuitive TRUE))
  (retract ?score)
)

(defrule sensing-intuitive
  ?score <- (SNSCORE ?sn-score&:(eq ?sn-score 0))
  =>  
  (assert (sensing TRUE))
  (assert (intuitive TRUE))
  (retract ?score)
)

; Thinkers-Feelings
(defrule infer-thinkers
  ?score <- (TFSCORE ?tf-score&:(> ?tf-score 0))
  =>
  (assert (thinkers TRUE))
  (retract ?score)
)

(defrule infer-feelings
  ?score <- (TFSCORE ?tf-score&:(< ?tf-score 0))
  =>  
  (assert (feelings TRUE))
  (retract ?score)
)

(defrule thinkers-feelings
  ?score <- (TFSCORE ?tf-score&:(eq ?tf-score 0))
  =>  
  (assert (thinkers TRUE))
  (assert (feelings TRUE))
  (retract ?score)
)

; Judging-Perceivers
(defrule infer-judging
  ?score <- (JPSCORE ?jp-score&:(> ?jp-score 0))
  =>
  (assert (judging TRUE))
  (retract ?score)
)

(defrule infer-perceivers
  ?score <- (JPSCORE ?jp-score&:(< ?jp-score 0))
  =>  
  (assert (perceivers TRUE))
  (retract ?score)
)

(defrule judging-perceivers
  ?score <- (JPSCORE ?jp-score&:(eq ?jp-score 0))
  =>  
  (assert (judging TRUE))
  (assert (perceivers TRUE))
  (retract ?score)
)

; INTROVERT
(defrule infer-istj
  (introvert TRUE)
  (sensing TRUE)
  (thinkers TRUE)
  (judging TRUE)
=>
  (printout t "ISTJ" crlf)
)

(defrule infer-isfj
  (introvert TRUE)
  (sensing TRUE)
  (feelings TRUE)
  (judging TRUE)
=>
  (printout t "ISFJ" crlf)
)

(defrule infer-infj
  (introvert TRUE)
  (intuitive TRUE)
  (feelings TRUE)
  (judging TRUE)
=>
  (printout t "INFJ" crlf)
)

(defrule infer-intj
  (introvert TRUE)
  (intuitive TRUE)
  (thinkers TRUE)
  (judging TRUE)
=>
  (printout t "INTJ" crlf)
)

(defrule infer-istp
  (introvert TRUE)
  (sensing TRUE)
  (thinkers TRUE)
  (perceivers TRUE)
=>
  (printout t "ISTP" crlf)
)

(defrule infer-isfp
  (introvert TRUE)
  (sensing TRUE)
  (feelings TRUE)
  (perceivers TRUE)
=>
  (printout t "ISFP" crlf)
)

(defrule infer-infp
  (introvert TRUE)
  (intuitive TRUE)
  (feelings TRUE)
  (perceivers TRUE)
=>
  (printout t "INFP" crlf)
)

(defrule infer-intp
  (introvert TRUE)
  (intuitive TRUE)
  (thinkers TRUE)
  (perceivers TRUE)
=>
  (printout t "INTP" crlf)
)

; EXTROVERT
(defrule infer-estj
  (extrovert TRUE)
  (sensing TRUE)
  (thinkers TRUE)
  (judging TRUE)
=>
  (printout t "ESTJ" crlf)
)

(defrule infer-esfj
  (extrovert TRUE)
  (sensing TRUE)
  (feelings TRUE)
  (judging TRUE)
=>
  (printout t "ESFJ" crlf)
)

(defrule infer-enfj
  (extrovert TRUE)
  (intuitive TRUE)
  (feelings TRUE)
  (judging TRUE)
=>
  (printout t "ENFJ" crlf)
)

(defrule infer-entj
  (extrovert TRUE)
  (intuitive TRUE)
  (thinkers TRUE)
  (judging TRUE)
=>
  (printout t "ENTJ" crlf)
)

(defrule infer-estp
  (extrovert TRUE)
  (sensing TRUE)
  (thinkers TRUE)
  (perceivers TRUE)
=>
  (printout t "ESTP" crlf)
)

(defrule infer-esfp
  (extrovert TRUE)
  (sensing TRUE)
  (feelings TRUE)
  (perceivers TRUE)
=>
  (printout t "ESFP" crlf)
)

(defrule infer-enfp
  (extrovert TRUE)
  (intuitive TRUE)
  (feelings TRUE)
  (perceivers TRUE)
=>
  (printout t "ENFP" crlf)
)

(defrule infer-entp
  (extrovert TRUE)
  (intuitive TRUE)
  (thinkers TRUE)
  (perceivers TRUE)
=>
  (printout t "INTP" crlf)
)
(defrule start-program 
  (declare (salience 1))
=> 
  (read-from-ie-file ?*ie-file*)
)

(defrule sn-test
  (analysis sn)
=>
  (read-from-sn-file ?*sn-file*)
)

(defrule tf-test
  (analysis tf)
=>
  (read-from-tf-file ?*tf-file*)
)

(defrule jp-test
  (analysis jp)
=>
  (read-from-jp-file ?*jp-file*)
)