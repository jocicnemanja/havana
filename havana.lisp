(defun globalVariableInit ()
      (setq vrednost_cvora '())
      (setq selected-node '())
      (setq stack '())
      (setq searchedFields '())
      (setq nextToPlay t)
      (setq gameState '())
      (setq row '())
      (setq column '())
      (setq listOfBoardEdges '())
      (setq matrixDimensions '()))

(defun printSign (list)
          (cond ((null list) '() )
            ((equalp (car list) 1000) (format t "  " ))
            ((equalp (car list) 1100 ) (format t " -  " ))
            ((equalp (car list) 1110 ) (format t " O  " ))
            ((equalp (car list) 1111 ) (format t " X  " ))
            (t(princ (code-char (car list ) )))))

(defun printRow (l)
      (cond ((null l) '())
            (t (printRow (cdr l)) (printSign (list (car l)))   )))

(defun printGameBoard ( asci-counter n m)
(cond ((= asci-counter (+ 65 n)) '())
      (t(printRow (reverse (assoc asci-counter m ))) (format t "~%")(printGameBoard (1+ asci-counter) n m) )))
;----------------------------------------------------------------------------

;Dynamic creation of matrix
;----------------------------------------------------------------------------
(defun createColumn (asciSign matrixDimensions number-of-blanko)
      (append (list asciSign) (createColumnList matrixDimensions number-of-blanko)))

(defun createColumnList (matrixDimensions number-of-blanko )
      (cond ((> matrixDimensions 0)
             (if ( > number-of-blanko 0 )
                        (append  '( 1000 ) (createColumnList (1- matrixDimensions) (1- number-of-blanko))  )
                             (append '( 1100 )  (createColumnList (1- matrixDimensions) (1- number-of-blanko)) ) )  )
                            (t '()) ))

(defun calculateRowDimension (matrixDimensions)
     (- (* matrixDimensions 2) 1) )

(defun calculateEmptyBoardSpaces (matrixDimensions)
     (- matrixDimensions 1) )

(defun initState (asci-counter matrixDimensions number-of-blanko conditionParm )
 (cond ( (< (abs number-of-blanko) conditionParm)
      (cond (( < number-of-blanko 0 )
              (cons (createColumn asci-counter matrixDimensions (abs number-of-blanko)) (initState (1+ asci-counter) matrixDimensions (1- number-of-blanko) conditionParm) ) )
                  (t (cons (createColumn asci-counter matrixDimensions number-of-blanko) (initState (1+ asci-counter) matrixDimensions (1- number-of-blanko) conditionParm) ))))
                        (t '())))

(defun startState(n)
      (initState 65 (calculateRowDimension n) (calculateEmptyBoardSpaces n ) (+ (calculateEmptyBoardSpaces n ) 1)))
;----------------------------------------------------------------------------

(defun getIndex (asciSign)
      (- (char-code asciSign) 65 ) )

(defun isMoveValid ( element)
    (equalp 1100 element) )

(defun putElementInList (i list element )
      (cond ((null list) '())
            ((atom element )
              (if (and (= i 0) (isMoveValid  (car list)))
                    (progn
                    (setq nextToPlay
         (not nextToPlay
        ))
                    (append (list element) (putElementInList (1- i) (cdr list) element)))
                      (append (list (car list))  (putElementInList (1- i) (cdr list) element ))))
                        (t(if (= i 0)
                            (append (list element) (putElementInList (1- i) (cdr list) element))
                                (append (list (car list))  (putElementInList (1- i) (cdr list) element ))))))

(defun play (asciSign index)
      (if nextToPlay
            (setq gameState
             (putElementInList (getIndex asciSign ) gameState (putElementInList index (assoc (char-code asciSign ) gameState) 1111 )))
              (setq gameState
               (putElementInList (getIndex asciSign ) gameState (putElementInList index (assoc (char-code asciSign ) gameState) 1110 )))))

;--------------------------------------------------------------
;Detect bridge

(defun detectBridge (list)
(cond ((null list) '())
      (t (prog1 (detectBridge (car list)) (detectBridge (cdr list))))))

(defun getValue (list j) 
  (cond ((= j -1) (car list))
        (t (getValue (cdr list ) (1- j) ))))

(defun isNodeEdgeOfBoard (node)
  (isNodeInList node listOfBoardEdges))


(defun isNodeInList (node list)
  (cond ((null list) '())
          ((equal node (car list)) t)
          (t(isNodeInList node (cdr list)))))

(defun createNode (i j)
  (append (list i) (list j)))

(defun pushOnStack (node)
     (cond
        ((not (or (isNodeInList node searchedFields) (isNodeInList node stack)))
             (setq stack (cons node stack)))
             (t '())))

(defun pushInSearchedFields (node)
      (if (not (isNodeInList node searchedFields))
               (setq searchedFields  (cons node searchedFields))))

(defun createListOfBoardEdges ()
  (list(append (list 65) (list (1- matrixDimensions )))
       (append (list 65) (list (- (* matrixDimensions 2) 2)))
       (append (list (+ 65 (1- matrixDimensions))) (list 0))
       (append (list (+ 65 (1- matrixDimensions))) (list (- (* matrixDimensions 2) 2)))
       (append (list (+ 65 (- (* matrixDimensions 2) 2))) (list (1- matrixDimensions)))
       (append (list (+ 65 (- (* matrixDimensions 2) 2))) (list(- (* matrixDimensions 2) 2)))))

(defun resetVariablesAfterForBridgeSearch ()
  (setq stack '())
  (setq searchedFields '()))

(defun searchForBridge (node)
    (setq selected-node node)
    (setq vrednost_cvora (vrati_vrednost   (assoc (car node) gameState) (car (cdr node))))
    (pushOnStack node )
    (pushInSearchedFields node)
    (print (searchBoard node )
    (resetVariablesAfterForBridgeSearch)))

(defun searchBoard (node)
    (cond
      ((null stack) '())
      ((and (isNodeEdgeOfBoard (car stack))
            (not (equal selected-node node))) (print "Bridge detected!!!!"))
      (t(prog1
               (pushInSearchedFields (car stack))
               (setq stack (cdr stack))
               (searchAdjacent  (car node) (car(cdr node)))
               (searchBoard (car stack))))))

(defun searchAdjacent (i j)
      (searchRow (assoc i gameState) i j -1)
      (searchRow (assoc (1- i) gameState ) (1- i) j -1)
      (searchRow (assoc (1+ i) gameState ) (1+ i) j -1))

(defun searchRow (list i j counter)
     (cond
     ((null list) '())
     ((and(= vrednost_cvora (car list))
          (or (= counter j) (= counter (- j 1)) (= counter (+ j 1))))
      (progn
            (pushOnStack (createNode i counter))
            (searchRow (cdr list) i j (1+ counter))))
      (t (searchRow (cdr list) i j (1+ counter) ))))

;--------------------------------------------------------------
(defun loop ()
      (print  "Row:")
      (setq row (read ))
      (print  "Column")
      (format t "~%")
      (setq column (read ))
      (play #\A 6)
      (play #\E 3)
        (play #\A 7)
          (play #\E 4)
            (play #\A 8)
              (play #\E 5)
                      (play #\B 6)
                      (play #\G 3)
                        (play #\B 7)
                          (play #\G 4)
                            (play #\B 8)
                              (play #\G 5)
                                (play #\B 9)
                                  (play #\G 6)
                                    (play #\B 10)
                                      (play #\G 7)
                                      (play #\B 11)
                                        (play #\H 7)

      (play row (1+ column))
      (printGameBoard 65 (* 2 matrixDimensions) gameState)
      (format t "~%")
      (format t "Detected bridge ~%")
      (searchForBridge (createNode 65 5))
      (loop))

(defun start ()
      (globalVariableInit )
      (print   "n: ")
      (setq matrixDimensions ( read ) )
      (setq listOfBoardEdges (createListOfBoardEdges))
      (format t "~%")
      (setf gameState (startState matrixDimensions) )
      (printGameBoard 65 (* 2 matrixDimensions) gameState)
      (loop))
(start)
;!!!!! How to play: enter value for like this #\A   #\B ...
;--------------------------------------------------------------
