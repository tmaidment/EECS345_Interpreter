; Jeff Eben
; Mammen Kurien
; Tristan Maidment

(load "classParser.scm")
(load "state.scm")


(define interpreter
  (lambda (file classtype)
    (setMemory (a_memoryStructure '() '())) ; clear variables
      ;(pushLayer) ; Initial layer
      ;(overallProgramWrapper (parser l) '() '())))
    (createStateDefinitions (parser file))
    (createInstance 'main (executeNew 'main (string->symbol classtype)))
    (interpreterFxn (cadr (getFxn 'main 'main (string->symbol classtype))) '() '() 'main (string->symbol classtype));
    ;(runClassMain (getClassBody (string->symbol classtype)) classtype) ; Run on class X
    )
  )

;(define runClassMain
;  (lambda (l classtype)
;    (cond
;      ((null? l) void)
;      ((eq? (caar l) 'var) ((executeVar (cdar l) '()) (runClassMain (cdr l) classtype))
;      ((eq? (caar l) 'function) ((saveFunction (cdar l) '() '()) (runClassMain (cdr l) classtype)))  ; MAY NOT NEED THIS LINE
;      ((eq? (caar l) 'static-function)     (createInstance 'main (executeNew 'main classtype))
                                        ;((defineFxn (cdar (name l)) (cdar (args l)) (cdar (body l)))
                                        ;(interpreterFxn (cdar (body l)) '() '()))
;                                       )
      ;(else) Line to deal with static-function main 
;      ))))

; 
(define createStateDefinitions
  (lambda (l)
    (cond
      ((null? l) (void))
      ((eq? (caar l) 'class) (begin
                              (defineClass (a_class (car (cdar l)) (cadr (cdar l)) (caddr (cdar l)))) ; type class-extension body
                              (createStateDefinitions (cdr l))))
      )))




(define interpreterFxn
  (lambda (l returnBreak tryBreak state type)
    ; Set the State
    (setCurrentState state type)
    (pushSubState)
    (letrec ((returnValue (lambda (l returnBreak tryBreak)
                            (call/cc
                             (lambda (returnBreak)
                               (overallProgramWrapper l returnBreak tryBreak))))))
      (let ((value (returnValue l returnBreak tryBreak)))
        (begin
          (popSubState)
          ; Reset State
          (setCurrentState (a_getStateName (a_previousState (a_getWorkingMem memory))) (getStateType (a_getStateName (a_previousState (a_getWorkingMem memory)))))
          value)))
    ))



(define overallProgramWrapper
  (lambda (l returnBreak tryBreak)
       (computeProgramContinuations l '() '() returnBreak tryBreak)
       )
    )

; This will create a list of a_state lists, which will have all types from child to parent
(define executeNew
  (lambda (name type)
    (cond
      ((null? (getClassParent type)) (cons (a_state name type newLayers) '()))
      (else (cons (a_state name type newLayers) (executeNew name (getClassParent type))))
      )
    )
  )


(define createInstance
  (lambda (name stateList)
    (cond
      ((null? stateList) void)
      (else
       (defineState (car stateList)) ; Make first state
       (computeProgramContinuations (getClassBody (getStateType name)) '() '() '() '()) ; Add vars/fxns to first state ;POTENTIAL PROBLEM = '(getClassBody (getStateType' since each state has multiple types if it inherits
       ; Remove State
       (if (not (null? (cdr (a_getWorkingMem memory))))
           (setCurrentState (a_getStateName (a_previousState (a_getWorkingMem memory))) (getStateType (a_getStateName (a_previousState (a_getWorkingMem memory))))))
       (createInstance name (cdr stateList))
       )
      )
    )
  )

; (getVar var state type)

; Returns the result of the variable/function being called on within the state
; Parents of class have same state with different type
(define executeDot
  (lambda (l)
    (cond
      ((eq? (car l) 'this) (recursiveDot (name (topState (a_getWorkingMem memory))) (getStateType (name (topState (a_getWorkingMem memory)))) (cadr l)))
      ((eq? (car l) 'super) (recursiveDot (name (topState (a_getWorkingMem memory))) (getClassParent (getStateType (name (topState (a_getWorkingMem memory))))) (cadr l)))
      ((atom? (car l)) (recursiveDot (car l) (getStateType (car l)) (cadr l)))
      ; If car not atom, must be new.  If not new, error
      ((not (eq? (caar l) 'new)) (error "invalid class instance call"))
      ; If new: 
      (else
       (begin
         (createInstance '() (executeNew '() (cadar l)))
         (recursiveDot '() (cadar l) (cadr l))
         )
       )
      )
    )
  )

          
; Takes in the current state and the method/variable being called.  For case of super, it just gets the parent state's type
(define recursiveDot
  (lambda (state type var)
    (cond
      ((not (null? (getVar var state type 'global))) (getVar var state type 'global))
      ((not (null? (getFxn var state type))) (getFxn var state type))
      ((null? (getClassParent type)) (error "Method does not exist is class heirarchy"))
      (else (recursiveDot state (getClassParent type) var)))
    ))


(define firstPart car)
(define restOfParse cdr)

(define computeProgramContinuations
  (lambda (l continue break returnBreak tryBreak)
    (cond
      ((null? l) (void))
      (else
       (let ((computation (computeProgramLineContinuations (firstPart l) continue break returnBreak tryBreak)))
         (cond
           ((void? computation) (computeProgramContinuations (restOfParse l) continue break returnBreak tryBreak))
           (else computation)))))))

(define name car)
(define fxnBody cdr)
(define throwVar cadr)

(define computeProgramLineContinuations
  (lambda (l continue break returnBreak tryBreak)
    (cond
      ((eq? (name l) 'static-function) (saveFunction (fxnBody l) returnBreak tryBreak)) ; do nothing
      ;((eq? (name l) 'new) (executeNew (fxnBody l)))
      ;((eq? (name l) 'dot) (executeDot (fxnBody l)))
      ((eq? (name l) 'function) (saveFunction (fxnBody l) returnBreak tryBreak))
      ((eq? (name l) 'funcall) (executeFunction (fxnBody l) returnBreak tryBreak))
      ((eq? (name l) 'var) (executeVar (fxnBody l) tryBreak))
      ((eq? (name l) '=) (executeEqual (fxnBody l) tryBreak))
      ((eq? (name l) 'return) (executeReturn (fxnBody l) returnBreak tryBreak)) ;only returnBreak needed, since a return breaks from whole function
      ((eq? (name l) 'if) (executeIf (fxnBody l) continue break returnBreak tryBreak)) ; Body could contain all possible call/cc vars
      ((eq? (name l) 'while) (executeWhile (fxnBody l) returnBreak tryBreak)) ; Body could contain all possible call/cc vars
      ((eq? (name l) 'begin) (computeProgramContinuations (fxnBody l) continue break returnBreak tryBreak)) 
      ((eq? (name l) 'continue)
       (if (null? continue)
           (error "You cannot continue when there is no loop")
           (continue 'continueReturn))) ;Should return to program spot where continuation is formed if I did it correctly
      ((eq? (name l) 'break)
       (if (null? break)
           (error "You cannot break when there is no loop")
           (break 'breakReturn)))

      
      ((eq? (name l) 'try) (executeTry (fxnBody l) continue break returnBreak tryBreak))
      ((eq? (name l) 'catch) (executeCatch (fxnBody l) continue break returnBreak tryBreak)) 
      ((eq? (name l) 'finally) (executeFinally (fxnBody l) continue break returnBreak tryBreak))
      ((eq? (name l) 'throw)
       (if (eq? tryBreak '())
           (error "You cannot throw an exception out of a try block")
           ((throw (throwVar l))
            (tryBreak 'tryReturn))))
      (else
       (if (eq? tryBreak '())
           (error "You cannot throw an exception out of a try block")
           (tryBreak 'tryReturn))) )))



;----------------------------------------------------------------------------------------
(define name car)
(define args cadr)
(define body caddr)

(define saveFunction
  (lambda (l returnBreak tryBreak)
    (cond
      ;((eq? (name l) 'main)
       ;(defineFxn (name l) (args l) (body l) S)
       ;(interpreterFxn (body l) returnBreak tryBreak)
       ;)
      (else (defineFxn (a_fxn (name l) (args l) (body l)) (name (topState (a_getWorkingMem memory))) (getStateType (name (topState (a_getWorkingMem memory)))) )
            )
      )))

(define ArgsBody car)
(define ArgValues cdr)

; Problems = getFxn not recursive.  (dot x y) called before fxn is interpreted
(define executeFunction
  (lambda (l returnBreak tryBreak)
    (cond
      ((eq? 'dot (car (ArgsBody l)))
       (interpreterFxn (functionCreator (recursiveGetFxn (caddr (ArgsBody l)) tryBreak (cadr (ArgsBody l)) (getStateType (cadr (ArgsBody l)))) (ArgValues l)) returnBreak tryBreak (cadr (ArgsBody l)) (getStateType (cadr (ArgsBody l)))))
      (else
       (interpreterFxn (functionCreator (ArgsBody l) (ArgValues l)) returnBreak tryBreak (cadr (ArgsBody l)) (getStateType (cadr (ArgsBody l))))
       ;(interpreterFxn (functionCreator (recursiveGetFxn (ArgsBody l) tryBreak (name (topState (a_getWorkingMem memory))) (getStateType (name (topState (a_getWorkingMem memory)))) ) (ArgValues l)) returnBreak tryBreak)) ; input looks like (((args) (body)) (argValues))
       )
      )
    )
  )

(define recursiveGetFxn
  (lambda (name tryBreak state type)
    (cond
      ((not (null? (getFxn name state type)))
       (getFxn name state type))
      ((null? (getClassParent type)) (error "Function doesn't exist"))
      (else (recursiveGetFxn name tryBreak state (getClassParent type))))
    )
  )

;(getFxn (executeOperator (ArgsBody l) tryBreak) (name (topState (a_getWorkingMem memory))) (getStateType (name (topState (a_getWorkingMem memory)))))


(define functionCreator
  (lambda (fxnArgsBody fxnArgValues)
    (let ((args (ArgsBody fxnArgsBody)) (body (cadr fxnArgsBody))) ; abstract out args and body into individual lists
      (cond
        ((eq? #t (stateExists? (car fxnArgValues))) (functionCreatorHelper args body fxnArgValues))
        (else (functionCreatorHelper args body (executeComputeValues fxnArgValues))))))) ; input looks like ((args) (body) (values))



(define functionCreatorHelper
  (lambda (args body values)
    (append (append (cons (list 'var args values) '()) body) '((return))) ; appended return for case where there is no return
    )
  )


       
      
(define restOfList cdr)

;(addVar name value state type)
(define executeVar
  (lambda (l tryBreak)
    (cond
      ((not (atom? (element1 l))) (executeMultVars (element1 l) (executeComputeValues (second_element l)))) ; for function passing
      ((null? (restOfList l)) (addVar (element1 l) '() topState (getStateType topState)))
      (else
       (cond
         ((not (list? (second_element l))) (addVar (element1 l) (executeOperator (second_element l) tryBreak) (name (topState (a_getWorkingMem memory))) (getStateType (name (topState (a_getWorkingMem memory))))))
         ((eq? (car (second_element l)) 'new) (createInstance (element1 l) (executeNew (element1 l) (cadr (second_element l)))))
         (else (addVar (element1 l) (executeOperator (second_element l) tryBreak) (name (topState (a_getWorkingMem memory))) (getStateType (name (topState (a_getWorkingMem memory)))))))))
    (void)))
         


       ;(addVar (element1 l) (executeOperator (second_element l) tryBreak) S)))
    ;(void)))


(define executeMultVars
  (lambda (vars values)
    (cond
      ((and (null? vars) (null? values)) void)
      ((or (null? vars) (null? values)) (error "Wrong number of inputs" ))
      ((null? (element1 values))
       (begin
         (addVar (element1 vars) '() (name (topState (a_getWorkingMem memory))) (getStateType (name (topState (a_getWorkingMem memory)))))
         (executeMultVars (restOfList vars) (restOfList values))))
      (else
       (begin
         (addVar (element1 vars) (element1 values) (name (topState (a_getWorkingMem memory))) (getStateType (name (topState (a_getWorkingMem memory)))))
         (executeMultVars (restOfList vars) (restOfList values)))))))

; Computes all values in value list to get rid of static State definition problem
(define executeComputeValues
  (lambda (l)
    (cond
      ((null? l) l)
      (else (cons (executeOperator (element1 l) '()) (executeComputeValues (restOfList l))))))) 

; setvar inputs = name value state type
(define executeEqual
  (lambda (l tryBreak)
    (cond
      ((and (list? (second_element l)) (eq? (car (second_element l)) 'new)) (createInstance (element1 l) (executeNew (element1 l) (cadr (second_element l)))))
      (else (let ((setLayer (settingHelper (element1 l) (name (topState (a_getWorkingMem memory))) (getStateType (name (topState (a_getWorkingMem memory)))))))
              
              (setVar (car setLayer) (executeOperator (second_element l) tryBreak) (cadr setLayer) (caddr setLayer)))))))
      ;(else (setVar (settingHelper (element1 l)) (executeOperator (second_element l) tryBreak) (name (topState (a_getWorkingMem memory))) (getStateType (name (topState (a_getWorkingMem memory))))))


; This will return the type that the variable is in
(define settingHelper
  (lambda (name state type)
    (cond
      ((atom? name) (settingHelper2 name state type)) ; Case of no dot
      (else (settingHelper2 (caddr name) (cadr name) (getStateType (cadr name))))))) ; Case of dot

(define settingHelper2
  (lambda (name state type)
    (cond
      ((eq? #t (checkVar name state type)) (cons name (cons state (cons type '())))) ; returns: (name type)
      ((null? (getClassParent type)) (error "variable not defined. name state type:" name state type))
      (else (settingHelperAtom name state (getClassParent type))))))

    


;atom? --> checks if atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;abstraction for ExecuteOperator and ExecuteReturn
(define element1 car)
(define second_element cadr )
(define rest_of_rest cddr )
(define third_element caddr )

; This helper function returns the value of some parse-tree arithmetic subexpression
; Could have numbers or variables that equal numbers
(define executeOperator
  (lambda (l tryBreak)
    (cond
      ; If it's a straight up number
      ((number? l) l)
      ; If it's 'true'
      ((eq? l 'true) #t)
      ((eq? l 'false) #f)
      ; If it's a variable
      ; If none of the others, its a variable
      ((atom? l) (getVar  l (name (topState (a_getWorkingMem memory))) (getStateType (name (topState (a_getWorkingMem memory)))) '()))
      ; Number in a list (for fib)

      ((eq? (name l) 'new) (executeNew (fxnBody l)))
      ((eq? (name l) 'dot) (executeDot (fxnBody l)))
      
      ; Function call
      ((eq? (element1 l) 'funcall) (executeFunction (cdr l) '() tryBreak))
      ; This allows working with AND's and OR's
      ((eq? (element1 l) '||) (or (executeOperator (second_element l) tryBreak) (executeOperator (third_element l) tryBreak)))
      ((eq? (element1 l) '&&) (and (executeOperator (second_element l tryBreak)) (executeOperator (third_element l) tryBreak)))
      ((eq? (element1 l) '!) (not (executeOperator (second_element l) tryBreak)))
      ; Allows working with inequalities
      ((eq? (element1 l) '>) (> (executeOperator (second_element l) tryBreak) (executeOperator (third_element l) tryBreak)))
      ((eq? (element1 l) '>=) (>= (executeOperator (second_element l) tryBreak) (executeOperator (third_element l) tryBreak)))
      ((eq? (element1 l) '<) (< (executeOperator (second_element l) tryBreak) (executeOperator (third_element l) tryBreak)))
      ((eq? (element1 l) '<=) (<= (executeOperator (second_element l) tryBreak) (executeOperator (third_element l) tryBreak)))
      ((eq? (element1 l) '==) (eq? (executeOperator (second_element l) tryBreak) (executeOperator (third_element l) tryBreak)))
      ((eq? (element1 l) '!=) (not (eq? (executeOperator (second_element l) tryBreak) (executeOperator (third_element l) tryBreak))))
      ; If list isn't null, isn't conditional, and isn't and/or, it must start with operator (or variable)
      ((eq? (element1 l) '+) (+ (executeOperator (second_element l) tryBreak) (executeOperator (third_element l) tryBreak)))
      ; Two cases: unary -, or subtraction
      ((eq? (element1 l) '-)
       (if (null? (rest_of_rest l)) ;This case is when unary - is used
            (- 0 (executeOperator (second_element l) tryBreak))
            (- (executeOperator (second_element l) tryBreak) (executeOperator (third_element l) tryBreak)))) ; this is subtraction case
      ((eq? (element1 l) '*) (* (executeOperator (second_element l) tryBreak) (executeOperator (third_element l) tryBreak)))
      ((eq? (element1 l) '/) (/ (executeOperator (second_element l) tryBreak) (executeOperator (third_element l) tryBreak)))
      ((eq? (element1 l) '%) (modulo (executeOperator (second_element l) tryBreak) (executeOperator (third_element l) tryBreak))) )))

; Wrapper
(define executeReturn
  (lambda (l returnBreak tryBreak)
    ; Case when there is no return value in a fxn
    (if (null? l) ((popLayer)
                   (returnBreak (void)))
    (if (eq? (executeOperator (element1 l) tryBreak) #t) ;covert #t to true
        ((popLayer)
        (returnBreak 'true))
        (if (eq? (executeOperator (element1 l) tryBreak) #f) ;convert #f to false
        ((popLayer)
         (returnBreak 'false))
        ((let ((result (executeOperator (element1 l) tryBreak)))
           ;(popLayer)
           (returnBreak result))))))))


;Abstraction for ExecuteIf
(define element_1 car)
(define second_element cadr )
(define rest_of_rest cddr )
(define third_element caddr )

; {(conditional) (then-statement) (optional-else-statement)}
; If (car l) isn't equal to an operator, it must be one of the computeProgram cases
(define executeIf
  (lambda (l continue break returnBreak tryBreak)
    (cond
      ((eq? (executeOperator (element_1 l) tryBreak) #t)
       (pushLayer)
       (breakAbstraction (second_element l) continue break returnBreak tryBreak 'if))
      ((not (eq? (rest_of_rest l) '()))
       (pushLayer)
       (breakAbstraction (third_element l) continue break returnBreak tryBreak 'if)))))


; Expression must either be some expression (executeOperator) or one of the 5 parse tree statements 
(define executeIfThenStatement
  (lambda (l continue break returnBreak tryBreak)
    (computeProgramLineContinuations l continue break returnBreak tryBreak) ; used ComputeProgram in case it returns value.  
      ))

; {(conditional) (body statement)}
; Create continuation before doing the whileThen, so that way if break is found, whole while loop is broken from
(define executeWhile
  (lambda (l returnBreak tryBreak) 
    (breakAbstraction l '() '() returnBreak tryBreak 'whilebreak)
    ))
    

; Created for correct implementation of break continuation
(define executeWhileAfterBreak
  (lambda (l break returnBreak tryBreak)
    (cond
      ((eq? (executeOperator (element1 l) tryBreak) #t) (executeWhileThen l break returnBreak tryBreak))
      )))



; For continue, this should break out of continue block and just do the (executeWhile initialList) if continue is called
(define executeWhileThen;CallCC
  (lambda (l break returnBreak tryBreak)
    (pushLayer)
    (breakAbstraction l '() break returnBreak tryBreak 'whilecontinue)
    ))


(define executeTry
  (lambda (l continue break returnBreak tryBreak)
    (pushLayer)
    (breakAbstraction l continue break returnBreak tryBreak 'try)))

;Abstraction for ExecuteFinally
(define second_element cadr )
(define throwName caar)

(define executeCatch
  (lambda (l continue break returnBreak tryBreak)
    (addVar (throwName l) (getThrow))
    (computeProgramContinuations (second_element l) continue break returnBreak tryBreak)
    (popLayer)
    ))

;Abstraction for ExecuteCatch
(define element_1 car)

(define executeFinally
  (lambda (l continue break returnBreak tryBreak)
    (computeProgramContinuations (element_1 l) continue break returnBreak tryBreak)
    (popLayer)
    ))

(define catchStatement cadr)
(define finallyStatement caddr)

(define breakAbstraction
  (lambda (l continue break returnBreak tryBreak type)
    (cond
      ((eq? type 'whilebreak)
       ; Must use this to make sure theres not a double remove from continue and break
       (cond
         ((eq? (call/cc
                (lambda (break)
                  (executeWhileAfterBreak l break returnBreak tryBreak))) 'breakReturn)
          (popLayer)
          )))
      ((eq? type 'whilecontinue)
       (let ((l2 (second_element l)))
       (call/cc
        (lambda (continue)
          (computeProgramLineContinuations l2 continue break returnBreak tryBreak)
       ))
         (popLayer)
         (executeWhileAfterBreak l break returnBreak tryBreak)))


      ((eq? type 'if)
       (letrec ((returnType (lambda (l continue break returnBreak tryBreak type)                     
                              (call/cc
                               (lambda (ifbreak)
                                 (executeIfThenStatement l ifbreak ifbreak ifbreak ifbreak))))))
         (let ((type (returnType l continue break returnBreak tryBreak type)))
           (cond
             ((eq? type 'breakReturn)
              (begin
                (popLayer)
                (break 'breakReturn)))
             ((eq? type 'continueReturn)
              (begin
                (popLayer)
                (continue 'continueReturn)))
             ((eq? type 'tryReturn)
              (let ((thrownVar (getThrow)))
                (begin
                  (popLayer)
                  (throw (executeOperator thrownVar tryBreak))
                  (tryBreak 'tryReturn))))
             ((not (void? type))
              (begin
                (popLayer)
                (returnBreak type)))
             (else
              (popLayer)
              )))))
                              

      ((eq? type 'try)
       (call/cc
        (lambda (tryBreak)
          (computeProgramContinuations (car l) continue break returnBreak tryBreak)))
       ; Will be '() if nothing thrown
       (let ((thrownVar (getThrow)))
         ; Get rid of current layer
         (popLayer)
         (cond
           ; If catch exists
           ((not (null? (catchStatement l)))
            (if (not (null? thrownVar))
                (begin
                  (pushLayer)
                  (throw thrownVar)
                  (computeProgramLineContinuations (catchStatement l) continue break returnBreak tryBreak)))
            (cond
              ((not (null? (finallyStatement l)))
               (begin
                 (pushLayer)
                 (computeProgramLineContinuations (finallyStatement l) continue break returnBreak tryBreak)))))
           ((not (null? (finallyStatement l)))
            (begin
              (pushLayer)
              (computeProgramLineContinuations (finallyStatement l) continue break returnBreak tryBreak))))))
      
      
    )))

