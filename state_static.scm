
;  _______________________________________________ _________
; /   _____/\__    ___/  _  \__    ___/\_   _____//   _____/
; \_____  \   |    | /  /_\  \|    |    |    __)_ \_____  \ 
; /        \  |    |/    |    \    |    |        \/        \
;/_______  /  |____|\____|__  /____|   /_______  /_______  /
;        \/                 \/                 \/        \/ 

;----------------------------------------
;             VARIABLES
;----------------------------------------

;Uses two manually syncronzied lists to hold variable - value pairs

;Stores variable declaration name
(define S '())

;----------------------------------------
;             ABSTRACTIONS
;----------------------------------------
(define varList car)

(define valList cadr)

(define getThrowLayer caddr)

(define getThrow
  (lambda ()
    (getThrowLayer (topLayer S))
    )
  )

(define fxnList cadddr)
  
(define newLayer '(() () () ()))

(define topLayer car)

(define pushLayerList
  (lambda (l)
    (setState (cons l S))
    )
  )

(define pushLayer
  (lambda ()
    (pushLayerList newLayer)
    )
  )

(define popLayer
  (lambda ()
    (setState (subLayers S))
    )
  )

(define setTopLayer
  (lambda (l)
    (popLayer)
    (pushLayerList l)
    )
  )

(define setTopLayerState
  (lambda (l state)
    (cons l (subLayers state))
    )
  )

(define firstElement car)

(define subLayers cdr)

(define layer
  (lambda (var val throw fxn)
    (list var val throw fxn)
    )
  )

(define fxn
  (lambda (name args body)
    (list name args body)
    )
  )


(define pushSubState
  (lambda ()
    (pushLayerList 'substate)
    (pushLayer)
    )
  )

(define popSubState
  (lambda ()
    (setState (popSubStateHelper S))
    )
  )

(define popSubStateHelper
  (lambda (state)
    (cond
      ((null? state) state)
      ((eq? (topLayer state) 'substate) (subLayers state))
      (else(popSubStateHelper (subLayers state)))
      )
    )
  ) 


;----------------------------------------
;             HELPER FUNCTIONS
;----------------------------------------

;(atom? x)
;Returns whether or not x is an atom.
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define substate?
  (lambda (l)
    (cond
      ((eq? l 'substate) #t)
      (else #f)
      )
    )
  )

;(varExists? n s)
;Returns whether or not a variable has been initialized.
(define varExistsLayer?
  (lambda (x layer)
    (cond
      ((null? layer) #f)
      ((substate? layer) #f)
      ((list? (varList layer)) (varExistsLayer? x (varList layer)))
      ((eq? x (car layer)) #t)
      (else (varExistsLayer? x (cdr layer)))
      )
    )
  )

;(varExists? x state)
;Returns whether or not a variable has been initialized.
(define varExists?
  (lambda (x state)
    (cond
      ((null? state) #f)
      ((varExistsLayer? x (topLayer state)) #t)
      (else (varExists? x (subLayers state)))
      )
    )
  )

(define varExistsSubState?
  (lambda (x state)
    (cond
      ((null? state) #f)
      ((substate? (topLayer state)) #f)
      ((varExistsLayer? x (topLayer state)) #t)
      (else (varExistsSubState? x (subLayers state)))
      )
    )
  )

(define fxnExistsLayer?
  (lambda (name fxnlist)
    (cond
      ((null? fxnlist) #f)
      ((eq? name (car (car fxnlist))) #t)
      (else (fxnExistsLayer? name (cdr fxnlist)))
      )
    )
  )

(define fxnExists?
  (lambda (name state)
    (cond
      ((null? state) #f)
      ((substate? (topLayer state)) (fxnExists? name (subLayers state)))
      ((fxnExistsLayer? name (fxnList (topLayer state))) #t)
      (else (fxnExists? name (subLayers state)))
      )
    )
  )

(define fxnExistsSubState?
  (lambda (name state)
    (cond
      ((null? state) #f)
      ((substate? (topLayer state)) #f)
      ((fxnExistsLayer? name (fxnList (topLayer state))) #t)
      (else (fxnExists? name (subLayers state)))
      )
    )
  )


;(removeIndex l i 0)
(define removeIndex
  (lambda (l i c)
    (cond
      ((= i c) (cdr l))
      (else (cons (car l) (removeIndex (cdr l) i (+ c 1))))
      )
    )
  )
;----------------------------------------
;             STATE FUNCTIONS
;----------------------------------------

(define throw
  (lambda (v)
    (setTopLayer (layer (varList (topLayer S)) (valList (topLayer S)) v (fxnList (topLayer S))))
    )
  )

;(setState s v)
;Sets the S and V list to s and v respectively.
(define setState
  (lambda (s)
      (set! S s)
    )
  )

;(addVar n v)
;Adds a variable and a value (which can be nothing) to the lists.
(define addVar
  (lambda (n v state)
    (if (varExistsSubState? n state) (error "Variable already defined"))
    (setTopLayer (layer (cons n (varList (topLayer state))) (cons v (valList (topLayer state))) (getThrowLayer (topLayer state)) (fxnList (topLayer state))))
    )
  )

;(setVar n v)
;Sets a defined variable to a value.
(define setVar
  (lambda (n v state)
    (cond
      ((varExistsSubState? n state) (setState (setVarHelper n v state)))
      ((varExistsLayer? n (getGlobalLayer state)) (setState (setGlobalVarHelper (setVarLayer n v (getGlobalLayer state)) state)))
      (else (error "Variable not defined"))
      )
    )
  )

(define setGlobalVarHelper
  (lambda (layer state)
    (cond
      ((null? (subLayers state)) (cons layer '()))
      (else (cons (topLayer state) (setGlobalVarHelper layer (subLayers state))))
      )
    )
  )

(define setVarHelper
  (lambda (n v state)
    (cond
      ((substate? (topLayer state)) (error "This condition should never occur."))
      ((varExistsLayer? n (topLayer state))
       (cons (setVarLayer n v (topLayer state)) (subLayers state)))
      (else (cons (topLayer state) (setVarHelper n v (subLayers state))))
      )
    )
  )

(define setVarLayer
  (lambda (n v layer)
      (list (cons n (varList (removeVarLayer n layer))) (cons v (valList (removeVarLayer n layer))) (getThrowLayer layer) (fxnList layer))
      ;LAYER DOESNT WORK HERE FOR SOME REASON 
    )
  )
      

;(removeVar n)
;Removes variable n from the lists.
(define removeVar
  (lambda (n state)
    (cond
      ((substate? (topLayer state)) (error "This shouldn't ever occur either."))
      ((eq? (removeVarLayer n (topLayer state)) 'error)
       (cons (topLayer state) (removeVar n (subLayers state))))
      ((null? (subLayers state)) (list (removeVarLayer n (topLayer state))))
      (else (setState (list (removeVarLayer n (topLayer state)) (subLayers state))))
      )
    )
  )

(define getVarIndex
  (lambda (n varlist i)
    (cond
       ((eq? n (firstElement varlist)) i)
       (else (getVarIndex n (cdr varlist) (+ i 1)))
       )
    )
  )

(define removeVarLayer
  (lambda (n layer)
    (cond
      ((not (varExists? n layer)) 'error)
      (else (list (removeIndex (varList layer) (getVarIndex n (varList layer) 0) 0) (removeIndex (valList layer) (getVarIndex n (varList layer) 0) 0) (getThrowLayer layer)))
      )
    )
  )


;(getVar x state)
;Returns the value of variable x in state S, if it exists.
(define getVar
  (lambda (x state)
    (cond
      ((varExistsLayer? x (topLayer state)) (getVarLayer x (topLayer state)))
      ((varExistsSubState? x (subLayers state)) (getVar x (subLayers state)))
      ((varExistsLayer? x (getGlobalLayer state))  (getVarLayer x (getGlobalLayer state)))
      (else (error "Variable does not exist"))
     )
  )
  )

(define getVarLayer
  (lambda (x layer)
    (cond
      ((getVarHelper x (varList layer) (valList layer)))
      )
    )
  )

(define getVarHelper
  (lambda (x var val)
    (cond
      ((null? var) 'error)
      ((eq? x (car var)) (car val))
      (else (getVarHelper x (cdr var) (cdr val)))
      )
    )
  )

(define getGlobalLayer
  (lambda (state)
    (cond
      ((null? (subLayers state)) (topLayer state))
      (else (getGlobalLayer (subLayers state)))
      )
    )
  )

(define executingMain
  (lambda (state)
    (cond
      ((null? state) #f)
      ((eq? 'substate (topLayer state)) (executingMainHelper (subLayers state)))
      (else (executingMain (subLayers state)))
      )
    )
  )

(define executingMainHelper
  (lambda (state)
    (cond
      ((null? state) #t)
      ((eq? 'substate (topLayer state)) #f)
      (else (executingMainHelper (subLayers state)))
      )
    )
  )

;Clears ALL defined variables from the list
(define clearVars
  (lambda ()
    (setState '())
    )
  )
     
(define defineFxn
  (lambda (name args body state)
    (if (fxnExists? name state) (error "Function already defined"))
    (setTopLayer (layer (varList (topLayer state)) (valList (topLayer state)) (getThrowLayer (topLayer state)) (cons (fxn name args body) (fxnList (topLayer state)))))
    )
  )

(define getFxnLayer
  (lambda (name fxnlist)
    (cond
      ((null? fxnlist) (error "Function not/no longer defined"))
      ((eq? name (car (car fxnlist))) (list (cadr (car fxnlist)) (caddr (car fxnlist))))
      (else (getFxnLayer name (cdr fxnlist)))
      )
    )
  )

(define getFxn
  (lambda (name state)
    (cond
      ((null? state) (error "Function not/no longer defined"))
      ((substate? (topLayer state)) (getFxn name (subLayers state)))
      ((fxnExistsLayer? name (fxnList (topLayer state))) (getFxnLayer name (fxnList (topLayer state))))
      (else (getFxn name (subLayers state)))
      )
    )
  )

;----------------------------------------
;             LAYER FUNCTIONS
;----------------------------------------

(pushLayer)
(addVar 'x 5 S)
;(addVar 'w 3 S)
;(addVar 'y 6 S)
;(addVar 'z 7 S)
(pushLayer)
;(addVar 'w 3 S)
;(pushSubState)
(addVar 'w 3 S)
;(addVar 'v 9 S)
;(addVar 'w 8 S)
(setVar 'x 10 S)
(throw 'exception)
(defineFxn 'test '() '() S)
(defineFxn 'main '() '() S)
