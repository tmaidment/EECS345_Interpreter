;define memory / set memory --------------
(define memory '())
(define debug #t)

;sets the memory to specified memory structure, and prints it
(define setMemory
  (lambda (memoryStructure)
    (set! memory memoryStructure)
    (display memory)
    (display "\n")
    )
  )

;structure defns / and abstractions for those structures ------------
;memory structure and abstractions
(define a_memoryStructure
  (lambda (working defns)
    (list working defns)
    )
  )
(define a_getWorkingMem car)
(define a_getClassDefns cadr)

;class structure for holding uninitialized classes
(define a_class
  (lambda (type extends body)
    (list type extends body)
    )
  )
(define a_getClassType car)
(define a_getClassExtends cadr)
(define a_getClassBody caddr)

;function structure for holding functions
(define a_fxn
  (lambda (name args body)
    (list name args body)
    )
  )
(define a_getFxnName car)
(define a_getFxnArgs cadr)
(define a_getFxnBody caddr)

;state structure - states are basically init classes
(define a_state
  (lambda (name type layers)
    (list name type layers)
    )
  )
(define a_getStateName car)
(define a_getStateType cadr)
(define a_getStateLayers caddr)
(define topState car)
(define subStates cdr)

;layer abstractions

;layer structure
(define a_layer
  (lambda (var val throw fxn)
    (list var val throw fxn)
    )
  )
(define a_getLayerVars car)
(define a_getLayerVals cadr)
(define a_getLayerThrow caddr)
(define a_getLayerFxns cadddr)

;abstractions for layers
(define a_topLayer car)
(define a_subLayers cdr)

(define newLayer (a_layer () () () ()))
(define newLayers (cons newLayer '()))

;abstractions for different parts of the layer
(define a_varList car)
(define a_valList cadr)
(define a_throwVal caddr)
(define a_fxnList cadddr)

;simple layer functions
(define pushLayer
  (lambda (layer)
    (setMemory (a_memoryStructure (cons (pushLayerHelper layer (topState (a_getWorkingMem memory))) (subStates (a_getWorkingMem memory))) (a_getClassDefns memory)))
    )
  )
(define pushLayerHelper
  (lambda (layer state)
    (a_state (a_getStateName state) (a_getStateType state) (cons layer (a_getStateLayers state)))
    )
  )

(define popLayer
  (lambda ()
    (setMemory (a_memoryStructure (cons (popLayerHelper (topState (a_getWorkingMem memory))) (subStates (a_getWorkingMem memory))) (a_getClassDefns memory)))
    )
  )
(define popLayerHelper
  (lambda (state)
    (if (null? (cdr (a_getStateLayers state))) (error "popLayerHelper: cannot pop layer"))
    (a_state (a_getStateName state) (a_getStateType state) (cdr (a_getStateLayers state)))
    )
  )

;marks off current execution as a substate, for entering functions
(define pushSubState
  (lambda ()
    (pushLayer 'substate)
    (pushLayer newLayer)
    )
  )
(define popSubState
  (lambda ()
    (cond
      ((null? (a_topLayer (a_getStateLayers (topState (a_getWorkingMem memory))))) (error "popSubState: no subState to pop"))
      ((eq? (a_topLayer (a_getStateLayers (topState (a_getWorkingMem memory)))) 'substate) (popLayer))
      (else (begin (popLayer) (popSubState)))
      )
    )
  )

;Overall memory interactions

;(defineClass (class 'type 'extends 'body))
;this will define the structure of a class for later initialization
(define defineClass
  (lambda (classObj)
    (setMemory (a_memoryStructure (a_getWorkingMem memory) (cons classObj (a_getClassDefns memory))))
    )
  )

(define getClassBody
  (lambda (type)
    (getClassBodyHelper type (a_getClassDefns memory))
    )
  )
(define getClassBodyHelper
  (lambda (type classDefns)
    (cond
      ((null? classDefns) (error "getClassBodyHelper: class undefined"))
      ((eq? (a_getClassType (car classDefns)) type) (a_getClassBody (car classDefns)))
      (else (getClassBodyHelper type (cdr classDefns)))
      )
    )
  )

;returns the parent of specified type
(define getClassParent
  (lambda (type)
    (getClassParentHelper type (a_getClassDefns memory))
    )
  )
(define getClassParentHelper
  (lambda (type classDefns)
    (cond
      ((null? classDefns) '())
      ((eq? (a_getClassType (car classDefns)) type) (a_getClassExtends (car classDefns)))
      (else (getClassParentHelper type (cdr classDefns)))
      )
    )
  )

;(defineState (state 'name 'class 'body)
;for init classes
(define defineState
  (lambda (stateObj)
    (setMemory (a_memoryStructure (cons stateObj (a_getWorkingMem memory)) (a_getClassDefns memory)))
    )
  )
(define getStateType
  (lambda (name)
    (getStateTypeHelper name (a_getWorkingMem memory))
    )
  )
(define getStateTypeHelper
  (lambda (name workingMem)
    (cond
      ((null? workingMem) (error "getStateTypeHelper: State undefined"))
      ((eq? (a_getStateName (car workingMem)) name) (a_getStateType (car workingMem)))
      (else (getStateTypeHelper name (cdr workingMem)))
      )
    )
  )
(define getStateLayers
  (lambda (name)
    (getStateLayersHelper name (a_getWorkingMem memory))
    )
  )
(define getStateLayersHelper
  (lambda (name workingMem)
    (cond
      ((null? workingMem) (error "getStateLayersHelper: State undefined"))
      ((eq? (a_getStateLayers (car workingMem)) name) (a_getStateLayers (car workingMem)))
      (else (getStateLayersHelper name (cdr workingMem)))
      )
    )
  )

;adds var to specified state and type
(define addVar
  (lambda (name value state type)
    (setMemory (a_memoryStructure (addVarWM name value state type (a_getWorkingMem memory)) (a_getClassDefns memory)))
    )
  )
;this should return the ENTIRETY of workingMem, w modified state
(define addVarWM
  (lambda (name value state type workingMem)
    (cond
      ((null? workingMem) (error "addVarWM: State undefined"))
      ((and (eq? state (a_getStateName (topState workingMem))) (eq? type (a_getStateType (topState workingMem)))) (cons (addVarWM2 name value (topState workingMem)) (subStates workingMem)))
      (else (cons (topState workingMem) (addVarWM name value state type (subStates workingMem))))
      )
    )
  )
;this should return the modified state
(define addVarWM2 ;fix setmemory
  (lambda (n v state)
    (a_state (a_getStateName state) (a_getStateType state) (addVarHelper n v (a_getStateLayers state)))
    )
  )
(define addVarHelper
  (lambda (n v state)
    (if (varExistsSubState? n state) (error "addVarHelper: Variable already defined"))
    (cons (a_layer (cons n (a_varList (a_topLayer state))) (cons v (a_valList (a_topLayer state))) (a_throwVal (a_topLayer state)) (a_fxnList (a_topLayer state))) (a_subLayers state))
    )
  )

(define setVar
  (lambda (name value state type)
    (setMemory (a_memoryStructure (setVarWM name value state type (a_getWorkingMem memory)) (a_getClassDefns memory)))
    )
  )
;this should return the ENTIRETY of workingMem, w modified state
(define setVarWM
  (lambda (name value state type workingMem)
    (cond
      ((null? workingMem) (error "setVarWM: State undefined"))
      ((and (eq? state (a_getStateName (topState workingMem))) (eq? type (a_getStateType (topState workingMem)))) (cons (setVarWM2 name value (topState workingMem)) (subStates workingMem)))
      (else (cons (topState workingMem) (setVarWM name value state type (subStates workingMem))))
      )
    )
  )
;this should return the modified state
(define setVarWM2
  (lambda (name value state)
    (a_state (a_getStateName state) (a_getStateType state) (setVarHelper name value (a_getStateLayers state)))
    )
  )
;this should return the modified layers
(define setVarHelper
  (lambda (n v state)
    (cond
      ((varExistsSubState? n state) (setVarSubstate n v state))
      ((varExistsLayer? n (getGlobalLayer state)) (setGlobalVarHelper (setVarLayer n v (getGlobalLayer state)) state)) ;check if global vars work right
      (else (error "setVarHelper: Variable not defined"))
      )
    )
  )
(define setGlobalVarHelper
  (lambda (layer state)
    (cond
      ((null? (a_subLayers state)) (cons layer '()))
      (else (cons (a_topLayer state) (setGlobalVarHelper layer (a_subLayers state))))
      )
    )
  )
(define setVarSubstate
  (lambda (n v state)
    (cond
      ((varExistsLayer? n (a_topLayer state))
       (cons (setVarLayer n v (a_topLayer state)) (a_subLayers state)))
      (else (cons (a_topLayer state) (setVarSubstate n v (a_subLayers state))))
      )
    )
  )
(define setVarLayer
  (lambda (n v layer)
      (a_layer (cons n (a_varList (removeVarLayer n layer))) (cons v (a_valList (removeVarLayer n layer))) (a_throwVal layer) (a_fxnList layer))
    ;a_layer used to cause errors and only list would work here - now it doesn't? if this causes errors, revert it to list
    )
  )

(define removeVar
  (lambda (name state type)
    (setMemory (a_memoryStructure (removeVarWM name state type (a_getWorkingMem memory)) (a_getClassDefns memory)))
    )
  )
;this should return the ENTIRETY of workingMem, w modified state
(define removeVarWM
  (lambda (name state type workingMem)
    (cond
      ((null? workingMem) (error "removeVarWM: State undefined"))
      ((and (eq? state (a_getStateName (topState workingMem))) (eq? type (a_getStateType (topState workingMem)))) (cons (removeVarWM2 name (topState workingMem)) (subStates workingMem)))
      (else (cons (topState workingMem) (removeVarWM name state type (subStates workingMem))))
      )
    )
  )
;this should return modified state
(define removeVarWM2
  (lambda (name state)
    (a_state (a_getStateName state) (a_getStateType state) (removeVarHelper name (a_getStateLayers state)))
    )
  )
;returns modified layers
(define removeVarHelper
  (lambda (n state)
    (cond
      ((substate? (a_topLayer state)) (removeVarHelper n (a_subLayers state)))
      ((eq? (removeVarLayer n (a_topLayer state)) 'error)
       (cons (a_topLayer state) (removeVarHelper n (a_subLayers state))))
      ((null? (a_subLayers state)) (list (removeVarLayer n (a_topLayer state))))
      (else (cons (removeVarLayer n (a_topLayer state)) (a_subLayers state)))
      )
    )
  )
(define removeVarLayer
  (lambda (n layer)
    (cond
      ((not (varExistsLayer? n layer)) 'error)
      (else (a_layer (removeIndex (a_varList layer) (getVarIndex n (a_varList layer) 0) 0) (removeIndex (a_valList layer) (getVarIndex n (a_varList layer) 0) 0) (a_throwVal layer) (a_fxnList layer)))
      )
    )
  )

(define checkVar
  (lambda (name state type)
    (checkVarWM name state type (a_getWorkingMem memory))
    )
  )
(define checkVarWM
  (lambda (name state type workingMem)
    (cond
      ((null? workingMem) #f)
      ((and (eq? state (a_getStateName (topState workingMem))) (eq? type (a_getStateType (topState workingMem)))) (checkVar2 name (topState workingMem)))
      (else (checkVarWM name state type (subStates workingMem)))
      )
    )
  )
(define checkVar2
  (lambda (x state)
    (varExists? x (a_getStateLayers state))
    )
  )

;will return var of a specified state USE CHECKVAR BEFORE GETTING
(define getVar
  (lambda (name state type)
    (getVarWM name state type (a_getWorkingMem memory))
    )
  )
(define getVarWM
  (lambda (name state type workingMem)
    (cond
      ((null? workingMem) (error "getVarWM: State undefined"))
      ((and (eq? state (a_getStateName (topState workingMem))) (eq? type (a_getStateType (topState workingMem)))) (getVar2 name (topState workingMem)))
      (else (getVarWM name state type (subStates workingMem)))
      )
    )
  )
(define getVar2
  (lambda (x state)
    (getVarHelper x (a_getStateLayers state))
    )
  )
(define getVarHelper
  (lambda (x state)
    (cond
      ((varExistsLayer? x (a_topLayer state)) (getVarLayer x (a_topLayer state)))
      ((varExistsSubState? x (a_subLayers state)) (getVarHelper x (a_subLayers state)))
      ((varExistsLayer? x (getGlobalLayer state))  (getVarLayer x (getGlobalLayer state)))
      (else '()) ;requested that it returns an empty list if it cant find the var - to prevent execution termination
      )
    )
  )
(define getVarLayer
  (lambda (x layer)
    (cond
      ((getVarHelper2 x (a_varList layer) (a_valList layer)))
      )
    )
  )

(define getVarHelper2
  (lambda (x var val)
    (cond
      ((null? var) 'error)
      ((eq? x (car var)) (car val))
      (else (getVarHelper2 x (cdr var) (cdr val)))
      )
    )
  )


;fxn stuff
(define defineFxn
  (lambda (fxnObj state type)
    (setMemory (a_memoryStructure (defineFxnWM fxnObj state type (a_getWorkingMem memory)) (a_getClassDefns memory)))
    )
  )
;should return all states
(define defineFxnWM
  (lambda (fxnObj state type workingMem)
    (cond
      ((null? workingMem) (error "defineFxnWM: State undefined"))
      ((and (eq? state (a_getStateName (topState workingMem))) (eq? type (a_getStateType (topState workingMem)))) (cons (defineFxnWM2 fxnObj (topState workingMem)) (subStates workingMem)))
      (else (cons (topState workingMem) (defineFxnWM fxnObj state type (subStates workingMem))))
      )
    )
  )
;this should return modified state
(define defineFxnWM2
  (lambda (fxnObj state)
    (a_state (a_getStateName state) (a_getStateType state) (defineFxnHelper fxnObj (a_getStateLayers state)))
    )
  )
;returns modified layer(s)
(define defineFxnHelper
  (lambda (fxnObj layer)
    (cond
      ((null? (a_subLayers layer)) (list (a_layer (a_varList (a_topLayer layer)) (a_valList (a_topLayer layer)) (a_throwVal (a_topLayer layer)) (cons fxnObj (a_fxnList (a_topLayer layer))))))
      ((fxnExistsLayer? (a_getFxnName fxnObj) (a_topLayer layer)) (error "defineFxnHelper: Function already defined"))
      (else (cons (a_topLayer layer) (defineFxnHelper fxnObj (a_subLayers layer))))
      )
    )
  )

;returns specified function
(define getFxn
  (lambda (name state type)
    (getFxnWM name state type (a_getWorkingMem memory))
    )
  )
(define getFxnWM
  (lambda (name state type workingMem)
    (cond
      ((null? workingMem) (error "getFxnWM: State undefined"))
      ((eq? state (a_getStateName (topState workingMem))) (getFxn2 name (topState workingMem)))
      (else (getFxnWM name state type (subStates workingMem)))
      )
    )
  )
(define getFxn2
  (lambda (x state)
    (getFxnHelper x (a_getStateLayers state))
    )
  )
(define getFxnHelper
  (lambda (x state)
    (cond
      ((null? state) '())
      ((fxnExistsFxnList? x (a_fxnList (a_topLayer state))) (getFxnLayer x (a_fxnList (a_topLayer state))))
      (else (getFxnHelper x (a_subLayers state)))
      )
    )
  )

(define getFxnLayer
  (lambda (name fxnlist)
    (cond
      ((null? fxnlist) (error "getFxnLayer: Function not/no longer defined"))
      ((eq? name (car (car fxnlist))) (list (a_getFxnArgs (car fxnlist)) (a_getFxnBody (car fxnlist)))) ;returns as (args body)
      (else (getFxnLayer name (cdr fxnlist)))
      )
    )
  )

;helper functions
(define getVarIndex
  (lambda (n a_varList i)
    (cond
       ((eq? n (car a_varList)) i)
       (else (getVarIndex n (cdr a_varList) (+ i 1)))
       )
    )
  )

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

;(varExists? x layers)
;Returns whether or not a variable has been initialized.
(define varExists?
  (lambda (x layers)
    (cond
      ((null? layers) #f)
      ((varExistsLayer? x (a_topLayer layers)) #t)
      (else (varExists? x (a_subLayers layers)))
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
      ((list? (a_varList layer)) (varExistsLayer? x (a_varList layer)))
      ((eq? x (car layer)) #t)
      (else (varExistsLayer? x (cdr layer)))
      )
    )
  )

(define getGlobalLayer
  (lambda (state)
    (cond
      ((null? (a_subLayers state)) (a_topLayer state))
      (else (getGlobalLayer (a_subLayers state)))
      )
    )
  )

(define varExistsSubState?
  (lambda (x state)
    (cond
      ((null? state) #f)
      ((substate? (a_topLayer state)) #f)
      ((varExistsLayer? x (a_topLayer state)) #t)
      (else (varExistsSubState? x (a_subLayers state)))
      )
    )
  )

(define fxnExists?
  (lambda (name state)
    (cond
      ((null? state) #f)
      ((substate? (a_topLayer state)) (fxnExists? name (a_subLayers state)))
      ((fxnExistsLayer? name (a_topLayer state)) #t)
      (else (fxnExists? name (a_subLayers state)))
      )
    )
  )

(define fxnExistsLayer?
  (lambda (name layer)
    (cond
      ((null? layer) #f)
      ((fxnExistsFxnList? name (a_fxnList layer)) #t)
      (else #f)
      )
    )
  )

(define fxnExistsFxnList?
  (lambda (name fxnlist)
    (cond
      ((null? fxnlist) #f)
      ((eq? name (car (a_getFxnName fxnlist))) #t)
      (else (fxnExistsFxnList? name (cdr fxnlist)))
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

;this initializes the memory properly
(define initMem
  (lambda ()
    (setMemory (a_memoryStructure '() '()))
    (display "state.scm initialized\n")
    )
  )

;basic function usage/testing
;(initMem)
;(defineClass (a_class 'x null '()))
;(defineClass (a_class 'y 'x '()))
;(defineState (a_state 'name 'x newLayers))
;(defineState (a_state 'name2 'y newLayers))
;(addVar 'x 5 'name 'x)
;(pushSubState)
;(addVar 'y 4 'name2 'y)
;(pushLayer newLayer)
;(setVar 'y 5 'name2 'y)
;(getVar 'y 'name2 'y) ;should work, not really tested
;(setVar 'x 4 'name 'x)
;(removeVar 'y 'name2 'y)
;(removeVar 'x 'name 'x)
;(defineFxn (a_fxn 'name 'args 'body) 'name 'x)
;(getFxn 'name 'name 'x)
;(checkVar 'x 'name 'y)


