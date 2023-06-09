;-----------------------
; Devin Lin
;-----------------------

#lang racket
(require "classParser.rkt")
; (load "functionParser.scm")

; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
(define call/cc call-with-current-continuation)

; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
#|(define interpret
  (lambda (file)
    (scheme->language
     (call/cc
      (lambda (return)
        (interpret-statement-list (parser file) (newenvironment) return
                                  (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  (lambda (v env) (myerror "Uncaught exception thrown"))))))))|#

; The main function. Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file)
    (call/cc
     (lambda (main-return)
       (execute-main (interpret-outer-program (parser file) (newenvironment)) (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Throw used outside of try/catch")) main-return)))))

; Looks up main in the state returned by interpret-outer-program and executes it THIS NEEDS TO BE CHANGED
(define execute-main
  (lambda (state break throw main-return)
    (main-return (scheme->language
        (M-state-function (get-function-body (lookup 'main state)) 'main main-args state break throw (lambda (v) v)))))) ; THIS WAS CHANGED TO BE (lambda (v) v) insted of return from the call/cc which makes sense but I'm not sure if it is correct

; we need a new function that is similar to interpret-statement-list which will only be ran once for the "outer layer" of the program
; this interpret will do all of the variable assignments and declarations along with function definitions and the main interpret function will call this instead of interpret-statement-list
; this function should only return a state and nothing more. We will write another function once this completes to find the main function in the program and run it.
(define interpret-outer-program
  (lambda (statement-list state)
    (if (null? statement-list)
        state
        (interpret-outer-program (cdr statement-list) (interpret-outer-statement (car statement-list) state)))))

(define newInterpret
  (lambda (code classes)
    (cond
      ((null? code) classes)
      ((eq? (caar code) 'class) (newInterpret (cdr code) (bindClassClosure (cadar code)(createClassClosure (car code)) classes)))
      (else (newInterpret (cdr code))))))


; interprets a line (or function) of the outer layer of the program and adds the necessary bindings to the state
; We will reuse the interpret-declare and interpret-assign functions but pass in 'null for the continuations because there should not be any continuations being used in the initial outer interpret.
; We just want to parse the program not run it which will be the job of execute-main.
(define interpret-outer-statement
  (lambda (statement state)
    (cond
      ((eq? 'var (statement-type statement)) (interpret-outer-declare statement state)) ;*** this needs to support function calls (value of) being a valid thing to be assigned to a variable (INCOMPLETE)
      ((eq? '= (statement-type statement)) (interpret-assign statement state (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Throw used outside of try/catch")) (lambda(v) v))) ; similar to above ^^^ (INCOMPLETE)
      ((eq? 'function (statement-type statement)) (interpret-function statement state))
      (else (myerror "Invalid statement:" (statement-type statement))))))

; interprets a function by creating its closure and binding it to the state
(define interpret-function
  (lambda (statement state)
    (insert (get-func-name statement) (createClosure statement state) state)))
    
; interprets a list of statements.  The environment from each statement is used for the next ones. <-- This is no longer true. We need to copy the base state of each statement into the new one
(define interpret-statement-list
  (lambda (statement-list current-type environment return break continue throw)
    (cond 
      ((null? statement-list) environment)
      ; this is wrong because calling functions but not returning them will make this function return
      ((not (pair? (interpret-statement (car statement-list) current-type environment return break continue throw))) (return (interpret-statement (car statement-list) current-type environment return break continue throw)))
      (else (interpret-statement-list (cdr statement-list) current-type (interpret-statement (car statement-list) environment return break continue throw) return break continue throw)))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement current-type environment return break continue throw)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement current-type environment break throw return))
      ((eq? 'ignored-return (statement-type statement)) environment)
      ((eq? 'var (statement-type statement)) (interpret-declare statement current-type environment break throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement current-type environment break throw return))
      ((eq? 'if (statement-type statement)) (interpret-if statement current-type environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement current-type environment break throw return))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement current-type environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement current-type environment break return throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement current-type environment return break continue throw))
      ((eq? 'function (statement-type statement)) (interpret-function statement current-type environment))
      ; if we see a 'funcall this means that the function's return value should be ignored
      ((eq? 'funcall (statement-type statement)) (pop-frame (M-state-function (ignore-return (get-function-body (lookup (get-func-name statement) environment))) (get-func-name statement) (arg-list statement) current-type environment break throw return)))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement current-type environment break throw return)
    (return (eval-expression (get-expr statement) current-type environment break throw return))))

; removes the return from functions that we should ignore the return for
(define ignore-return
  (lambda (body)
    (cond
      ((null? body) body)
      ((list? (car body)) (cons (ignore-return (car body)) (ignore-return (cdr body))))
      ((eq? (car body) 'return) (list 'ignored-return))
      (else (cons (car body) (ignore-return (cdr body)))))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement current-type environment break throw)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (call/cc (lambda (return) (eval-expression (get-declare-value statement) current-type environment break throw return))) environment)
        (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement current-type environment break throw return)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) current-type environment break throw return) environment)))

; New interpret-declare function which will be used to assign function values to global variables
(define interpret-outer-declare
  (lambda (statement environment)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (call/cc (lambda (return) (eval-expression (get-declare-value statement) environment (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Throw used outside of try/catch")) return))) environment); this will be used to return the value of functions when assigning them to a global variable
        (insert (get-declare-var statement) 'novalue environment))))
       
; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement current-type environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) current-type environment break throw return) (interpret-statement (get-then statement) current-type environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) current-type environment return break continue throw))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement current-type environment break throw return)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition current-type environment break throw return)
                            (loop condition body (interpret-statement body current-type environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement current-type environment return break continue throw)
    (pop-frame (interpret-statement-list (cdr statement)
                                         current-type 
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement current-type environment break throw return)
    (throw (eval-expression (get-expr statement) current-type environment break return throw))))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement current-type environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block current-type env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     current-type 
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement current-type environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block current-type environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block current-type env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block current-type env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) current-type environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          current-type 
                          (interpret-block try-block current-type environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr current-type environment break throw return)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr current-type environment))
      (else (eval-operator expr current-type environment break throw return)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr current-type environment break throw return)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) current-type environment break throw return)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) current-type environment break throw return)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment break throw return) current-type environment break throw return)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value current-type environment break throw return)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) current-type environment break throw return)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) current-type environment break throw return)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) current-type environment break throw return)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) current-type environment break throw return)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) current-type environment break throw return)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) current-type environment break throw return)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) current-type environment break throw return))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) current-type environment break throw return)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) current-type environment break throw return)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) current-type environment break throw return)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) current-type environment break throw return)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) current-type environment break throw return)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) current-type environment break throw return)))
      ;((eq? 'closure op1value) (return (list expr op1value environment)))
      ((eq? 'funcall (operator expr)) (M-state-function (operand1 op1value) (get-func-name expr) (arg-list expr) current-type environment break throw return))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))

;FIGURE OUT WHAT CURRENT-TYPE IS USED FOR IN M_STATE_FUNCTION************ prob has smth to do with hint 2?
; M state function to evaluate a function when a function is called. It will call on a helper function and pass it the function state from the closure.
(define M-state-function
  (lambda (body name args current-type state break throw return)
    (M-state-eval-function-body
     body 
     (update-closure (bind-parameters (closure-formal-params (lookup name state)) args (closure-func-state (lookup name state)) state break throw return) name (closure-formal-params (lookup name state)) body) ; bind parameters to state and use this as function state
     (lambda (s) (error "error: break out of loop"))
     throw
     return))) ; this last part for the return continuation might be wrong

; Executes when a function is being called recursively. Updates and returns the function state with a new closure for the recursively called function.
(define update-closure
  (lambda (fstate name formal-params body)
    (update name (list formal-params body fstate) fstate)))
            
; Evaluates the function body given the function's closure and updated state/function state     
(define M-state-eval-function-body
  (lambda (body current-type fstate break throw return)
    (interpret-statement-list body fstate return break (lambda (s) (error "error: continue out of loop")) throw)))

; Binds the actual parameters to the formal parameters and puts then bindings into the function state
(define bind-parameters
  (lambda (params args fstate state break throw return)
    (cond
      ((not (eq? (length params) (length args))) (error "Unexpected input size"))
      ((null? params) (copy-over-globals fstate state))
      (else (bind-parameters (rest-of-elements params) (rest-of-elements args) (insert (var-name params) (eval-expression (var-expr args) state break throw return) fstate) state break throw return)))))

(define copy-over-globals
  (lambda (fstate state)
    (if (null? (cdr fstate))
        (list (global-in-env state))
        (cons (car fstate) (copy-over-globals (cdr fstate) state)))))

(define global-in-env
  (lambda (state)
    (if (null? (cdr state))
        (car state)
        (global-in-env (cdr state)))))
;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define var-name car)
(define closure-formal-params car)
(define var-expr car)
(define rest-of-elements cdr)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)
(define arg-list cddr)
(define nested-body-check caar)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)
(define get-function-body operand1)
(define get-func-name operand1)
(define get-arg-list cddr)
(define closure-func-state operand2)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))

;------------------------
; Functions to create class closure 
;------------------------

; Function to get the super class 
(define getSuperClassName
  (lambda (syntax)
    (cond
      ((null? (caddr syntax)) '())
      (else (car(cdaddr syntax))))))

; Function to get list of class instance variables
(define getInstanceVariableNames
  (lambda (syntax)
    (cond
      ((null? syntax) null)
      ((eq? (caar syntax) 'function) (getInstanceVariableNames (cdr syntax)))
      ((eq? (caar syntax) 'static-function) (getInstanceVariableNames (cdr syntax)))
      (else (cons (cadar syntax) (getInstanceVariableNames (cdr syntax)))))))

; Function to get list of class instance initial values
(define getClassInstanceVariableValues
  (lambda (syntax)
    (cond
      ((null? syntax) null)
      ((eq? (caar syntax) 'function) null)
      ((null? (cddar syntax)) (cons 'novalue (getClassInstanceVariableValues (cdr syntax))))
      (else (cons (caddar syntax) (getClassInstanceVariableValues (cdr syntax)))))))

; Function to get list of function names
(define getClassFunctionNames
  (lambda (syntax)
    (cond
      ((null? syntax) null)
      ((or (eq? (caar syntax) 'function) (eq? (caar syntax) 'static-function)) (cons (cadar syntax) (getClassFunctionNames (cdr syntax))))
      (else (getClassFunctionNames (cdr syntax))))))

; Function to get a list of function closures
(define getClassFunctionClosures
  (lambda (syntax)
    (cond
      ((null? syntax) null)
      ((or (eq? (caar syntax) 'function) (eq? (caar syntax) 'static-function)) (cons (getFunctionClosure (car syntax)) (getClassFunctionClosures (cdr syntax))))
      (else (getClassFunctionClosures (cdr syntax))))))



(define createClassClosure
  (lambda (syntax)
    (cons (list (getSuperClassName syntax)) (cons (list (getInstanceVariableNames (getClassBody syntax)) (getClassInstanceVariableValues (getClassBody syntax))) (list (cons (getClassFunctionNames (getClassBody syntax)) (list (getClassFunctionClosures (getClassBody syntax)))))))))

(define getClassBody
  (lambda (syntax)
    (cadddr syntax)))

;------------------------
; Functions to access elements of the class closure. 
;------------------------

; Function to lookup a class closure from the class closures.
(define lookupClassClosure
  (lambda (c classes)
    (lookupClassClosureHelper c (car classes) (cadr classes))))

; Helper function to lookup a class closure from the state.
(define lookupClassClosureHelper
  (lambda (c lis1 lis2)
    (cond
      ((eq? (car lis1) c) (car lis2))
      (else (lookupClassClosureHelper c (cdr lis1) (cdr lis2))))))

; Function to get variable lists from class closure
(define lookupClassVarList
  (lambda (closure)
    (cadr closure)))

; Function to get the superclass of a class closure.
(define lookupSuperClass
  (lambda (closure)
    (cond
      ((null? (car closure)) null)
      (else (caar closure)))))



; Function to get the function names from a class closure.
(define lookupFunctionNames
  (lambda (closure)
    (cond
      ((null? (cddr closure)) null)
      (else (caaddr closure)))))

; Function to get the function closures from a class closure.
(define lookupFunctionClosures
  (lambda (closure)
    (cond
      ((null? (cddr closure)) null)
      (else (car(cdaddr closure))))))

;------------------------
; Functions to create an instance closure
;------------------------

; Function to get the Instance variable name. Syntax in the form: '(var a (new A))
(define getInstanceName
  (lambda (syntax)
    (cadr syntax)))

; Function to get the runtime type of a variable
(define getRuntimeType
  (lambda (syntax)
    (car(cdaddr syntax))))

(define getInstanceClosure
  (lambda (syntax classes)
    (append (list (getInstanceName syntax) (getRuntimeType syntax)) (list (getInstanceVariablesList (getRuntimeType syntax) classes)))))

(define getInstanceVariablesList
  (lambda (runtimeType classes)
    (lookupClassVarList (lookupClassClosure runtimeType classes))))

;------------------------
; Functions to Access and update members of an instance closure. 
;------------------------

(define lookupInstances
  (lambda (state)
    (caar state)))

(define lookupInstanceClosures
  (lambda (state)
    (cadar state)))

(define lookupInstanceClosure
  (lambda (v state)
    (cond
     ((eq? (car (topframe state)) v) (topframe state))
     (else (lookupInstanceClosure v (remainingframes state))))))

; function to get the runtime type of an instance variable.
(define lookupRuntimeType
  (lambda (closure)
    (cadr closure)))


;------------------------
; Environment/State Functions
;------------------------

(define getClassNamesList
  (lambda (classes)
    (car classes)))

(define getClassClosureList
  (lambda (classes)
    (cadr classes)))

; A function to add a new class closure to the classes.
(define bindClassClosure
  (lambda (className closure classes)
    (cons (addClassName className classes)(list (addClassClosure closure classes)))))

(define addClassName
  (lambda (className classes)
    (append (getClassNamesList classes) (list className))))

(define addClassClosure
  (lambda (closure classes)
    (append (getClassClosureList classes) (list closure))))

; A function to add a new instance closure to the state.
(define bindInstanceClosure
  (lambda (closure state)
    (cons closure state)))

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; Function to get a list of the formal parameters of a function definition
(define getFormalParams
  (lambda (syntax)
    (caddr syntax)))

; Function to get the body of a function given the syntax.
(define getFunctionBody
  (lambda (syntax)
    (cadddr syntax)))


; Function to create a closure
(define createClosure
  (lambda (syntax environment)
    (append (cons (getFormalParams syntax) (list (getFunctionBody syntax))) (list (insert (get-func-name syntax) 'closure (push-frame environment))))))

; Function to create a function closure
(define getFunctionClosure
  (lambda (syntax)
    (append (cons (append (getFormalParams syntax) '(this)) (list (getFunctionBody syntax))))))



; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; arguments for main
(define main-args '())
    
; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))


; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

;CHANGE LOOKUPS TO WORK FOR CURRENT-TYPE*************
; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var current-type environment)
    (lookup-variable var current-type environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var current-type environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined/out of scope:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

