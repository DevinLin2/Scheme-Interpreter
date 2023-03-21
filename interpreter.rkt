#lang racket
(require "simpleParser.rkt")
;___________________
; Devin Lin
; Caroline Schafer
; Kamsiyochukwu Eneh
;___________________

; renamed some commonly used operations for abstraction
(define operator car)

(define leftoperand cadr)

(define rightoperand caddr)

(define rightoperand_list cddr)

(define first_element car)

(define second_element cadr)

(define rest_of_elements cdr)

(define top_layer car)

(define top_layer_var_list caar)


(define condition car)

(define then cadr)

(define else caddr)

; interpret: takes a filename and calls the heavylifting interpret function with the parse tree
(define interpret
  (lambda (filename)
    (interpret2 (parser filename) '())))

; interpret2: parses through the input parse tree and calls the appropriate M_state functions depending on the operator
(define interpret2
  (lambda (PT state)
    (cond
      ((and (null? PT) (list? state)) '())
      ((null? PT) state)
      ((eq? (caar PT) 'return) (M_state_return (car PT) state))
      ((eq? (caar PT) 'var) (interpret2 (cdr PT) (M_state_declare (car PT) state)))
      ((eq? (caar PT) 'if) (interpret2 (cdr PT) (M_state_if (cdar PT) state)))
      ((eq? (caar PT) 'while) (interpret2 (cdr PT) (M_state_while (cdar PT) state)))
      (else (interpret2 (cdr PT) (M_state_assignment (car PT) state state))))))

; M_state_declare: called when declaring a variable. Checks the top layer if variable is already declared. If not it adds the variable and the optional value to the state.
(define M_state_declare
  (lambda (syntax state)
    (cond
      ((M_state_lookup_declare (leftoperand syntax) (top_layer_var_list state)) (error "the variable has already been declared"))
      ((null? (rightoperand_list syntax)) (cons (M_state_variable_declare (leftoperand syntax) (top_layer state)) (rest_of_elements state)))
      ((or (boolean? (rightoperand syntax)) (number? (rightoperand syntax))) (cons (M_state_variable_declare (list (leftoperand syntax) (rightoperand syntax)) (top_layer state)) (rest_of_elements state)))
      (else (cons (M_state_variable_declare (list (leftoperand syntax) (M_value_expression (rightoperand syntax) state)) (top_layer state)) (rest_of_elements state))))))

; M_state_assignment: called when assigning a value to a variable. Checks if the variable has been declared. If it has, delete its old entry (variable) from state and add a new entry of (variable value) to the state.
(define M_state_assignment
  (lambda (syntax state original_state)
    (cond
      ((null? state) (error "an element used has not been declared"))
      ((and (eq? (leftoperand syntax) (caar state)) (or (number? (rightoperand syntax)) (or (eq? (rightoperand syntax) 'true) (eq? (rightoperand syntax) 'false)))) (cons (cdr syntax) (cdr state)))
      ((and (eq? (leftoperand syntax) (caar state)) (not (pair? (rightoperand syntax)))) (cons (list (leftoperand syntax) (M_state_lookup_value (rightoperand syntax) original_state)) (cdr state)))
      ((eq? (leftoperand syntax) (caar state)) (cons (list (leftoperand syntax) (M_value_expression (rightoperand syntax) original_state)) (cdr state)))
      (else (cons (car state) (M_state_assignment syntax (cdr state) original_state))))))

; M_state_if: called when performing an if operation. Checks if the conditional evaluates to a boolean.
;             If it does and is true, call M_state_then to eveluate the then statement, otherwise call M_state_else to evaluate the else statement
(define M_state_if
  (lambda (syntax state)
    (cond
      ((not (boolean? (M_value_expression (condition syntax) state))) (error "invalid conditional"))
      ((M_value_expression (condition syntax) state) (M_state_then (then syntax) state))
      ((not (null? (cddr syntax))) (M_state_else (else syntax) state))
      (else state))))

; M_state_then: called by M_state_if when the conditional is true. Checks if the then statement is a list of statememts.
;               If it is, function recursively calls itself to parse the list. Otherwise, function calls the respective M_state functions depending on the operation in syntax.
(define M_state_then
  (lambda (syntax state)
    (cond
      ((null? syntax) state)
      ((list? (car syntax)) (M_state_then (cdr syntax) (M_state_then (car syntax) state)))
      ((eq? (operator syntax) 'if) (M_state_if (cdr syntax) state))
      ((eq? (operator syntax) '=) (M_state_assignment syntax state state))
      ((eq? (operator syntax) 'var) (M_state_declare syntax state))
      ((eq? (operator syntax) 'return) (M_state_return syntax state))
      (else state))))

; M_state_else: called by M_state_if when the conditional is false. Checks if the else statement is a list of statememts.
;               If it is, function recursively calls itself to parse the list. Otherwise, function calls the respective M_state functions depending on the operation in syntax.
;               Note that the operation could be another 'if' denoting an 'else if' operation. This is handled the same way as the other operations.
; this could be combined with M_state_then to be one function but for semantic purposes this makes more sense
(define M_state_else
  (lambda (syntax state)
    (cond
      ((null? syntax) state)
      ((list? (car syntax)) (M_state_else (cdr syntax) (M_state_else (car syntax) state)))
      ((eq? (operator syntax) 'if) (M_state_if (cdr syntax) state))
      ((eq? (operator syntax) '=) (M_state_assignment syntax state state))
      ((eq? (operator syntax) 'var) (M_state_declare syntax state))
      ((eq? (operator syntax) 'return) (M_state_return syntax state))
      (else state))))

; M_state_while: called when performing a while operation. Checks if the condition evaluates to a boolean.
;                If the condition is true, recursively call M_state_while passing in the new state after evaluating the body with M_state_while_body. Otherwise exit the function and return the current state.
(define M_state_while
  (lambda (syntax state)
    (cond
      ((not (boolean? (M_value_expression (condition syntax) state))) error "invalid conditional")
      ((M_value_expression (condition syntax) state) (M_state_while syntax (M_state_while_body (leftoperand syntax) state)))
      (else state))))

; M_state_while_body: called by M_state_while when the condition is true. Evaluates the body of the while loop and returns the new state.
(define M_state_while_body
  (lambda (syntax state)
    (cond
      ((null? syntax) state)
      ((list? (car syntax)) (M_state_while_body (cdr syntax) (M_state_while_body (car syntax) state)))
      ((eq? (operator syntax) 'if) (M_state_if (cdr syntax) state))
      ((eq? (operator syntax) '=) (M_state_assignment syntax state state))
      ((eq? (operator syntax) 'var) (M_state_declare syntax state))
      ((eq? (operator syntax) 'return) (M_state_return syntax state))
      (else state))))

; M_state_return: called when returning a value. If the value is a number or boolean, return it. If the value is an expression, return the evaluation of the expression. If the value is a variable, look it up in the state and return it.
(define M_state_return
  (lambda (syntax state)
    (cond
      ((number? (cdr syntax)) (cdr syntax))
      ((eq? (cdr syntax) #t) "true")
      ((eq? (cdr syntax) #f) "false")
      ((not (pair? (cdr syntax))) (M_state_lookup_value (leftoperand syntax)))
      (else (M_state_return (cons (car syntax) (M_value_expression (leftoperand syntax) state)) state)))))

;____________________________________________
;HELPER METHODS
;____________________________________________

; M_value_expression: Evaluates an expression. Determines the type of operation being performed and calls the respective M_value functions. 
(define M_value_expression
  (lambda (syntax state)
    (cond
      ((eq? syntax 'true) #t)
      ((eq? syntax 'false) #f)
      ((or (number? syntax) (boolean? syntax) (string? syntax)) syntax)
      ((not (pair? syntax)) (M_state_lookup_value syntax state))
      ((and (list? syntax) (eq? (operator syntax) '!)) (M_value_conditional_expression '! (M_value_expression (leftoperand syntax) state) '()))
      ((and (list? syntax) (eq? (operator syntax) '+))(M_value_arithmetic_expression '+ (M_value_expression (leftoperand syntax) state) (M_value_expression (rightoperand syntax) state)))
      ((and (list? syntax) (eq? (operator syntax) '-) (null? (cddr syntax))) (M_value_arithmetic_expression '* -1 (M_value_expression (leftoperand syntax) state)))
      ((and (list? syntax) (eq? (operator syntax) '-))(M_value_arithmetic_expression '- (M_value_expression (leftoperand syntax) state) (M_value_expression (rightoperand syntax) state)))
      ((and (list? syntax) (eq? (operator syntax) '*)) (M_value_arithmetic_expression '* (M_value_expression (leftoperand syntax) state) (M_value_expression (rightoperand syntax) state)))
      ((and (list? syntax) (eq? (operator syntax) '/)) (M_value_arithmetic_expression '/ (M_value_expression (leftoperand syntax) state) (M_value_expression (rightoperand syntax) state)))
      ((and (list? syntax) (eq? (operator syntax) '%)) (M_value_arithmetic_expression '% (M_value_expression (leftoperand syntax) state) (M_value_expression (rightoperand syntax) state)))
      ((and (list? syntax) (eq? (operator syntax) '==)) (M_value_conditional_expression '== (M_value_expression (leftoperand syntax) state) (M_value_expression (rightoperand syntax) state)))
      ((and (list? syntax) (eq? (operator syntax) '!=)) (M_value_conditional_expression '!= (M_value_expression (leftoperand syntax) state) (M_value_expression (rightoperand syntax) state)))
      ((and (list? syntax) (eq? (operator syntax) '<)) (M_value_conditional_expression '< (M_value_expression (leftoperand syntax) state) (M_value_expression (rightoperand syntax) state)))
      ((and (list? syntax) (eq? (operator syntax) '>)) (M_value_conditional_expression '> (M_value_expression (leftoperand syntax) state) (M_value_expression (rightoperand syntax) state)))
      ((and (list? syntax) (eq? (operator syntax) '<=)) (M_value_conditional_expression '<= (M_value_expression (leftoperand syntax) state) (M_value_expression (rightoperand syntax) state)))
      ((and (list? syntax) (eq? (operator syntax) '>=)) (M_value_conditional_expression '>= (M_value_expression (leftoperand syntax) state) (M_value_expression (rightoperand syntax) state)))
      ((and (list? syntax) (eq? (operator syntax) '&&)) (M_value_conditional_expression '&& (M_value_expression (leftoperand syntax) state) (M_value_expression (rightoperand syntax) state)))
      ((and (list? syntax) (eq? (operator syntax) '||)) (M_value_conditional_expression '|| (M_value_expression (leftoperand syntax) state) (M_value_expression (rightoperand syntax) state)))
      (else (error "invalid/unknown syntax")))))

; M_value_conditional_expression: evaluates a conditional expression
(define M_value_conditional_expression
  (lambda (operator expression1 expression2)
    (cond
      ((or (and (number? expression1) (boolean? expression2)) (and (number? expression2) (boolean? expression1))) (error "type mismatch"))
      ((and (eq? operator '!) (not (boolean? expression1))) (error "invalid type for ! operator"))
      ((and (or (eq? operator '&&) (eq? operator '||)) (not (and (boolean? expression1) (boolean? expression2)))) (error "invalid type for operator"))
      ((eq? operator '!) (not expression1))
      ((eq? operator '==) (eq? expression1 expression2))
      ((eq? operator '!=) (not (eq? expression1 expression2)))
      ((eq? operator '<) (< expression1 expression2)) ; this should only be used to compare numbers or strings but scheme already crashes if there is an invalid type
      ((eq? operator '>) (> expression1 expression2))
      ((eq? operator '<=) (<= expression1 expression2))
      ((eq? operator '>=) (>= expression1 expression2))
      ((eq? operator '&&) (and expression1 expression2))
      ((eq? operator '||) (or expression1 expression2))
      (else (error "invalid/unknown syntax")))))

; M_value_arithmetic_expression: evaluates an arithmetic expression
(define M_value_arithmetic_expression
  (lambda (operator expression1 expression2)
    (cond
      ((not (and (number? expression1) (number? expression2))) (error "invalid types for arithmetic operator"))
      ((eq? operator '+) (+ expression1 expression2))
      ((eq? operator '-) (- expression1 expression2))
      ((eq? operator '*) (* expression1 expression2))
      ((and (eq? operator '/) (< (/ expression1 expression2) 0)) (exact-ceiling (/ expression1 expression2)))
      ((eq? operator '/) (exact-floor (/ expression1 expression2))) ; used exact-floor to make this integer division
      ((eq? operator '%) (remainder expression1 expression2))
      (else (error "invalid/unknown syntax")))))

; lookup_declare: takes an element and a state and checks if the element is in the state
(define M_state_lookup_declare
  (lambda (elem var_list)
    (cond
      ((null? var_list) #f)
      ((eq? (first_element var_list) elem) #t)
      (else (M_state_lookup_declare elem (rest_of_elements var_list))))))

; M_state_variable_declare: takes an element and an optional value and adds it to the top layer state
(define M_state_variable_declare
  (lambda (elem top_layer_state)
    (cond
      ((list? elem) (list (cons (first_element elem) (first_element top_layer_state)) (cons (second_element elem) (second_element top_layer_state))))
      (else (list (cons elem (first_element top_layer_state)) (cons '$ (second_element top_layer_state)))))))
    

; lookup_value: takes an element and a state and checks if the element is in the state, returns the value of the element
(define M_state_lookup_value
  (lambda (elem state)
    (cond
      ((null? state) (error "an element used has not been declared"))
      ((and (eq? (caar state) elem) (null? (cdar state))) (error "an element used has not been assigned a value"))
      ((eq? (caar state) elem) (cadar state))
      (else (M_state_lookup_value elem (cdr state))))))


