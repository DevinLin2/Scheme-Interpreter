#lang racket
(require "simpleParser.rkt")
;___________________
; Devin Lin
; Caroline Schafer
; Kamsiyochukwu Eneh
;___________________

; renamed some commonly used operations for abstraction
(define operator car)

(define block_operator caar)

(define leftoperand cadr)

(define rightoperand caddr)

(define rightoperand_list cddr)

(define first_element car)

(define first_element_in_var_list caar)

(define first_element_in_value_list caadr)

(define second_element cadr)

(define rest_of_elements cdr)

(define top_layer car)

(define top_layer_var_list caar)

(define condition car)

(define then cadr)

(define multiline_body_check cdr)

(define else_check cddr)

(define else caddr)

(define conditional_syntax_format cdar)

(define initial_state '((()())))

(define state_layer '(()()))

(define add_state_layer
  (lambda (state)
    (cons state_layer state)))

(define remove_state_layer
  (lambda (state)
    (rest_of_elements state)))

; rest_of_state_layer: this is a helper function that takes a state layer (for example: ((a b c) (1 2 3))) and returns the state without the first binding pairs e.g. ((b c) (2 3))
(define rest_of_state_layer
  (lambda (state_layer)
    (list (rest_of_elements (first_element state_layer)) (rest_of_elements (second_element state_layer)))))

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
  (lambda (syntax state return)
    (cond
      ((M_value_exists (leftoperand syntax) (top_layer_var_list state)) (error "the variable has already been declared"))
      ((null? (rightoperand_list syntax)) (cons (M_state_variable_declare (leftoperand syntax) (top_layer state)) (rest_of_elements state)))
      ((or (boolean? (rightoperand syntax)) (number? (rightoperand syntax))) (cons (M_state_variable_declare (list (leftoperand syntax) (rightoperand syntax)) (top_layer state)) (rest_of_elements state)))
      (else (cons (M_state_variable_declare (list (leftoperand syntax) (M_value_expression (rightoperand syntax) state)) (top_layer state)) (rest_of_elements state))))))

; M_state_assignment: called when assigning a value to a variable. Checks if the variable has been declared starting from the top most state layer and working down. If it has, replace its binding with the new value.
(define M_state_assignment
  (lambda (syntax state original_state return)
    (cond
      ((null? state) (error "an element used has not been declared"))
      ((M_value_exists (leftoperand syntax) (top_layer_var_list state)) (return (cons (M_state_assignment_helper syntax (first_element state) original_state (lambda (v) v)) (rest_of_elements state))))
      (else (M_state_assignment syntax (rest_of_elements state) original_state (lambda (v) (return (cons (first_element state) v))))))))

; M_state_assignment_helper: this is called when the variable is found in the state (in any layer). This function will return a state with the new binding for the given variable.
(define M_state_assignment_helper
  (lambda (syntax state_layer original_state return)
    (cond
      ((and (eq? (leftoperand syntax) (first_element_in_var_list state_layer)) (or (number? (rightoperand syntax)) (or (eq? (rightoperand syntax) 'true) (eq? (rightoperand syntax) 'false)))) (return (list (cons (leftoperand syntax) (rest_of_elements (first_element state_layer))) (cons (rightoperand syntax) (rest_of_elements (second_element state_layer))))))
      ((and (eq? (leftoperand syntax) (first_element_in_var_list state_layer)) (not (pair? (rightoperand syntax)))) (return (list (cons (leftoperand syntax) (rest_of_elements (first_element state_layer))) (cons (M_value_lookup (rightoperand syntax) original_state) (rest_of_elements (second_element state_layer))))))
      ((eq? (leftoperand syntax) (first_element_in_var_list state_layer)) (return (list (cons (leftoperand syntax) (rest_of_elements (first_element state_layer))) (cons (M_value_expression (rightoperand syntax) original_state) (rest_of_elements (second_element state_layer))))))
      (else (M_state_assignment_helper syntax (rest_of_state_layer state_layer) original_state (lambda (v) (return (list (cons (first_element_in_var_list state_layer) (first_element v)) (cons (first_element_in_value_list state_layer) (second_element v))))))))))

; M_state_if: called when performing an if operation. Checks if the conditional evaluates to a boolean.
;             If it does and is true, call M_state_then to eveluate the then statement, otherwise call M_state_else to evaluate the else statement
(define M_state_if
  (lambda (syntax state)
    (cond
      ((not (boolean? (M_value_expression (condition syntax) state))) (error "invalid conditional"))
      ((M_value_expression (condition syntax) state) (M_state_then (rest_of_elements (then syntax)) (add_state_layer state)))
      ((not (null? (else_check syntax))) (M_state_else (rest_of_elements (else syntax)) (add_state_layer state)))
      (else state))))

; M_state_then: called by M_state_if when the conditional is true. Checks if the then statement is a list of statememts.
;               If it is, function recursively calls itself to parse the list. Otherwise, function calls the respective M_state functions depending on the operation in syntax.
;***** make sure to check for code blocks with begin here ***************
(define M_state_then
  (lambda (syntax state)
    (cond
      ((null? syntax) (remove_state_layer state))
      ((not (null? (multiline_body_check syntax))) (M_state_then (rest_of_elements syntax) (M_state_then (first_element syntax) state)))
      ((eq? (block_operator syntax) 'if) (M_state_if (conditional_syntax_format syntax) state))
      ((eq? (block_operator syntax) '=) (M_state_assignment (first_element syntax) state state (lambda (v) v)))
      ((eq? (block_operator syntax) 'var) (M_state_declare (first_element syntax) state (lambda (v) v)))
      ((eq? (block_operator syntax) 'return) (M_state_return (first_element syntax) state))
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
      ((eq? (operator syntax) 'if) (M_state_if (cdar syntax) state))
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
      ((not (pair? (cdr syntax))) (M_value_lookup (leftoperand syntax)))
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
      ((not (pair? syntax)) (M_value_lookup syntax state))
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

; value_exists: takes an element and a list of variables and checks if the element is in the list
(define M_value_exists
  (lambda (elem var_list)
    (cond
      ((null? var_list) #f)
      ((eq? (first_element var_list) elem) #t)
      (else (M_value_exists elem (rest_of_elements var_list))))))

; M_state_variable_declare: takes an element and an optional value and adds it to the top layer state
(define M_state_variable_declare
  (lambda (elem top_layer_state)
    (cond
      ((list? elem) (list (cons (first_element elem) (first_element top_layer_state)) (cons (second_element elem) (second_element top_layer_state))))
      (else (list (cons elem (first_element top_layer_state)) (cons 'null (second_element top_layer_state)))))))
    

; lookup_value: takes an element and a state and checks if the element is in the state, returns the value of the element
(define M_value_lookup
  (lambda (elem state)
    (cond
      ((null? state) (error "an element used has not been declared"))
      ((M_value_exists elem (top_layer_var_list state)) (M_value_lookup_helper elem (first_element state)))
      (else (M_value_lookup elem (rest_of_elements state))))))

; M_value_lookup_helper: this is called when the element being looked up exists in the state. If the element has been assigned a value, return the value.
(define M_value_lookup_helper
  (lambda (elem state_layer)
    (cond
      ((and (eq? elem (first_element_in_var_list state_layer)) (eq? 'null (first_element_in_value_list state_layer))) (error "an element used has not been assigned a value"))
      ((eq? elem (first_element_in_var_list state_layer)) (first_element_in_value_list state_layer))
      (else (M_value_lookup_helper elem (rest_of_state_layer state_layer))))))


