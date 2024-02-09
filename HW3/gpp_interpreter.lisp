;; Author: Emre Oytun
;; HOW TO RUN THE PROGRAM :
;; 1) Load it to the lisp interpreter by using command: laod "gpp_interpreter.lisp"
;; 2) Call 'gppinterpreter' function with or without giving the .txt file name.
;; Example function calls:
;; (gppinterpreter)
;; (gppinterpreter "test_input.txt")

(defvar LEXEMES '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "def" "for" "if" "exit"
                  "load" "display" "true" "false"
                  "+" "-" "/" "*" "(" ")" "\,"))
(defvar TOKENS '("KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEF" "KW_FOR" "KW_IF" "KW_EXIT"
                 "KW_LOAD" "KW_DISPLAY" "KW_TRUE" "KW_FALSE"
                 "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_OP" "OP_CP" "OP_COMMA"))


;; Try to find a match with one of the lexemes.
;; Return token and lexeme length.
;; If there are more than one matching lexemes:
;; i) Pick the longest one if their lengths are not the same.
;; ii) Pick the prior one in the LEXEMES list if their lengths are the same. 
(defun matchLexeme(line)
	(let ((token nil)
         (lexemeLen -1))
		
		(loop :for i :from 0 :below (length LEXEMES)
			  :if (and (and (>= (length line) (length (nth i LEXEMES)))
							(string= (nth i LEXEMES) (subseq line 0 (length (nth i LEXEMES)))))
					   (> (length (nth i LEXEMES)) lexemeLen))
			  :do 
				(progn
					(setf token (nth i TOKENS)) ; Update token if a match is found
					(setf lexemeLen (length (nth i LEXEMES)))))
					
		(values token lexemeLen)))
	
;; Try to find a match with an IDENTIFIER.
;; Return token and lexeme length.	
(defun matchIdentifier(line)
	(let ((ch0 (char line 0))
		  (token nil)
		  (lexemeLen -1))
		  
		;; Check if the first character is [a-zA-Z].  
		(if (alpha-char-p ch0)
		(progn
			(let ((endIdx 0)
				  (isDone nil))
				(loop :while (not isDone)
					  :while (< endIdx (length line)) do
					;; If the current character is not alphabetic or digit, then the matching is done.
					(if (not (or (alpha-char-p (char line endIdx))
								 (digit-char-p (char line endIdx))))
					(progn	
						(setf isDone t))
					(progn
						(setf endIdx (+ 1 endIdx))))
					
					:finally
					(progn
						;; Set token and lexeme length here.
						(setf token "IDENTIFIER")
						(setf lexemeLen endIdx))))))
			
		(values token lexemeLen)))
		
;; Try to find a match with a VALUEF.
;; Return token and lexeme length.		
(defun matchValueF(line)
	(let ((token nil)
		  (lexemeLen -1)
	      (bIdx -1)
		  (endIdx 0))
		
		;; Check if the first char is digit.
		(if (digit-char-p (char line 0))
		(progn	
			(loop :with i := 0
				  :with isDone := nil
				  :while (and (not isDone) (< i (length line))) do
				;; Check if the current char is 'b' character. If it's then the first part is done.
				(if (char= #\b (char line i))
				(progn
					(setf isDone t)
					(setf bIdx i))
				(progn
					;; Check if the current char is digit. If it's not, then there is no matching.
					(if (not (digit-char-p (char line i)))
					(progn
						(setf isDone t))
					(progn 
						(setf i (+ i 1)))))))
				
			;; Check if 'b' is found. Otherwise, do not continue.
			(if (/= bIdx -1)
			(progn
				(loop :with i := (+ bIdx 1)
					  :with isDone := nil
					  :while (and (not isDone) (< i (length line))) do
					(if (not (digit-char-p (char line i)))
					(progn
						(setf isDone t))
					(progn
						(setf i (+ i 1))))
				
					:finally (setf endIdx i))
				
				;; Check if there is at least 1 digit after 'b' character.
				(if (/= 1 (- endIdx bIdx))
				(progn
					(setf token "VALUEF")
					(setf lexemeLen endIdx)))))))
		
		(values token lexemeLen)))
	
;; Try to find a match with COMMENT.
;; Return token and lexeme length.	
(defun matchComment(line)
	(let ((token nil)
		  (lexemeLen -1))
		;; Firstly, check if the first two characters are ';;'.
		(if (and (>= (length line) 2) (string= ";;" (subseq line 0 2)))
			(loop :with endIdx := 0
				  :with isDone := nil
				  :while (and (< endIdx (length line)) (not isDone)) do
				;; Take the remaining part as comment until the end of the line.
				(if (member (char line endIdx) '(#\Newline))
				(progn
					(setf isDone t))
				(progn
					(setf endIdx (+ endIdx 1))))
					
				:finally 
				(progn
					(setf token "COMMENT")
					(setf lexemeLen endIdx))))
					
		(values token lexemeLen)))

(defparameter tokenValueList '())

;; Parse the given line, and print the tokens.					
(defun tokenizeLine(line)
  (setf tokenValueList '())	
  (let ((lastInvalidTokenIdx -1)
		(isTokenized 1))
  (loop :with i := 0
        :with isDone := nil
		:while (and (< i (length line)) (not isDone)) do ;; If the line is done, then exit the loop.
		
        ;; Check if it's whitespace and return carriage character.
        (if (member (char line i) '(#\Space #\Tab #\Newline #\Return))
		(progn
			;; Check if there is a previous unmatching input.
			;; If there is, then print the invalid input from the start index of the last invalid token to the start index of the valid token after that
			;; and set isDone to exit the loop.
			(if (/= lastInvalidTokenIdx -1)
			(progn
				(format t "SYNTAX_ERROR ~a cannot be tokenized ~%" (subseq line lastInvalidTokenIdx i))
				(setf lastInvalidTokenIdx -1)
				(setf isTokenized -1)
				(setf isDone t)))
			(setf i (+ i 1)))
		(progn
			;; Here, call the match functions to get the matching tokens and their lengths.
			;; Then find the longest token among the matching ones.
			;; If the lengths are equal, then select according to their priorities.
			;; Since the 'push' function pushes elements at the beginning of the list, match functions below have higher priority.
			(let ((tokenList (list))
				  (lexemeLenList (list)))
				
				(multiple-value-bind (token lexemeLen) (matchIdentifier (subseq line i (length line)))
					(if (> lexemeLen -1)
						(progn
							(push token tokenList)
							(push lexemeLen lexemeLenList)
							)))
							
				(multiple-value-bind (token lexemeLen) (matchValueF (subseq line i (length line)))
					(if (> lexemeLen -1)
						(progn
							(push token tokenList)
							(push lexemeLen lexemeLenList)
							)))
							
				(multiple-value-bind (token lexemeLen) (matchLexeme (subseq line i (length line)))
					(if (> lexemeLen -1)
						(progn
							(push token tokenList)
							(push lexemeLen lexemeLenList)
							)))			
							
				(multiple-value-bind (token lexemeLen) (matchComment (subseq line i (length line)))
					(if (> lexemeLen -1)
						(progn
							(push token tokenList)
							(push lexemeLen lexemeLenList)
							)))
				
				;; Check if there is at least one match.
				(if (> (length tokenList) 0)
				(progn
					;; Find the first longest element.
					(let ((maxLen -1)
						 (maxIdx 0))

						(loop :with j := 0 
							  :while (< j (length tokenlist)) do
							(if (> (nth j lexemeLenList) maxLen)
							(progn
								(setf maxLen (nth j lexemeLenList))
								(setf maxIdx j)))
							(setf j (+ j 1)))							
						
						(if (/= lastInvalidTokenIdx -1)
						(progn
							(format t "SYNTAX_ERROR ~a cannot be tokenized ~%" (subseq line lastInvalidTokenIdx i))
							(setf lastInvalidTokenIdx -1)
							(setf isTokenized -1)
							(setf isDone t))
						(progn													
							;; (format t "~a ~%" (nth maxIdx tokenList))		
							;; (format t "~a ~%" (subseq line i (+ i maxLen)));
							(setf tokenValueList (append tokenValueList (list (list (nth maxIdx tokenList) (subseq line i (+ i maxLen))))))
							(setf i (+ i (nth maxIdx lexemeLenList)))
							))))
				
				;; Else, there are no match found. Set the lastInvalidTokenIdx if it has not been set already.
				(progn
					(if (= lastInvalidTokenIdx -1)
					(progn
						(setf lastInvalidTokenIdx i)))
					(setf i (+ i 1)))))))
		:finally 
		(progn 
			(if (/= lastInvalidTokenIdx -1)
			(progn
				(format t "SYNTAX_ERROR ~a cannot be tokenized ~%" (subseq line lastInvalidTokenIdx i))
				(setf lastInvalidTokenIdx -1)
				(setf isTokenized -1)))))
		
		(values isTokenized)))

;; Expression node to create abstract syntax tree.
(defstruct expressionnode
	(type #\d)
	(valuef "-1b1")
	(id "defaultId")
	(op #\+)
	(numActualParams -1)
	first
	second
	third
)

;; Function node to store the functions.
(defstruct functionnode
	(id "defaultId")
	(parameters '())
	(numParameters 0)
	(body)
)

;; Variable node to store the variables.
(defstruct variablenode
	(id "defaultId")
	(valuef "-1b1")
)
	
(defparameter tokenIdx 0)
(defparameter functionStack '())
(defparameter variableStack '())
(defparameter errorMsg "Syntax error!")
	
(defun addFunctionToStack(functionnode)
	(setf functionStack (append functionStack (list functionnode))))

(defun addVariableToStack(variablenode)
	(setf variableStack (append variableStack (list variablenode))))	


;; RECURSIVE PARSER: It tries to match the tokens with the CFG rules one by one. When a match is found, it stops matching process.

;; KW_EXIT: Checks if current token is KW_EXIT.
(defun KW_EXIT()
	(let ((numTokensParsed 0) (val nil))
		(if (and (< tokenIdx (length tokenValueList))
				 (string= (nth 0 (nth tokenIdx tokenValueList)) "KW_EXIT"))
		(progn	
			(setf numTokensParsed 1)
			(setf val "EXIT")
			(setf tokenIdx (+ tokenIdx 1))))
		
		(values numTokensParsed val)))
		
;; KW_DEF: Checks if current token is KW_DEF.
(defun KW_DEF()
	(let ((numTokensParsed 0) (val nil))
		(if (and (< tokenIdx (length tokenValueList))
				 (string= (nth 0 (nth tokenIdx tokenValueList)) "KW_DEF"))
		(progn	
			(setf numTokensParsed 1)
			(setf val (nth 1 (nth tokenIdx tokenValueList)))
			(setf tokenIdx (+ tokenIdx 1))))
		
		(values numTokensParsed val)))

;; IDENTIFIER: Checks if current token is IDENTIFIER.
(defun IDENTIFIER()
	(let ((numTokensParsed 0) (val nil))
		(if (and (< tokenIdx (length tokenValueList))
				 (string= (nth 0 (nth tokenIdx tokenValueList)) "IDENTIFIER"))
		(progn	
			(setf numTokensParsed 1)
			(setf val (nth 1 (nth tokenIdx tokenValueList)))
			(setf tokenIdx (+ tokenIdx 1))))
		
		(values numTokensParsed val)))

;; OP_PLUS: Checks if current token is OP_PLUS.
(defun OP_PLUS()
	(let ((numTokensParsed 0) (val nil))
		(if (and (< tokenIdx (length tokenValueList))
				 (string= (nth 0 (nth tokenIdx tokenValueList)) "OP_PLUS"))
		(progn	
			(setf numTokensParsed 1)
			(setf val (nth 1 (nth tokenIdx tokenValueList)))
			(setf tokenIdx (+ tokenIdx 1))))
		
		(values numTokensParsed val)))
		
;; OP_MINUS: Checks if current token is OP_MINUS.
(defun OP_MINUS()
	(let ((numTokensParsed 0) (val nil))
		(if (and (< tokenIdx (length tokenValueList))
				 (string= (nth 0 (nth tokenIdx tokenValueList)) "OP_MINUS"))
		(progn	
			(setf numTokensParsed 1)
			(setf val (nth 1 (nth tokenIdx tokenValueList)))
			(setf tokenIdx (+ tokenIdx 1))))
		
		(values numTokensParsed val)))
		
;; OP_DIV: Checks if current token is OP_DIV.
(defun OP_DIV()
	(let ((numTokensParsed 0) (val nil))
		(if (and (< tokenIdx (length tokenValueList))
				 (string= (nth 0 (nth tokenIdx tokenValueList)) "OP_DIV"))
		(progn	
			(setf numTokensParsed 1)
			(setf val (nth 1 (nth tokenIdx tokenValueList)))
			(setf tokenIdx (+ tokenIdx 1))))
		
		(values numTokensParsed val)))
		
;; OP_MULT: Checks if current token is OP_MULT.
(defun OP_MULT()
	(let ((numTokensParsed 0) (val nil))
		(if (and (< tokenIdx (length tokenValueList))
				 (string= (nth 0 (nth tokenIdx tokenValueList)) "OP_MULT"))
		(progn	
			(setf numTokensParsed 1)
			(setf val (nth 1 (nth tokenIdx tokenValueList)))
			(setf tokenIdx (+ tokenIdx 1))))
		
		(values numTokensParsed val)))		

;; OP_OP: Checks if current token is OP_OP.
(defun OP_OP()
	(let ((numTokensParsed 0) (val nil))
		(if (and (< tokenIdx (length tokenValueList))
				 (string= (nth 0 (nth tokenIdx tokenValueList)) "OP_OP"))
		(progn	
			(setf numTokensParsed 1)
			(setf val (nth 1 (nth tokenIdx tokenValueList)))
			(setf tokenIdx (+ tokenIdx 1))))
		
		(values numTokensParsed val)))

;; OP_CP: Checks if current token is OP_CP.
(defun OP_CP()
	(let ((numTokensParsed 0) (val nil))
		(if (and (< tokenIdx (length tokenValueList))
				 (string= (nth 0 (nth tokenIdx tokenValueList)) "OP_CP"))
		(progn	
			(setf numTokensParsed 1)
			(setf val (nth 1 (nth tokenIdx tokenValueList)))
			(setf tokenIdx (+ tokenIdx 1))))
		
		(values numTokensParsed val)))

;; VALUEF: Checks if current token is VALUEF.
(defun VALUEF()
	(let ((numTokensParsed 0) (val nil))
		(if (and (< tokenIdx (length tokenValueList))
				 (string= (nth 0 (nth tokenIdx tokenValueList)) "VALUEF"))
		(progn	
			(setf numTokensParsed 1)
			(setf val (nth 1 (nth tokenIdx tokenValueList)))
			(setf tokenIdx (+ tokenIdx 1))))
		
		(values numTokensParsed val)))

;; E1: Checks if tokens match with OP_OP OP_PLUS EXP EXP OP_CP rule.
(declaim (ftype function E))
(defun E1()
	(let ((numTokensParsed 0) (val nil) (resultTokenValues '()) (cannotParse nil))
	
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_OP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_PLUS)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_CP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
					
		(setf val (make-expressionnode))
		(setf (expressionnode-type val) #\o)
		(setf (expressionnode-op val) #\+)
		(setf (expressionnode-first val) (nth 2 resultTokenValues))
		(setf (expressionnode-second val) (nth 3 resultTokenValues))
		(values numTokensParsed val)))

;; E2: Checks if tokens match with OP_OP OP_MINUS EXP EXP OP_CP rule.
(defun E2()
	(let ((numTokensParsed 0) (val nil) (resultTokenValues '()) (cannotParse nil))
	
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_OP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_MINUS)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_CP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
			
		(setf val (make-expressionnode))
		(setf (expressionnode-type val) #\o)
		(setf (expressionnode-op val) #\-)
		(setf (expressionnode-first val) (nth 2 resultTokenValues))
		(setf (expressionnode-second val) (nth 3 resultTokenValues))
		(values numTokensParsed val)))
		
;; E3: Checks if tokens match with OP_OP OP_MULT EXP EXP OP_CP rule.
(defun E3()
	(let ((numTokensParsed 0) (val nil) (resultTokenValues '()) (cannotParse nil))
	
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_OP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_MULT)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_CP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
			
		(setf val (make-expressionnode))
		(setf (expressionnode-type val) #\o)
		(setf (expressionnode-op val) #\*)
		(setf (expressionnode-first val) (nth 2 resultTokenValues))
		(setf (expressionnode-second val) (nth 3 resultTokenValues))
		(values numTokensParsed val)))

;; E4: Checks if tokens match with OP_OP OP_DIV EXP EXP OP_CP rule.
(defun E4()
	(let ((numTokensParsed 0) (val nil) (resultTokenValues '()) (cannotParse nil))
	
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_OP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_DIV)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_CP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
			
		(setf val (make-expressionnode))
		(setf (expressionnode-type val) #\o)
		(setf (expressionnode-op val) #\/)
		(setf (expressionnode-first val) (nth 2 resultTokenValues))
		(setf (expressionnode-second val) (nth 3 resultTokenValues))
		(values numTokensParsed val)))

;; E5: Checks if tokens match with OP_OP IDENTIFIER OP_CP rule.
(defun E5()
	(let ((numTokensParsed 0) (val nil) (resultTokenValues '()) (cannotParse nil))
	
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_OP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (IDENTIFIER)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
					
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_CP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
			
		(setf val (make-expressionnode))
		(setf (expressionnode-type val) #\f)
		(setf (expressionnode-id val) (nth 1 resultTokenValues))
		(setf (expressionnode-numActualParams val) 0)
		(values numTokensParsed val)))
		
;; E6: Checks if tokens match with OP_OP IDENTIFIER EXP OP_CP rule.
(defun E6()
	(let ((numTokensParsed 0) (val nil) (resultTokenValues '()) (cannotParse nil))
	
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_OP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (IDENTIFIER)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
					
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_CP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
			
		(setf val (make-expressionnode))
		(setf (expressionnode-type val) #\f)
		(setf (expressionnode-id val) (nth 1 resultTokenValues))
		(setf (expressionnode-first val) (nth 2 resultTokenValues))
		(setf (expressionnode-numActualParams val) 1)
		(values numTokensParsed val)))
		
;; E7: Checks if tokens match with OP_OP IDENTIFIER EXP EXP OP_CP rule.
(defun E7()
	(let ((numTokensParsed 0) (val nil) (resultTokenValues '()) (cannotParse nil))
	
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_OP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (IDENTIFIER)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
					
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_CP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
			
		(setf val (make-expressionnode))
		(setf (expressionnode-type val) #\f)
		(setf (expressionnode-id val) (nth 1 resultTokenValues))
		(setf (expressionnode-first val) (nth 2 resultTokenValues))
		(setf (expressionnode-second val) (nth 3 resultTokenValues))
		(setf (expressionnode-numActualParams val) 2)
		(values numTokensParsed val)))

;; E8: Checks if tokens match with OP_OP IDENTIFIER EXP EXP EXP OP_CP rule.		
(defun E8()
	(let ((numTokensParsed 0) (val nil) (resultTokenValues '()) (cannotParse nil))
	
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_OP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (IDENTIFIER)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
					
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_CP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
			
		(setf val (make-expressionnode))
		(setf (expressionnode-type val) #\f)
		(setf (expressionnode-id val) (nth 1 resultTokenValues))
		(setf (expressionnode-first val) (nth 2 resultTokenValues))
		(setf (expressionnode-second val) (nth 3 resultTokenValues))
		(setf (expressionnode-third val) (nth 4 resultTokenValues))
		(setf (expressionnode-numActualParams val) 3)
		(values numTokensParsed val)))

(defun convertFractionToInteger (valuef)
  (let ((b-position (position #\b valuef)))
	(values (parse-integer (subseq valuef 0 b-position))
			(parse-integer (subseq valuef (1+ b-position))))))

;; E9: Checks if tokens match with IDENTIFIER rule.
(defun E9()
	(let ((numTokensParsed 0) (val nil) (resultTokenValues '()) (cannotParse nil))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (IDENTIFIER)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (/= numTokensParsed 0)
		(progn
			(setf val (make-expressionnode))
			(setf (expressionnode-type val) #\i)
			(setf (expressionnode-id val) (nth 0 resultTokenValues))))
		
		(values numTokensParsed val)))

;; E10: Checks if tokens match with VALUEF rule.
(defun E10()
	(let ((numTokensParsed 0) (val nil) (resultTokenValues '()) (cannotParse nil))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (VALUEF)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (/= numTokensParsed 0)
		(progn
			(let ((denom (nth-value 1 (convertFractionToInteger (nth 0 resultTokenValues)))))
				(if (= denom 0)
				(progn
					(setf errorMsg "Syntax error! Denom cannot be 0."))
				(progn
					(setf val (make-expressionnode))
					(setf (expressionnode-type val) #\v)
					(setf (expressionnode-valuef val) (nth 0 resultTokenValues))
					))
			)))
		
		(values numTokensParsed val)))

;; E: Checks if any of rules are matched with tokens at hand by trying E1, E2 ... E10.
(defun E()
	(let ((numTokensParsed 0) (val nil) (parsedAlready nil))

		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E1)
			(if (/= resultNumTokens 0)
			(progn
				(setf val resultVal)
				(setf numTokensParsed resultNumTokens)
				(setf parsedAlready t))))))
				
		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E2)
			(if (/= resultNumTokens 0)
			(progn
				(setf val resultVal)
				(setf numTokensParsed resultNumTokens)
				(setf parsedAlready t))))))
				
		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E3)
			(if (/= resultNumTokens 0)
			(progn
				(setf val resultVal)
				(setf numTokensParsed resultNumTokens)
				(setf parsedAlready t))))))
				
		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E4)
			(if (/= resultNumTokens 0)
			(progn
				(setf val resultVal)
				(setf numTokensParsed resultNumTokens)
				(setf parsedAlready t))))))
				
		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E5)
			(if (/= resultNumTokens 0)
			(progn
				(setf val resultVal)
				(setf numTokensParsed resultNumTokens)
				(setf parsedAlready t))))))
				
		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E6)
			(if (/= resultNumTokens 0)
			(progn
				(setf val resultVal)
				(setf numTokensParsed resultNumTokens)
				(setf parsedAlready t))))))
			
		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E7)
			(if (/= resultNumTokens 0)
			(progn
				(setf val resultVal)
				(setf numTokensParsed resultNumTokens)
				(setf parsedAlready t))))))
				
		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E8)
			(if (/= resultNumTokens 0)
			(progn
				(setf val resultVal)
				(setf numTokensParsed resultNumTokens)
				(setf parsedAlready t))))))
		
		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E9)
			(if (/= resultNumTokens 0)
			(progn
				(setf val resultVal) 
				(setf numTokensParsed resultNumTokens)
				(setf parsedAlready t))))))
				
		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E10)
			(if (/= resultNumTokens 0)
			(progn
				(setf val resultVal) 
				(setf numTokensParsed resultNumTokens)
				(setf parsedAlready t))))))
		
		(values numTokensParsed val)))					

;; F1: Checks if tokens match with OP_OP KW_DEF IDENTIFIER EXP OP_CP rule.
(defun F1()
	(let ((numTokensParsed 0) (val nil) (resultTokenValues '()) (cannotParse nil))
	
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_OP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (KW_DEF)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (IDENTIFIER)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_CP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
					
		(setf val (make-functionnode))
		(setf (functionnode-id val) (nth 2 resultTokenValues))
		(setf (functionnode-numParameters val) 0)
		(setf (functionnode-body val) (nth 3 resultTokenValues))
		(addFunctionToStack val)
		(values numTokensParsed "#FUNCTION")))
		
;; F2: Checks if tokens match with OP_OP KW_DEF IDENTIFIER IDENTIFIER EXP OP_CP rule.
(defun F2()
	(let ((numTokensParsed 0) (val nil) (resultTokenValues '()) (cannotParse nil))
	
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_OP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (KW_DEF)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (IDENTIFIER)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (IDENTIFIER)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_CP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
					
		(setf val (make-functionnode))
		(setf (functionnode-id val) (nth 2 resultTokenValues))
		(setf (functionnode-numParameters val) 1)
		(setf (functionnode-parameters val) (append (functionnode-parameters val) (list (nth 3 resultTokenValues))))
		(setf (functionnode-body val) (nth 4 resultTokenValues))
		(addFunctionToStack val)
		(values numTokensParsed "#FUNCTION")))
		
;; F3: Checks if tokens match with OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP rule.
(defun F3()
	(let ((numTokensParsed 0) (val nil) (resultTokenValues '()) (cannotParse nil))
	
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_OP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (KW_DEF)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (IDENTIFIER)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (IDENTIFIER)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (IDENTIFIER)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_CP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
					
		(setf val (make-functionnode))
		(setf (functionnode-id val) (nth 2 resultTokenValues))
		(setf (functionnode-numParameters val) 2)
		(setf (functionnode-parameters val) (append (functionnode-parameters val) (list (nth 3 resultTokenValues))))
		(setf (functionnode-parameters val) (append (functionnode-parameters val) (list (nth 4 resultTokenValues))))
		(setf (functionnode-body val) (nth 5 resultTokenValues))
		(addFunctionToStack val)
		(values numTokensParsed "#FUNCTION")))
		
;; F4: Checks if tokens match with OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP rule.
(defun F4()
	(let ((numTokensParsed 0) (val nil) (resultTokenValues '()) (cannotParse nil))
	
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_OP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (KW_DEF)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (IDENTIFIER)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (IDENTIFIER)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (IDENTIFIER)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (IDENTIFIER)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
				
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_CP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
					
		(setf val (make-functionnode))
		(setf (functionnode-id val) (nth 2 resultTokenValues))
		(setf (functionnode-numParameters val) 3)
		(setf (functionnode-parameters val) (append (functionnode-parameters val) (list (nth 3 resultTokenValues))))
		(setf (functionnode-parameters val) (append (functionnode-parameters val) (list (nth 4 resultTokenValues))))
		(setf (functionnode-parameters val) (append (functionnode-parameters val) (list (nth 5 resultTokenValues))))
		(setf (functionnode-body val) (nth 6 resultTokenValues))
		(addFunctionToStack val)
		(values numTokensParsed "#FUNCTION")))

;; F: Checks if any of rules are matched with tokens at hand by trying F1, F2, F3, F4.
(defun F()
	(let ((numTokensParsed 0) (val nil) (parsedAlready nil))

		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (F1)
			(if (/= resultNumTokens 0)
			(progn
				(setf val resultVal)
				(setf numTokensParsed resultNumTokens)
				(setf parsedAlready t))))))
				
		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (F2)
			(if (/= resultNumTokens 0)
			(progn
				(setf val resultVal) 
				(setf numTokensParsed resultNumTokens)
				(setf parsedAlready t))))))
	
		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (F3)
			(if (/= resultNumTokens 0)
			(progn
				(setf val resultVal) 
				(setf numTokensParsed resultNumTokens)
				(setf parsedAlready t))))))
				
		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (F4)
			(if (/= resultNumTokens 0)
			(progn
				(setf val resultVal) 
				(setf numTokensParsed resultNumTokens)
				(setf parsedAlready t))))))
	
		(values numTokensParsed val)))

(defun searchFunction (id)
	(let ((functionRes nil))
		(loop :with i := (- (length functionStack) 1)
			  :with isDone := nil
			  :while (and (not isDone) (>= i 0)) :do
			  
			(if (string= id (functionnode-id (nth i functionStack)))
			(progn
				(setf functionRes (nth i functionStack))
				(setf isDone t))
			(progn
				(setf i (- i 1)))))
				
		functionRes))
		
(defun searchVariable (id)
	(let ((variableRes nil))
		(loop :with i := (- (length variableStack) 1)
			  :with isDone := nil
			  :while (and (not isDone) (>= i 0)) :do
			(if (string= id (variablenode-id (nth i variableStack)))
			(progn
				(setf variableRes (nth i variableStack))
				(setf isDone t))
			(progn
				(setf i (- i 1)))))
				
		variableRes))

;; evalExpression: Evaluates the given expression according to the type field of the expression.
(declaim (ftype function evalFunction))
(defun evalExpression (expNode)
  (let ((result t)
		(resultVal nil)
		(funcNode nil)
		(varNode nil)
        (evalLeft nil)
        (evalRight nil)
		(evalFirst nil)
		(evalSecond nil)
		(evalThird nil)
        (numLeft -1)
        (numRight -1)
        (denomLeft -1)
        (denomRight -1)
        (resultNum -1)
        (resultDenom -1))
    
	(if expNode
	(case (expressionnode-type expNode)
      (#\v
		(setf resultVal (expressionnode-valuef expNode)))
	  
	  (#\o
		(multiple-value-bind (resultExp resultValExp) (evalExpression (expressionnode-first expNode))
			(setf result resultExp)
			(setf evalLeft resultValExp))
			
		(if result
		(progn
			(multiple-value-bind (resultExp resultValExp) (evalExpression (expressionnode-second expNode))
				(setf result resultExp)
				(setf evalRight resultValExp))
		))
		
		(if result
		(progn	
			(multiple-value-bind (numLeftRes denomLeftRes) (convertFractionToInteger evalLeft)
				(setf numLeft numLeftRes)
				(setf denomLeft denomLeftRes))
			
			(multiple-value-bind (numLeftRes denomLeftRes) (convertFractionToInteger evalRight)
				(setf numRight numLeftRes)
				(setf denomRight denomLeftRes))

			(cond
				((char= (expressionnode-op expNode) #\+)
				(setf resultNum (+ (* numLeft denomRight) (* numRight denomLeft)))
				(setf resultDenom (* denomLeft denomRight)))

				((char= (expressionnode-op expNode) #\-)
				(setf resultNum (- (* numLeft denomRight) (* numRight denomLeft)))
				(setf resultDenom (* denomLeft denomRight)))

				((char= (expressionnode-op expNode) #\*)
				(setf resultNum (* numLeft numRight))
				(setf resultDenom (* denomLeft denomRight)))

				((char= (expressionnode-op expNode) #\/)
				(if (or (= numRight 0) (= denomLeft 0))
				(progn
					(setf errorMsg "Syntax error! Division by 0.")
					(setf result nil))
				(progn
					(setf resultNum (* numLeft denomRight))
					(setf resultDenom (* denomLeft numRight))))))
						
			(setf resultVal (format nil "~ab~a" resultNum resultDenom)))))
			
		
		(#\f
			(setf funcNode (searchFunction (expressionnode-id expNode)))
			(if (not funcNode)
			(progn
				(setf errorMsg "Syntax error! Function is not defined.")
				(setf result nil)))
				
			(if (and result (not (= (functionnode-numParameters funcNode) (expressionnode-numActualParams expNode))))
			(progn
				(setf errorMsg "Syntax error! Actual parameters are not matching with function parameters.")
				(setf result nil)))
			
			(if result
			(progn
				(multiple-value-bind (resultExp resultValExp) (evalExpression (expressionnode-first expNode))
					(setf result resultExp)
					(setf evalFirst resultValExp))))
			
			(if result
			(progn
				(multiple-value-bind (resultExp resultValExp) (evalExpression (expressionnode-second expNode))
					(setf result resultExp)
					(setf evalSecond resultValExp))))
					
			(if result
			(progn
				(multiple-value-bind (resultExp resultValExp) (evalExpression (expressionnode-third expNode))
					(setf result resultExp)
					(setf evalThird resultValExp))))
					
			(if result
			(progn
				(multiple-value-bind (resultFunc resultValFunc) (evalFunction funcNode evalFirst evalSecond evalThird)
					(setf result resultFunc)
					(setf resultVal resultValFunc))))
		)
			
		(#\i
			(setf varNode (searchVariable (expressionnode-id expNode)))
			(if (not varNode)
			(progn
				(setf errorMsg "Syntax error! Variable is not defined.")
				(setf result nil))
			(progn
				(setf resultVal (variablenode-valuef varNode))))
		)
		
	))
	
    (values result resultVal)))
	
;; evalFunction: Evaluates the given function. 
;; It initializes its variables in the variable stack, then it removes these variables afterwards.
(defun evalFunction (funcNode actualParam1 actualParam2 actualParam3)
  (let ((result t)
		(resultVal nil)
		(variable1 nil)
		(variable2 nil)
		(variable3 nil))
	
	(if (>= (functionnode-numParameters funcNode) 1)
	(progn
		(setf variable1 (make-variablenode))
		(setf (variablenode-id variable1) (nth 0 (functionnode-parameters funcNode)))
		(setf (variablenode-valuef variable1) actualParam1)
		(addVariableToStack variable1)))
		
	
	(if (>= (functionnode-numParameters funcNode) 2)
	(progn
		(setf variable2 (make-variablenode))
		(setf (variablenode-id variable2) (nth 1 (functionnode-parameters funcNode)))
		(setf (variablenode-valuef variable2) actualParam2)
		(addVariableToStack variable2)))
		
	(if (>= (functionnode-numParameters funcNode) 3)
	(progn
		(setf variable3 (make-variablenode))
		(setf (variablenode-id variable3) (nth 2 (functionnode-parameters funcNode)))
		(setf (variablenode-valuef variable3) actualParam3)
		(addVariableToStack variable3)))
		
	(multiple-value-bind (resultExp resultValExp) (evalExpression (functionnode-body funcNode))
		(setf result resultExp)
		(setf resultVal resultValExp))
		
	(dotimes (i (functionnode-numParameters funcNode))
		(setq variableStack (butlast variableStack)))
 
    (values result resultVal)))
	
;; EXIT1: Checks if tokens are matched with EXIT rule.
(defun EXIT1()
	(let ((numTokensParsed 0) (resultTokenValues '()) (cannotParse nil))
	
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_OP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (KW_EXIT)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
		
		(if (not cannotParse)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (OP_CP)
			(if (= resultNumTokens 0)
			(progn
				(setf tokenIdx (- tokenIdx numTokensParsed))
				(setf numTokensParsed 0)
				(setf cannotParse t))
			(progn
				(setf numTokensParsed (+ numTokensParsed resultNumTokens))
				(setf resultTokenValues (append resultTokenValues (list resultVal))))))))
					
		(values numTokensParsed "EXIT")))

;; S: Start symbol in CFG. Checks if tokens are matched with any of E, F, KW_EXIT rules.	
(defun S()
	(let ((sRes -1) (val nil) (parsedAlready nil))
	
		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (E)
				(if (/= resultNumTokens 0)
				(progn
					(setf parsedAlready t)
					(multiple-value-bind (expResult expValuef) (evalExpression resultVal)
						(if expResult
						(progn
							(setf val expValuef)
							(setf sRes 0)))))))))
							
		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (EXIT1)
				(if (/= resultNumTokens 0)
				(progn
					(setf parsedAlready t)
					(setf sRes 1)
					(setf val resultVal))))))
					
		(if (not parsedAlready)
		(progn
			(multiple-value-bind (resultNumTokens resultVal) (F)
				(if (/= resultNumTokens 0)
				(progn
					(setf parsedAlready t)
					(setf sRes 0)
					(setf val resultVal))))))

		(values sRes val)))

;; It tries to parse all tokens by calling S procedure until all tokens in a line are consumed.
(defun parse()
  ;; Check all tokens are parsed.
	(setf tokenIdx 0)
    (let ((result nil) (sRes 0) (sVal nil) (resultValues '()))
		(loop
		  :with isDone := nil
		  :while (and (< tokenIdx (length tokenValueList)) (not isDone)) :do
			(multiple-value-bind (sResLocal sValLocal) (S)
				(setf sRes sResLocal)
				(setf sVal sValLocal))
			(if (= sRes -1)
			(progn
				(format t "~a ~%" errorMsg)
				(setf result nil)
				(setf isDone t)))
				
			;; KW_EXIT, exit program without error
			(if (= sRes 1)
			(progn
				(setf resultValues (append resultValues (list "EXIT")))
				(setf result t)
				(setf isDone t)))
				
			(if (= sRes 0)
			(progn
				(setf resultValues (append resultValues (list sVal)))
				(setf result t))))
				
			;; If the input is successfully parsed, then print the eval results.
			(if result 
			(progn
				(loop :with i := 0
					  :with isDone := nil
					  :while (and (not (= i (length resultValues))) (not isDone)) :do
					(if (string= "EXIT" (nth i resultValues))
					(progn
						(setf result nil)
						(setf isDone t))
					(progn
						(format t "~a ~%" (nth i resultValues))))
					
					(setf i (+ i 1))
				)
			))
			(values result)))

;; Takes fileName as optional parameter.
;; If the fileName is given, then it reads the file line-by-line and tokenizes it.
;; Otherwise, it reads input from user and tokenizes it line-by-line.
(defun gppinterpreter(&optional fileName)

	(if (not fileName)
	(progn
		(loop :with userInput := nil
			  :with isDone := nil
			  :while (not isDone) do
			   
			; Using the FORMAT function to print each element on a new line
			(format t ">")
			(force-output)
			
			(setf userInput (read-line)) 		
			(if (= (tokenizeLine userInput) -1)
			(progn
				(format t "gppinterpreter CLOSED. ~%")
				(setf isDone t))
			(progn
				(if (not (parse))
				(progn					
					(format t "gppinterpreter CLOSED. ~%")
					(setf isDone t)))))))
	(progn
		(handler-case
		  (with-open-file (stream fileName :direction :input)
			(loop :with isDone := nil
				  :while (not isDone) do		
				(let ((line (read-line stream nil)))
					(if (not line)
					(progn
						(setf isDone t))
					(progn
						(if (= (tokenizeLine line) -1)
						(progn
							(format t "gppinterpreter CLOSED. ~%")
							(setf isDone t))
						(progn
							(if (not (parse))
							(progn					
								(format t "gppinterpreter CLOSED. ~%")
								(setf isDone t)))))
						)))))
			
		(file-error (e)
		  (format t "File not found or cannot be opened. ~%")
		  (format t "Error: ~a~%" e))))))