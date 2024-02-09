;; Author: Emre Oytun
;; HOW TO RUN THE PROGRAM :
;; 1) Load it to the lisp interpreter by using command 'sbcl --load gpp_lexer.lisp'
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

;; Parse the given line, and print the tokens.					
(defun parseLine(line)
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
							(format t "~a ~%" (nth maxIdx tokenList))
							(setf i (+ i (nth maxIdx lexemeLenList)))))))
				
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
						
						
;; Takes fileName as optional parameter.
;; If the fileName is given, then it reads the file line-by-line and tokenizes it.
;; Otherwise, it reads input from user and tokenizes it line-by-line.
(defun gppinterpreter(&optional fileName)

	(if (not fileName)
	(progn
		(format t "Enter ':q' to quit ~%")

		(loop :with userInput := nil
			  :with isDone := nil
			  :while (not isDone) do
			  
			(format t ">")
			(force-output)
			
			(setf userInput (read-line)) 
			(if (string= userInput ":q")
			(progn
				(format t "gppinterpreter CLOSED. ~%")
				(setf isDone t))
			(progn
				(if (= (parseLine userInput) -1)
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
						(let ((parseResult (parseLine line)))
						(if (not parseResult)
						(progn
							(setf isDone t)))))))))
			
		(file-error (e)
		  (format t "File not found or cannot be opened. ~%")
		  (format t "Error: ~a~%" e))))))