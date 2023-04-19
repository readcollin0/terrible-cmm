(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)
(ql:quickload :uiop)
(ql:quickload :alexandria)

(defun reload ()
	(load "parser.lsp"))

(defun debug-print (str val)
	(format t (concatenate 'string str "~S~%") val)
	val)

(defmacro debug-print-var (var)
	`(progn (format t "~A: ~A~%" ,(string var) ,var)
			,var))
		
(defmacro debug-print-expansion (form &key run)
	`(progn
		(format t "Expansion: ~S~%" (macroexpand-1 (quote ,form)))
		,(when run form)))

(defmacro shadow-globals (globals &rest body)
	`(let (,@(loop for global in globals collect `(,global ,global)))
		,@body))

(define-symbol-macro [...]
	(error "Unimplemented!"))

(defparameter *parser-helpers* nil)
(defparameter *parser-text-symbol* (gensym))
(defparameter *parser-position-symbol* (gensym))
(defparameter *parser-nil-symbol* 'NONE)
(defun show-parser-globals ()
	(debug-print-var *parser-helpers*)
	(debug-print-var *parser-text-symbol*)
	(debug-print-var *parser-position-symbol*)
	(debug-print-var *parser-nil-symbol*))

(defparameter *parser-option-trim-whitespace* t)
(defparameter *parser-option-no-space-single* nil)
(defparameter *parser-option-comment-regex* nil)



(defun get-whitespace-regex (&key enforce-start enforce-end)
	(concatenate 'string 
		(when enforce-start "^")
		"(\\s|(" *parser-option-comment-regex* "))*"
		(when enforce-end "$")))

(defun string-contains-at (text pos substr)
	(let ((end (+ pos (length substr))))
		(when (<= end (length text))
			(when (string= text substr :start1 pos :end1 end)
				end))))

(defun remaining-whitespace-p (text after-pos)
	(cl-ppcre:scan (get-whitespace-regex :enforce-start t :enforce-end t)
				   text :start after-pos))

(defun string-contains-at-regex (text pos regex)
	(let ((regex (concatenate 'string "^(" regex ")")))
		(multiple-value-bind (start end)
				(cl-ppcre:scan regex text :start pos)
			(when start ;When did it actually parse. Plus, gets rid of warning.
				end))))

(defun digest-string (text position string &key (use-regex nil) (trim-whitespace t))
	(when trim-whitespace
		(let ((whitespace-len (string-contains-at-regex text position (get-whitespace-regex :enforce-start t))))
			(when whitespace-len (setf position whitespace-len))))
	(let ((end (if use-regex
				   (string-contains-at-regex text position string)
				   (string-contains-at       text position string))))
		(when end ; substring found
			(list (subseq text position end)
				  end))))

;(eval-when (:compile-toplevel)

(defun add-prefix-to-symbol (sym prefix)
	(intern (concatenate 'string prefix "-" (string sym))))

;)

(defmacro with-gensyms (syms &body body)
	`(let (,@(loop for sym in syms collect `(,sym (gensym))))
		,@body))
				
				
				
(defmacro try-parse (&body body)
	(with-gensyms (return-block alt-pos-symbol result)
		`(block ,return-block
			(macrolet ((fail-parse () '(return-from ,return-block nil)))
				(let* ((,alt-pos-symbol ,*parser-position-symbol*)
					   (,result 
							(symbol-macrolet ((,*parser-position-symbol* ,alt-pos-symbol))
								,@body)))
					(setf ,*parser-position-symbol* ,alt-pos-symbol)
					,result)))))

(defmacro test-parser-env (text position &body body)
	(with-gensyms (return-value)
		`(let ((,*parser-text-symbol* ,text)
			   (,*parser-position-symbol* ,position))
			(try-parse
				(let ((,return-value (progn ,@body)))
					(format t "Finished test at position: ~A~%" ,*parser-position-symbol*)
					(format t "Test returned: ~S~%" ,return-value))))))

(defmacro parser-digest (string &key (use-regex nil) (auto-fail nil))
	(with-gensyms (parse-result)
		`(let ((,parse-result (digest-string ,*parser-text-symbol*
											 ,*parser-position-symbol*
											 ,string
											 :trim-whitespace (and *parser-option-trim-whitespace* 
																   (not *parser-option-no-space-single*))
											 :use-regex ,use-regex)))
			(when *parser-option-no-space-single*
			      (setf *parser-option-no-space-single* nil))
			(if ,parse-result
				(progn (setf ,*parser-position-symbol* (second ,parse-result))
					   (first ,parse-result))
				,(when auto-fail `(fail-parse))))))

(defmacro defparser-helper (name args &rest body)
	(let ((new-name (add-prefix-to-symbol name "PARSER-HELPER")))
		(setf *parser-helpers* (acons name new-name *parser-helpers*))
		`(defmacro ,new-name (,@args) 
			,@body)))




(defparser-helper log (string to-log)
	`(debug-print ,string (parse-base ,to-log)))

(defparser-helper regex (string)
	`(parser-digest ,string :use-regex t))

(defparser-helper keyword (string &key (chars "\\w"))
	`(parser-digest (concatenate 'string "(?<![" ,chars "])" ,string "(?![" ,chars "])" :use-regex t)))

(defparser-helper no-space (without-space)
	`(let ((*parser-option-no-space-single* t))
		(parse-base ,without-space)))

(defparser-helper no-space-all (without-space)
	`(let ((*parser-option-trim-whitespace* nil))
		(parse-base ,without-space)))

(defparser-helper with-space-all (with-space)
	`(let ((*parser-option-trim-whitespace* t))
		(parse-base ,with-space)))

(defparser-helper list (&rest items)
	`(try-parse (list ,@(loop for item in items collect `(parse-base ,item :auto-fail t)))))

(defparser-helper literal (value)
	value)

(defparser-helper ? (optional)
	`(or (parse-base ,optional)
	     ',*parser-nil-symbol*))

(defparser-helper + (repeated)
	(with-gensyms (result)
		`(loop with ,result = nil
			do (setf ,result (parse-base ,repeated))
			while ,result
			collect ,result)))

(defparser-helper * (repeated)
	(with-gensyms (result)
		`(or (loop with ,result = nil
				do (setf ,result (parse-base ,repeated))
				while ,result
				collect ,result)
			 ',*parser-nil-symbol*)))

(defparser-helper or (&rest options)
	`(or ,@(loop for option in options
				 collect `(parse-base ,option))))

(defparser-helper or-type (&rest options)
	(with-gensyms (result)
		`(or ,@(loop for option in options
					 collect `(let ((,result (parse-base ,option)))
								   (when ,result (list ',option ,result)))))))

(defparser-helper delimited (delimiter content &key at-least-one)
	(let ((default (if at-least-one nil *parser-nil-symbol*)))
		(with-gensyms (result next)
			`(let ((,result nil))
				(if (push (parse-base ,content) ,result)
					(loop with ,next = nil
						  do (setf ,next 
						 		   (try-parse (cons (parse-base ,delimiter :auto-fail t)
						 							(parse-base ,content :auto-fail t))))
						  if ,next
						  do (progn (push (car ,next) ,result)
						 		    (push (cdr ,next) ,result))
						  else
						  return (nreverse ,result))
					',default)))))

(defparser-helper disallow (to-parse cannot-be)
	(with-gensyms (result pos-backup)
		`(try-parse
			(let ((,pos-backup ,*parser-position-symbol*)
				  (,result (parse-base ,to-parse)))
				(let ((,*parser-position-symbol* ,pos-backup))
					(when (parse-base ,cannot-be)
						(fail-parse))
					,result)))))
				
(defparser-helper parse-int (to-parse)
	(with-gensyms (result)
		`(alexandria:when-let ((,result (parse-base ,to-parse)))
			(cond ((string-contains-at ,result 0 "0x")
						(parse-integer (subseq ,result 2) :radix 16))
				  ((string-contains-at ,result 0 "0b")
						(parse-integer (subseq ,result 2) :radix 2))
				  (t (parse-integer ,result))))))



(defparameter *parser-debug-depth* 0)
(defparameter *parser-debug-depth-str* "  ")
(defparameter *parser-debug-print-result* nil)
(defparameter *parser-debug-print* nil)
(defparameter *parser-last-success* 0)

(defun repeat-string (str times)
	(let ((result ""))
		(dotimes (n times)
			(setf result (concatenate 'string result str)))
		result))

(defun parser-print-debug-depth ()
	(format t "~A~A:" (repeat-string *parser-debug-depth-str* *parser-debug-depth*)
					  *parser-debug-depth*))

(defun parser-debug-enter (name position)
	(when *parser-debug-print*
		(parser-print-debug-depth)
		(format t "~A Entering (~A)~%" name position)
		(incf *parser-debug-depth*)))

(defun parser-debug-leave (name position result)
	(when *parser-debug-print*
		(decf *parser-debug-depth*)
		(parser-print-debug-depth)
		(if *parser-debug-print-result*
			(format t "~A Result (~A): ~S~%" name position result)
			(if result
				(format t "~A Parse succeeded (~A)~%" name position)
				(format t "~A Parse failed (~A)~%" name position)))))

(defun update-last-success (pos)
	(when (> pos *parser-last-success*)
		(setf *parser-last-success* pos)))

(defmacro defparser (parser-name settings &rest rules)
	(destructuring-bind (init-rule &key comment-regex debug-mode)
						settings
		(with-gensyms (result)
			(let* ((rule-names (mapcar #'first rules)))
				`(defun ,parser-name (,*parser-text-symbol* &key use-whole-string)
					(macrolet ((parse-base (thing &key auto-fail)
									`(or
										,(cond ((symbolp thing)
												(if (find thing ',rule-names)
													; Actual code for rule resolution
													`(let ((result (,(add-prefix-to-symbol thing "PARSER-RULE") ,*parser-position-symbol*)))
														  (when result
															    (update-last-success (second result))
																(setf ,*parser-position-symbol* (second result))
																(first result)))
													(error "Parser rule not found: ~A" thing)))
											  ((stringp thing)
											   `(parser-digest ,thing))
											  ((listp thing)
											   (let ((helper-name (cdr (assoc (first thing) *parser-helpers*))))
												   (if helper-name
													   `(,helper-name ,@(rest thing))
													   (error "Parser helper not found: ~A" (first thing)))))
											  (t (throw "Unexpected in parser: ~A" thing)))
										,(when auto-fail `(fail-parse)))))
						(labels (,@(loop for rule in rules
										collect `(,(add-prefix-to-symbol (first rule) "PARSER-RULE") (,*parser-position-symbol*)
													,(when debug-mode `(parser-debug-enter ',(first rule) ,*parser-position-symbol*))
													(let ((result (parse-base ,(second rule))))
														,(when debug-mode `(parser-debug-leave ',(first rule) ,*parser-position-symbol* result))
														(when result 
															(list result ,*parser-position-symbol*))))))
							(let ((*parser-last-success* 0)
								  (*parser-option-comment-regex* ,comment-regex))	
								(let ((,result (,(add-prefix-to-symbol init-rule "PARSER-RULE") 0)))
									(when ,result
										(if (or (not use-whole-string)
												(and (>= (length ,*parser-text-symbol*) 
														 (second ,result))
													 (remaining-whitespace-p ,*parser-text-symbol* (second ,result))))
											(first ,result)
											(progn (format t "Last successful parse: ~A~%" *parser-last-success*)
												nil))))))))))))




	



(defparameter *final-lines* nil)
(defparameter *lines-to-parse* nil)

(defmacro defun-preproc-always (name args &body body)
	(let ((new-name (add-prefix-to-symbol name "PREPROC-COMMAND")))
		`(defun ,new-name ,args ,@body)))

(defmacro defun-preproc (name args &body body)
	`(defun-preproc-always ,name ,args
		(unless *preproc-hold-output*
			,@body)))

(defmacro call-preproc (name &rest args)
	(let ((new-name (add-prefix-to-symbol name "PREPROC-COMMAND")))
		`(,new-name ,@args)))

(defmacro setf-concat (str &rest new-text)
	`(setf ,str (concatenate 'string ,str ,@new-text)))

(defun add-to-final-lines (text)
	(setf-concat *final-lines* (format nil "~A~%" text)))

(defun add-to-to-parse (lines)
	(setf *lines-to-parse* (nconc lines *lines-to-parse*)))



(defparameter *preproc-hold-output* nil)
(defparameter *preproc-vars* nil)

(defun preproc-create-var (var value)
	(push (cons var value) *preproc-vars*))

(defun preproc-get-var-cont (var)
	(assoc var *preproc-vars*))

(defun preproc-get-var (var)
	(cdr (preproc-get-var-cont var)))

(defun preproc-remove-var (var)
	(let ((var-cont (preproc-get-var-cont var)))
		(setf *preproc-vars* (delete var-cont *preproc-vars*))))


(defun-preproc define (name &optional value)
	(preproc-create-var name value))

(defun-preproc undef (name)
	(preproc-remove-var name))


(defparameter *preproc-if-depth* 0)
(defparameter *preproc-active-if-layers* nil)

(defun preproc-get-latest-if-layer ()
	(let ((latest (first *preproc-active-if-layers*)))
		(unless latest (error "Preprocessor command requires an `if`!"))
		latest))

(defun preproc-latest-if-true ()
	(cdr (preproc-get-latest-if-layer)))

(defun preproc-current-if-active ()
	(= *preproc-if-depth* (car (preproc-get-latest-if-layer))))
		
		

(defmacro preproc-if-handler (cond)
	(with-gensyms (cond-result)
		`(progn
			(incf *preproc-if-depth*)
			(unless *preproc-hold-output*
				(let ((,cond-result ,cond))
					(setf *preproc-hold-output* (not ,cond-result))
					(push (cons *preproc-if-depth* ,cond-result) *preproc-active-if-layers*))))))

(defun-preproc-always ifdef (name)
	(preproc-if-handler (preproc-get-var-cont name)))

(defun-preproc-always ifndef (name)
	(preproc-if-handler (not (preproc-get-var-cont name))))

(defun-preproc-always if (val)
	(preproc-if-handler (if (numberp val)
							(not (= val 0))
							(let ((var-val (preproc-get-var val)))
								(if (numberp var-val) ; If variable undefined or nil, treat as false.
									(not (= var-val 0))
									(error "Variable (~A) not a number: ~A" val var-val))))))

(defun-preproc-always toggle-out ()
	(setf *preproc-hold-output* (not *preproc-hold-output*)))

(defun-preproc-always else ()
	(when (preproc-current-if-active)
		  (call-preproc toggle-out)))

(defun-preproc-always endif ()
	(when (preproc-current-if-active)
		  (setf *preproc-hold-output* nil)
		  (pop *preproc-active-if-layers*))
	(decf *preproc-if-depth*))

(defun-preproc include (file-name)
	(add-to-to-parse (uiop:read-file-lines file-name :if-does-not-exist :error)))






(defun preproc-replace (line define value)
	(let* ((def-regex (concatenate 'string "(?<![\\w])" (format nil "~A" define) "(?![\\w])"))
		   (replace-with (format nil "~A" (or value ""))))
		(cl-ppcre:regex-replace-all def-regex line replace-with)))

(defun preproc-replace-with-defines (line)
	(loop for var in *preproc-vars*
			do (setf line (preproc-replace line (car var) (cdr var))))
	line)

(defun preproc-process-normal-line (line)
	(preproc-replace-with-defines line))

(defun extract-preproc (line)
	(let* ((white-end (string-contains-at-regex line 0 "\\s*"))
		   (is-preproc (string-contains-at line white-end "#")))
		(when is-preproc
			(read-from-string (subseq line (1+ white-end))))))

(defun execute-preproc (command)
	(let* ((new-comm (add-prefix-to-symbol (first command) "PREPROC-COMMAND"))
		   (new-func (symbol-function new-comm)))
		(unless new-func (error "Unknown preprocessor command: ~A" command))
		(apply (symbol-function new-comm) (cdr command))))

(defun preprocess (lines)
	(shadow-globals (*final-lines*
					 *preproc-hold-output*
					 *preproc-if-depth*
					 *preproc-active-if-layers*
					 *preproc-vars*)
		(let ((*lines-to-parse* lines)
			  (*final-lines* nil)
			  (*preproc-hold-output* nil))
			(loop while *lines-to-parse*
				  do (let* ((line (pop *lines-to-parse*))
							(command (extract-preproc line)))
						(if command
							(execute-preproc command)
							(unless *preproc-hold-output*
									(add-to-final-lines (preproc-process-normal-line line))))))
			*final-lines*)))
















(defparser parse-program (program
						  :comment-regex "(\\/\\/.*)|(\\/\\*([^\\*]+|\\*[^\\/])\\*\\/)"
						  :debug-mode t)

    (program (* (or-type global func-def)))
	
	(global (list identifier "=" global-val ";"))
	(global-val (or-type string number array))
	(array (list array-type "array" (or-type array-size array-content)))
	(array-type (or "byte" "half" "word" "dword"))
	(array-size (list "[" number "]"))
	(array-content (list "{" (delimited "," number :at-least-one t) "}"))
	(string (regex "\"([^\\\"]|\\[\\\"nt])*\""))
	
	(func-def (list "func" identifier "(" (delimited "," (list (? vartype) identifier)) ")" statement))
	
	
	(statement (or-type block if-else while do-while break label goto
					    assignment assignment-by load store
						function-call compiler-call local-array
					    declaration retire return empty))
	(block (list "{" (* statement) "}"))
	(if-else (list "if" condition statement (? (list "else" statement))))
	(while (list "while" condition statement))
	(do-while (list "do" statement "while" condition ";"))
	(condition (list "(" (or-type bool-expr simple-expr num-value negative) ")"))
	(bool-expr (list num-value compare-op num-value))
	(compare-op (or "==" "!=" "<=" "<" ">=" ">"))
	
	
	(assignment (list identifier "=" num-expression ";"))
	(assignment-by (list identifier bin-assign-op num-value ";"))
	(bin-assign-op (list bin-op (no-space "=")))
	
	(num-expression (or-type simple-expr num-value negative))
	(simple-expr (list identifier bin-op num-value))
	(num-value (or identifier number))
	(negative (list "-" (no-space identifier)))
	(bin-op (or "+" "-" "|" "&" "^" ">>>" ">>" "<<" "<"))
	
	(store (list "store" identifier (? index) "=" identifier ";"))
	(load (list identifier "=" "load" identifier (? index) ";"))
	(index (list "[" num-value "]"))
	
	(function-call (list identifier "(" (delimited "," num-expression) ")" ";"))
	
	(compiler-call (list "@" (no-space identifier) "(" compiler-call-args ")"))
	(compiler-call-args (or number identifier))
				   
	(declaration (list vartype (delimited "," (list identifier (? (list "=" num-expression)))) ";"))
	(local-array (list "array" array-size identifier ";"))
	
	(retire (list "retire" (delimited "," identifier) ";"))
	(return (list "return" (? (list num-expression (? (list "," num-expression)))) ";"))
	
	(label (list identifier (no-space ":")))
	(goto (list "goto" identifier (? condition) ";"))
	
	(break "break" (? condition) ";")
	(empty ";")
	
	(identifier (disallow (regex "[a-zA-Z_][a-zA-Z0-9_]*") 
						  (or "func" "if" "while" "tmpvar" "savvar" "memvar" "do" "else" "return")))
	(vartype (or "memvar" "savvar" "tmpvar"))
	;(register (regex "\\$[a-zA-Z0-9]+"))
	(number (parse-int (regex "-?(0x[0-9a-f]+|0b[01]+|[0-9]+)"))))


(defun replace-nil-symbol (parsed)
	(cond ((listp parsed)
		   (loop for item in parsed collect (replace-nil-symbol item)))
		  ((symbolp parsed)
		   (if (eq parsed *parser-nil-symbol*)
			   nil
			   parsed))
		  (t parsed)))




(defparameter *riscv-saved-regs* '(s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11))
(defparameter *riscv-temp-regs* '(t0 t1 t2 t3 t4 t5 t6))
(defparameter *riscv-arg-regs* '(a0 a1 a2 a3 a4 a5 a6 a7))

(defparameter *riscv-arg-vars* '("arg0" "arg1" "arg2" "arg3" "arg4" "arg5" "arg6" "arg7"))
(defparameter *compiler-disallowed-vars* '("arg0" "arg1" "arg2" "arg3" "arg4" "arg5" "arg6" "arg7" "ret0" "ret1"))

(defparameter *compiler-global-variables* nil)
(defparameter *compiler-data-segment* nil)

(defparameter *compiler-vars* `(("zero" reg perm zero)
								("arg0" reg perm a0)
								("arg1" reg perm a1)
								("arg2" reg perm a2)
								("arg3" reg perm a3)
								("arg4" reg perm a4)
								("arg5" reg perm a5)
								("arg6" reg perm a6)
								("arg7" reg perm a7)
								("ret0" reg perm a0)
								("ret1" reg perm a1)
								("register_tp" reg perm tp)
								("register_gp" reg perm gp)))  ; Base variables, plus shadowed for scopes.
(defparameter *compiler-used-saved-regs* nil)		; To be shadowed for saving
(defparameter *compiler-label-count* 0)
(defparameter *compiler-available-stack* nil)
(defparameter *compiler-stack-var-count* 0)
(defparameter *compiler-return-label* nil)
(defparameter *compiler-break-target* nil)

(defparameter *compiler-labels* nil)

(defparameter *inverse-comp* '(("==" . "!=")
							   ("!=" . "==")
							   ("<"  . ">=")
							   (">=" . "<")
							   (">"  . "<=")
							   ("<=" . ">")))

(defun print-all (&rest items)
	(loop for item in items do (format t "~S~%" item)))


(defun make-label (type &optional name)
	(let ((label `(label ,type ,(incf *compiler-label-count*) ,@(when name `(,name)))))
		(push label *compiler-labels*)
		label))



(defun get-var (name)
	(find-if (lambda (item) (equal (first item) name))
			 *compiler-vars*))

(defun get-global (name)
	(find-if (lambda (item) (equal (first item) name))
			 *compiler-global-variables*))

(defun get-reg-or-num (thing)
	(if (numberp thing)
		thing
		(let ((var (get-var thing)))
			(when (eq 'reg (second var))
				  (fourth var)))))

(defun check-not-nil (x &optional message)
	(unless x (error (or message "Nil check failed!")))
	x)

(defun check-var-type (name var type err-msg)
	(if (eq type (second var))
		var
		(error err-msg name)))


(defun compile-block (block)
	(loop for statement in (second block)
			collect (compile-statement statement)))

(defun is-name-disallowed (name)
	(find name *compiler-disallowed-vars* :test #'equal))

(defun remove-delimiter (items)
	(loop with i = 0
		for item in items
		do (incf i)
		if (oddp i)
		collect item))

(defun compile-declaration (decl)
	(let ((type (first decl)))
		(loop for var in (remove-delimiter (second decl))
			collect (let ((name (first var))
						  (assn (second (second var))))
						 (when (is-name-disallowed name) (error "Variable name not allowed: ~A" name))
						 (when (get-var name) (error "Variable name in use: ~A" name))
						 (let ((var-entry
									(alexandria:eswitch (type :test #'equal)
										("memvar"
											(let ((stack-spot (or (pop *compiler-available-stack*)
																  (incf *compiler-stack-var-count*))))
												`(,name mem stack ,stack-spot)))
										("savvar"
											(let ((reg (or (pop *riscv-saved-regs*)
														   (error "Too many saved regs: ~a" name))))
												(pushnew reg *compiler-used-saved-regs*)
												`(,name reg saved ,reg)))
										("tmpvar"
											(let ((reg (or (pop *riscv-temp-regs*)
														   (error "Too many temp regs: ~a" name))))
												`(,name reg temp ,reg))))))
							(push var-entry *compiler-vars*)
							(when assn
								(%compile-assignment name assn)))))))

(defparameter *type-sizes* '(("byte" . 1) ("half" . 2) ("word" . 4) ("dword" . 8)))

(defun int-ceiling (val amount)
	(* amount (ceiling val amount)))

(defun compile-local-array (decl)
	(let* ((size (second (second decl)))
		   (real-size (int-ceiling size 4))
		   (var-name (third decl))
		   (stack-spots-taken (ash real-size -2))
		   (stack-spot (1+ *compiler-stack-var-count*)))
		(incf *compiler-stack-var-count* stack-spots-taken)
		(push `(,var-name mem array ,stack-spot ,stack-spots-taken) *compiler-vars*)
		nil))

(defun to-bool (form)
	(not (null form)))

(defun test-multi-sym-switch (test pattern)
	(when (eq t test)
		(return-from test-multi-sym-switch t))
	(loop for pat-item in pattern
		  for tst-item in test
		do (unless (if (listp pat-item)
					   (find tst-item pat-item)
					   (eq tst-item pat-item))
				(return-from test-multi-sym-switch nil)))
	t)

(defun stack-space-to-sp-offset (space)
	(* 4 (1- space)))

(defun var-sp-offset (var)
	(stack-space-to-sp-offset (fourth var)))

(defun get-designator (var index)
	(let ((local (get-var var))
		  (global (get-global var)))
		(cond (local
				(alexandria:eswitch ((subseq local 1 3) :test #'test-multi-sym-switch)
					('(reg (arg temp saved))
					  (if index
						  `(reg ,(fourth local) ,(check-not-nil (get-reg-or-num index)))
						  (error "Cannot load a non-memory variable: ~A" var)))
					('(mem stack)
					  (if index
						  (error "Cannot index into a memory variable: ~A" var)
						  `(reg sp ,(* 4 (1- (fourth local))))))
					('(mem array)
					  (if index
					      (if (numberp index)
							  `(reg sp ,(+ index (var-sp-offset local)))
							  `(reg sp ,(check-not-nil (get-reg-or-num index)) ,(var-sp-offset local)))
						  `(reg sp ,(var-sp-offset local))))))
			  (global
			   (if (eq 'addr (second global))
			       ; Is address-type (string or array)
			       (if index
					   `(lbl ,(fourth global) ,index) ; Indexed array access
					   `(lbl ,(fourth global)))		  ; Array pointer load
				   ; Is value-type (number)
				   (if index
					   (error "Cannot index global value: ~A" var)
					   `(lbl ,(fourth global) 0))))
			  (t (error "Undefined variable: ~A" var)))))

(defun compile-store (store)
	(let* ((src-var (get-var (fifth store)))
		   (ind (second (third store)))
		   (dst-des (get-designator (second store) ind)))
		(check-var-type (fifth store) src-var 'reg "Cannot store from non-register: ~A")
		`(load-store store ,(fourth src-var) ,dst-des)))
		

(defun compile-load (load)
	(let* ((dst-var (get-var (first load)))
		   (ind (second (fifth load)))
		   (src-des (get-designator (fourth load) ind)))
		(check-var-type (first load) dst-var 'reg "Cannot load to non-register: ~A")
		`(load-store load ,(fourth dst-var) ,src-des)))

(defun make-simple-assignment (dst src)
	(let ((dst-var (get-var dst))
          (src-var (get-var src)))
		(if (numberp src)
			`(move ,(fourth dst-var) ,src)
			(if src-var
				(alexandria:eswitch ((subseq src-var 1 3) :test #'test-multi-sym-switch)
					('(reg)
					  `(move ,(fourth dst-var) ,(fourth src-var)))
					('(mem array)
					  `(math + ,(fourth dst-var) sp ,(var-sp-offset src-var))))
				(let ((glob-var (get-global src)))
					(if glob-var
						`(load-store load ,(fourth dst-var) ,(get-designator src nil))
						(error "Invalid assignment: ~A = ~A~%" dst src)))))))

(defun make-expr-assignment-custom-dst (dst-reg src1 op src2)
	(let ((src1-var (get-var src1))
		  (src2-des (get-reg-or-num src2)))
		(check-var-type src1 src1-var 'reg "Source not a register: ~A")
		(check-not-nil src2-des (format nil "Source not a register or number: ~A" src2))
		`(math ,(intern op) ,dst-reg ,(fourth src1-var) ,src2-des)))
	

(defun make-expr-assignment (dst src1 op src2)
	(let ((dst-var (get-var dst)))
		(check-var-type dst dst-var 'reg "Destination not a register: ~A")
		(make-expr-assignment-custom-dst (fourth dst-var) src1 op src2)))
		  
(defun %compile-assignment (dst num-expr)
	(let ((expr-type (first num-expr))
		  (expr		 (second num-expr)))
	  (alexandria:eswitch (expr-type)
		('num-value (make-simple-assignment dst expr))
		('simple-expr (make-expr-assignment dst (first expr) (second expr) (third expr)))
		('negative (make-expr-assignment dst "zero" "-" (second expr))))))
	

(defun compile-assignment (asmt)
	(%compile-assignment (first asmt) (third asmt)))

(defun compile-assignment-by (asmt-by)
	(let ((dst (first asmt-by))
		  (op  (first (second asmt-by)))
		  (src (third asmt-by)))
		(make-expr-assignment dst dst op src)))

(defun compile-unconditional-branch (target-label)
	`(branch always 0 0 ,(third target-label)))

(defun compile-conditional-branch (condition target-label &key invert)
	(let* ((cond-type (first (second condition)))
		   (cond-expr (second (second condition)))
		   (target-num (third target-label)))
		(alexandria:eswitch (cond-type)
			('bool-expr
				(let ((src1-des (get-reg-or-num (first cond-expr)))
					  (op       (if invert 
					                (cdr (assoc (second cond-expr) *inverse-comp* :test #'equal))
									(second cond-expr)))
					  (src2-des (get-reg-or-num (third cond-expr))))
					(if (and src1-des src2-des)
						(list 'branch (intern op) src1-des src2-des target-num)
						(error "Invalid conditional: ~A ~A ~A" (first cond-expr) op (second cond-expr)))))
			('num-value
				(if (numberp cond-expr)					
					(let ((invert (to-bool invert))
						  (is-zero (to-bool (= cond-expr 0))))
						(list 'branch (if (eq invert is-zero) 'always 'never)))
					(let ((var (get-var cond-expr)))
						(check-var-type cond-expr var 'reg "Cannot branch on variable: ")
						`(branch ,(if invert '== '!=) ,(fourth var) zero ,target-num))))
			('simple-expr
				(list (make-expr-assignment-custom-dst 'tp (first cond-expr) (second cond-expr) (third cond-expr))
					  `(branch ,(if invert '== '!=) tp zero ,target-num))))))

(defun compile-if-else (if-else)
	(let ((bool-expr (second if-else))
	      (then-stmt (third if-else))
		  (else-label (make-label 'branch "else"))
		  (else-stmt (second (fourth if-else)))
		  (end-label (make-label 'branch "if_end")))
		(list (compile-conditional-branch bool-expr else-label :invert t)
			  (compile-statement then-stmt)
			  (if else-stmt 
				(list (compile-unconditional-branch end-label)
				      else-label
				      (compile-statement else-stmt)
				      end-label)
				else-label))))
				  
(defun compile-while (while)
	(let ((loop-label (make-label 'branch "while"))
		  (bool-expr (second while))
	      (body-stmt (third while))
		  (end-label (make-label 'branch "while_end")))
		(let ((*compiler-break-target* end-label))
			(list loop-label
				  (compile-conditional-branch bool-expr end-label :invert t)
				  (compile-statement body-stmt)
				  (compile-unconditional-branch loop-label)
				  end-label))))

(defun compile-retire (retire)
	(loop for identifier in (remove-delimiter (second retire))
		do (let ((var (get-var identifier)))
				(alexandria:eswitch ((third var))
					('saved (push (fourth var) *riscv-saved-regs*))
					('temp (push (fourth var) *riscv-temp-regs*))
					('perm (error "Cannot retire permanent variable: ~A" identifier))
					('stack (push (fourth var) *compiler-available-stack*))
					('array (loop with base = (fourth var)
								  for i from (1- (fifth var)) downto 0
								  do (push (+ i base) *compiler-available-stack*))))
				(setf *compiler-vars* (delete var *compiler-vars*))
				nil)))

(defun compile-do-while (do-while)
	(let ((loop-label (make-label 'branch "do_while"))
	      (body-stmt (second do-while))
		  (bool-expr (fourth do-while))
		  (*compiler-break-target* (make-label 'branch "do_while_break")))
		(list loop-label
			  (compile-statement body-stmt)
			  (compile-conditional-branch bool-expr loop-label)
			  *compiler-break-target*)))

(defun compile-function-call (function-call)
	(pushnew 'ra *compiler-used-saved-regs*)
	(let ((target (first function-call))
	      (args   (remove-delimiter (third function-call))))
		(if (equal '(nil) args)
			`(call ,target)
			(list (loop for arg in args
						for var in *riscv-arg-vars*
						collect (%compile-assignment var arg))
				  `(call ,target)))))
			

(defun compile-compiler-call (compiler-call)
	(let ((command (second compiler-call))
		  (args (remove-delimiter (fourth compiler-call))))
		(alexandria:eswitch (command :test #'equal)
			("regvar"
				(let ((var-name (first args))
				      (reg (second args))
					  (type (third args)))
					`(,var-name reg ,(read-from-string type) ,(read-from-string reg)))))))

(defun compile-break (break)
	(unless *compiler-break-target*
		(error "Not in a breakable loop!"))
	(if (second break)
		(compile-conditional-branch (second break) *compiler-break-target*)
		(compile-unconditional-branch *compiler-break-target*)))

(defun compile-return (return)
	(let ((ret0 (first (second return)))
		  (ret1 (second (second (second return)))))
		(list
			(if ret1
				(list 
					(%compile-assignment "register_tp" ret0)
					(%compile-assignment "ret1" ret1)
					`(move a0 tp))
				(when ret0 (%compile-assignment "ret0" ret0)))
			(compile-unconditional-branch *compiler-return-label*))))

(defun compile-label (label)
	(make-label 'label (first label)))
	
(defun compile-goto (goto)
	(let ((dummy-label `(0 0 ,(second goto) ,(second goto))))
		(if (third goto)
			(compile-conditional-branch (third goto) dummy-label)
			(compile-unconditional-branch dummy-label))))

(defun compile-statement (statement)
	(let ((type (first statement))
		  (content (second statement)))
		(alexandria:eswitch (type)
			('block         (compile-block         content))
			('declaration   (compile-declaration   content))
			('local-array   (compile-local-array   content))
			('assignment    (compile-assignment    content))
			('assignment-by (compile-assignment-by content))
			('store         (compile-store         content))
			('load          (compile-load          content))
			('if-else       (compile-if-else       content))
			('while         (compile-while         content))
			('retire        (compile-retire        content))
			('do-while      (compile-do-while      content))
			('function-call (compile-function-call content))
			('compiler-call (compile-compiler-call content))
			('return        (compile-return        content))
			('break			(compile-break		   content))
			('label         (compile-label         content))
			('goto          (compile-goto          content))
			('empty nil))))

(defun compile-func-args (args)
	(let ((args (remove-delimiter args)))
		(loop for arg in args
			  for var in *riscv-arg-vars*
			  for reg in *riscv-arg-regs*
			  collect (if (first arg)
						  (let* ((decl `(,(first arg) ((,(second arg) ("=" (num-value ,var)) ";")))))
							    (compile-declaration decl))
						  (progn 
							(push `(,(second arg) reg arg ,reg) *compiler-vars*)
							nil)))))
			

(defun compile-func (func)
	(let ((name (second func))
		  (args (fourth func))
		  (body (sixth func)))
		
		(shadow-globals (*compiler-used-saved-regs*
						 *compiler-available-stack*
						 *compiler-stack-var-count*
						 *compiler-vars*
						 *compiler-break-target*
						 *riscv-saved-regs*
						 *riscv-temp-regs*
						 *riscv-arg-regs*)
			(let* ((*compiler-return-label* (make-label 'return name))
				   (arg-setup (compile-func-args args))
				   (comp-body (compile-statement body))
				   (used-stack-space (+ *compiler-stack-var-count* (length *compiler-used-saved-regs*))))
				`(,(make-label 'function name)
				  (stack-space ,used-stack-space)
				  (backup-regs ,*compiler-stack-var-count* ,@*compiler-used-saved-regs*)
				  ,arg-setup
				  ,comp-body
				  ,*compiler-return-label*
				  (restore-regs ,*compiler-stack-var-count* ,@*compiler-used-saved-regs*)
				  (stack-space ,(- used-stack-space))
				  (return))))))


(defun compile-global-array-designator (val)
	(let ((data-size (first val))
		  (init-type (first (third val)))
		  (init-cont (second (third val))))
		(alexandria:eswitch (init-type)
			('array-size `(,data-size ,(second init-cont)))
			('array-content `(,data-size ,(remove-delimiter (second init-cont)))))))

(defun compile-global (global)
	(let* ((identifier (first global))
		   (type (first (third global)))
		   (value (second (third global)))
		   (label (make-label 'global identifier))
		   (label-num (third label)))
		(push label *compiler-data-segment*)
		(alexandria:eswitch (type)
			('string (push `(global string ,value) *compiler-data-segment*)
					 (push `(,identifier addr string ,label-num) *compiler-global-variables*))
			('number (push `(global number ,value) *compiler-data-segment*)
					 (push `(,identifier value number ,label-num) *compiler-global-variables*))
			('array  (push `(global array ,(compile-global-array-designator value)) *compiler-data-segment*)
					 (push `(,identifier addr array ,label-num) *compiler-global-variables*)))
		nil))
					 

(defun compile-to-intermediate (program)
	(let ((prog (loop for part in program
					  collect (alexandria:eswitch ((first part))
								('func-def (compile-func (second part)))
								('global (compile-global (second part)))))))
		(list '(segment data)
			   (nreverse *compiler-data-segment*)
			   '(segment text)
			   prog)))









(defparameter *intermediate-instructions*
	'(stack-space move load-store branch math label return
	  backup-regs restore-regs call segment global comment .globl))

(defun int-instr-p (item)
	(and (listp item)
		 (find (first item) *intermediate-instructions*)))

(defun %flatten-if (to-flatten last test-stop)
	(if to-flatten
		(if (funcall test-stop to-flatten)
			(cons to-flatten last)
			(if (listp to-flatten)
				(loop for item in to-flatten
					do (setf last (%flatten-if item last test-stop))
					finally (return last))
				(error "Cannot flatten more: ~S~%" to-flatten)))
		last))

(defun flatten-intermediate (intermediate)
	(nreverse (%flatten-if intermediate nil #'int-instr-p)))
	; (nreverse (%flatten-intermediate intermediate nil)))










(defparameter *translator-labels* nil)
(defparameter *translator-label-counter* 0)

(defun get-label-num ()
	(write-to-string (incf *translator-label-counter*)))

(defun get-label-from-num (num)
	(cdr (assoc num *translator-labels*)))

(defun reg-to-string (reg)
	(string-downcase (string reg)))

(defun num-to-string (num)
	(let ((neg (< num 0))
          (num (abs num)))
		(concatenate 'string (when neg "-") "0x" (write-to-string num :base 16))))

(defun num-in-bit-range (bits num)
	(let* ((bit-max (ash 1 (1- bits)))
		   (max (1- bit-max))
		   (min (- bit-max)))
		(<= min num max)))

(defun generate-reg-sav-backup (riscv-instr inter-instr)
	(let ((offset (second inter-instr))
		  (regs (cddr inter-instr)))
		(loop for reg in regs
			  with i = (* 4 offset)
			  collect (format nil "    ~A ~A, ~A(sp)"
							  riscv-instr
							  (reg-to-string reg)
							  i)
			  do (incf i 4))))

(defun operator-to-instr (operator)
	(alexandria:eswitch (operator)
		('+ "add")
		('- "sub")
		('\| "or")
		('& "and")
		('^ "xor")
		('<< "sll")
		('>> "srl")
		('>>> "sra")
		('< "slt")))

(defun execute-bool (operator op1 op2)
	(funcall (symbol-function operator)
			 op1 op2))

(defun bool-to-instr (operator)
	(alexandria:eswitch (operator)
		('< "blt")
		('> "bgt")
		('<= "ble")
		('>= "bge")
		('== "beq")
		('!= "bne")))

(defmacro let-if (cond lets &body body)
	(with-gensyms (cond-result)
		`(let ((,cond-result ,cond))
			(let (,@(loop for let in lets collect `(,(first let) (if ,cond-result ,(second let) ,(first let)))))
				,@body))))

(defun translate-intermediate-instr (instr)
	(alexandria:switch ((first instr))
		('segment 
			(alexandria:eswitch ((second instr))
				('data ".data")
				('text ".text")))
		('label (concatenate 'string (get-label-from-num (third instr)) ":"))
		('call (concatenate 'string "    jal " (second instr)))
		('stack-space (unless (= (second instr) 0)
						  (concatenate 'string "    addi sp, sp, " (num-to-string (* -4 (second instr))))))
		('return "    ret")
		('backup-regs (generate-reg-sav-backup "sw" instr))
		('restore-regs (generate-reg-sav-backup "lw" instr))
		('math
			(let* ((operator (second instr))
				   (dst (reg-to-string (third instr)))
				   (op1 (reg-to-string (fourth instr)))
				   (op2 (fifth instr))
				   (riscv-instr (operator-to-instr operator)))
				(if (numberp op2) ;Is immediate
					(let-if (eq '- operator)
							((riscv-instr "add")
							 (op2 (- op2)))
						(if (num-in-bit-range 12 op2)
							(format nil "    ~ai ~a, ~a, ~a" riscv-instr dst op1 (num-to-string op2))
							(list (format nil "    li tp, ~a" (num-to-string op2))
								  (format nil "    ~a ~a, ~a, tp" riscv-instr dst op1))))
					(format nil "    ~a ~a, ~a, ~a" riscv-instr dst op1 (reg-to-string op2)))))
		('branch
			(let ((bool (second instr))
				  (op1 (third instr))
				  (op2 (fourth instr))
				  (dest-label (if (stringp (fifth instr))
								  (fifth instr)
								  (get-label-from-num (fifth instr)))))
				(cond ((eq 'never bool) nil)
					  ((eq 'always bool)
					   (format nil "    b ~a" dest-label))
					  (t (let ((riscv-instr (bool-to-instr bool))
							   (op1-num (numberp op1))
							   (op2-num (numberp op2)))
							(if (or op1-num op2-num)
								(if (and op1-num op2-num)
									(when (execute-bool bool op1 op2)
										  (format nil "    b ~a" dest-label))
									(let* ((num (if op1-num op1 op2))
										   (num-str (num-to-string num))
										   (reg-str (reg-to-string (if op1-num op2 op1))))
										(if (= num 0)
											(format nil "    ~a ~a, zero, ~a" riscv-instr reg-str dest-label)
											(list (format nil "    li tp, ~a" num-str)
												  (format nil "    ~a ~a, tp, ~a" riscv-instr reg-str dest-label)))))
								(format nil "    ~a ~a, ~a, ~a" riscv-instr (reg-to-string op1) (reg-to-string op2) dest-label)))))))
		('move
			(if (numberp (third instr))
				(format nil "    li ~a, ~a" (reg-to-string (second instr))
											(num-to-string (third instr)))
				(unless (equal (second instr) (third instr))
						(format nil "    mv ~a, ~a" (reg-to-string (second instr))
													(reg-to-string (third instr))))))
		('load-store
			(let* ((type (second instr))
				   (type-instr (if (eq 'store type) "sw" "lw"))
				   (reg  (third instr))
				   (reg-str (reg-to-string reg))
				   (mem  (fourth instr))
				   (mem-type (first mem))
				   (mem-src1 (second mem))
				   (mem-src2 (third mem))
				   (num-index (fourth mem)))
				(alexandria:eswitch (mem-type)
					('reg 
						(let ((mem-src1-str (reg-to-string mem-src1)))
							(if (numberp mem-src2)
								(format nil "    ~a ~a, ~a(~a)" type-instr reg-str mem-src2 mem-src1-str)
								(list (format nil "    add tp, ~a, ~a" mem-src1-str (reg-to-string mem-src2))
								      (format nil "    ~a ~a, ~a(tp)" type-instr reg-str (or num-index 0))))))
					('lbl 
						(let ((label-name (get-label-from-num mem-src1)))
							(if mem-src2
								(list (format nil "    la tp, ~a" label-name)
									  (if (numberp mem-src2)
										  (if (num-in-bit-range 12 mem-src2)
											  (format nil "    ~a ~a, ~a(tp)" type-instr reg-str mem-src2)
											  (error "Cannot access global arrays with large indices!"))
										  (list (format nil "    add tp, tp, ~a" (reg-to-string mem-src2))
									            (format nil "    ~a ~a, ~a(tp)" type-instr reg-str (or num-index 0)))))
								(if (eq 'load type)
									(format nil "    la ~a, ~a" reg-str label-name)
									(error "Cannot write to array-type variables!"))))))))
		('global
			(let ((type (second instr))
				  (arg  (third instr)))
				(alexandria:eswitch (type)
					('number (format nil "    .word ~a" (num-to-string arg)))
					('string (format nil "    .string ~a" arg))
					
					
					
					;('array (format nil "    .byte 0~a" (repeat-string ",0" (1- arg))))
					('array (format nil "    .~a ~a" (first arg)
					                                 (if (listp (second arg))
														 (format nil "~{~d~^,~}" (second arg))
														 (format nil "~v@{~A,~:*~}0" (1- (second arg)) 0)))))))
		('.globl (format nil ".globl ~a" (second instr)))
						 
		(t instr)))


(defun create-label (label-instr)
	(let ((num (third label-instr))
		  (text (alexandria:eswitch ((second label-instr))
					('global (concatenate 'string "glob_" (get-label-num) "_" (fourth label-instr)))
					('function (fourth label-instr))
					('branch (concatenate 'string "L_" (get-label-num) "_" (fourth label-instr)))
					('return (concatenate 'string "ret_" (fourth label-instr)))
					('label (fourth label-instr)))))
		(push `(,num . ,text) *translator-labels*)))

(defun prepass-create-labels (intermediate)
	(loop for instr in intermediate
		if (eq 'label (first instr))
		do (create-label instr)))

(defun translate-to-asm (intermediate)
	(prepass-create-labels intermediate)
	(loop for instr in intermediate
		collect (translate-intermediate-instr instr)))
		



(defun flatten-asm (asm-list)
	(nreverse (%flatten-if asm-list nil (lambda (x) (not (listp x))))))

(defun write-string-to-file (out-file string)
	(with-open-file (out out-file :direction :output
								  :if-exists :supersede
								  :if-does-not-exist :create)
		(format out "~A" string)))

(defun full-compile (in-file out-file &key debug-preproc preproc-out debug-parse)
	(let ((*parser-debug-print* debug-parse))
		(let* ((file-lines (uiop:read-file-lines in-file))
			   (comp-unit (preprocess file-lines))
			   (parsed (parse-program comp-unit :use-whole-string t))
			   (replaced (replace-nil-symbol parsed))
			   (intermediate (compile-to-intermediate replaced))
			   (flat-interm (flatten-intermediate intermediate))
			   (translated (translate-to-asm flat-interm))
			   (final-asm (flatten-asm translated)))
			(when debug-preproc
				  (format t "Preprocessed text: ~%~A~%" comp-unit))
			(when preproc-out
				(write-string-to-file preproc-out comp-unit))
			(when (null parsed) 
				(format t "Parse failed!~%")
				(return-from full-compile nil))
			(with-open-file (out out-file :direction :output
										  :if-exists :supersede
										  :if-does-not-exist :create)
				(loop for line in final-asm
					do (format out "~A~%" line))
				(format t "Compile successful!~%")
				t))))





(defun test-final (&optional)
	(let ((*parser-debug-print* t))
		(let* ((file-string (uiop:read-file-string "test.cmm"))
			   (parsed (parse-program file-string :use-whole-string t))
			   (replaced (replace-nil-symbol parsed))
			   (intermediate (compile-to-intermediate replaced))
			   (flat-interm (flatten-intermediate intermediate))
			   (translated (translate-to-asm flat-interm))
			   (final-asm (flatten-asm translated)))
			(when (null parsed)
				  (error "Error parsing program!"))
			(loop for item in flat-interm
				do (format t "~A~%" item))
			(loop for item in final-asm
				do (format t "~A~%" item))
				)))