;;;; boxen.lisp

;;;; Syntax for strings, comments, and sub-programs that looks like
;;;; boxes.

;;; Copyright (C) 2010 Joseph F. Miklojcik III.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;; NOTE: This has only been tested against the SBCL compiler.
;;; Unicode support in other compilers appears to me as a frightening
;;; monster.  /Caveat compiler/.

(in-package #:boxen)

;;; These just make sure we always call I/O routines correctly.  Do not
;;; export.
(defun readc (stream)
  "Call READ-CHAR with correct optional arguments for operation within
   recursive calls to READ."
  (read-char stream nil nil t))
(defun peekc (stream)
  "Call PEEK-CHAR with correct optional arguments for operation within
   recursive calls to READ."
  (peek-char nil stream nil nil))
(defun writec (c stream)
  "Call WRITE-CHAR with correct optional arguments for operation within
   recursive calls to READ."
  (write-char c stream))
(defun unreadc (c stream)
  "Call UNREAD-CHAR with correct optional arguments for operation within
   recursive calls to READ."
  (unread-char c stream))

(defun consume (in char-list)
  "Discard characters from given stream IN so long as they match some
   character in the string CHAR-LIST.  Return the character read that
   did not match CHAR-LIST."
  (loop
     for c = (readc in)
     if (not (member c char-list))
       do (return c)))

(defun collect-to-eol (in out)
  "READC characters from stream IN until #\NEWLINLE, appending them to
   RESULT using APPEND-ADSTRING.  This does more kibbitzing than
   READ-LINE."
  (loop
     for c = (readc in)
     if (null c) 
       do (error "End of file reached in boxy string.")
     if (member c '(#\─ #\│ #\└ #\┌)) 
       do (warn "Boxy string character found within boxy string.")
     if (equal c #\⌖)
       if (equal (peekc in) #\NEWLINE)
         do (readc in)
         and return (values)
       else
         do (warn "#\⌖ not followed immediately by #\NEWLINE in boxy string.")
     do (writec c out)
     until (equal c #\NEWLINE))
  (values))

(defun collect-lines (in)
  "Starting with stream IN at the beginning of a boxy string line (not
   the first line), return the subsequent boxy string up to and
   including the end."
  (with-output-to-string (out)
    (loop
       do (case (consume in '(#\SPACE #\TAB))
	    ((#\│) (collect-to-eol in out))
	    ((#\└) (progn
		     (unreadc (consume in (list #\─)) in)
		     (return)))
	    ((#\|) (warn (format nil
				  "You must use #\BOX_DRAWINGS_LIGHT_VERTICAL, ~
                                   not #\VERTICAL_LINE, in boxy strings.  ~
                                   Be careful, they look similar in most ~
                                   fonts.")))
	    ((nil) (error "End of file reached in boxy string."))
	    (otherwise
	     (error (format nil "Boxy string line does not begin with #\│, ~
                                 or perhaps it is not closed properly.")))))))

(defun string-reader (stream char)
  "Install this with (SET-MACRO-CHARACTER #\┌ #'STRING-READER) to arm
   boxy string syntax."
  (if (not (equal char #\┌))
      (error "BOXEN::STRING-READER called on character that is not #\┌.")
      (let ((c (consume stream (list #\─ #\SPACE #\TAB))))
	(if (not (equal c #\NEWLINE))
	    (error "Boxy string opening didn't end in #\NEWLINE.")
	    (collect-lines stream)))))

(defun do-special (key string)
  (cond
    ((equal key 'comment) (values))
    ((equal key 'string) string)
    (t (error "Unknown key in boxy special syntax."))))

(defun read-special-key-gabba-gabba-hey (in)
  (with-output-to-string (out)
    (loop
       for c = (readc in)
       if (null c)
         do (error "End of file reached in boxy special key.")
       if (equal c #\═)
	 do (unreadc (consume in '(#\═)) in)
	 and return (values)
       if (equal c #\NEWLINE)
	 do (write-string "comment" out)
	 and return (values)
       do (writec c out))))

(defun read-special-key (in)
  (with-input-from-string (s (read-special-key-gabba-gabba-hey in))
    (read s t nil t)))

(defun working-read-special-line (in)
  (case (consume in '(#\SPACE #\TAB))
    ((#\║) (with-output-to-string (out)
	     (collect-to-eol in out)))
    ((#\╚) (progn
	     (unreadc (consume in '(#\═)) in)
	     :end))
    ((nil) (error "End of file reached in boxy special syntax."))
    (otherwise nil)))

(defun read-special-line (in)
  (let ((c (consume in '(#\SPACE #\TAB))))
    (case c
      ((#\║) (with-output-to-string (out)
	       (collect-to-eol in out)))
      ((#\╚) (progn
	       (unreadc (consume in '(#\═)) in)
	       :end))
      ((#\NEWLINE) "")
      ((nil) (error "End of file reached in boxy special syntax."))
      (otherwise (progn
		   (unreadc c in)
		   (read-preserving-whitespace in t nil t))))))

(defun read-special-lines (stream)
  (loop
     with acc = ()
     for line = (read-special-line stream)
       if (equal line :end)
         do (return acc)
       else
         do (push line acc)))

(defun special-reader (stream char)
  "Install this with (SET-MACRO-CHARACTER #\╔ #'SPECIAL-READER)
   to get boxy style special syntax."
  (if (not (equal char #\╔))
      (error "SPECIAL-READER called on character that is not #\╔")
      (progn
	(unreadc (consume stream '(#\═ #\SPACE)) stream)
	(let ((key (read-special-key stream)) ; these side-effect STREAM in order
	      (c (consume stream '(#\═ #\SPACE))))
	  (format t "~S~%" key)
	  (if (not (equal c #\NEWLINE))
	      (error "Extra junk ~S on first line of boxy special syntax." c)
	      (let ((acc (read-special-lines stream)))
		`(funcall (lambda ()
			    (do-special
			      (quote ,key)
			      (cat ,@(nreverse acc)))))))))))

(defun cat (&rest args)
  "Helper function used in closures generated by SPECIAL-READER.
  Returns a string that is the concatination of string
  representations (via WRITE-STRING) of all arguments in order."
  (with-output-to-string (out)
    (loop
       for str in args
	 if (not (subtypep (type-of str) 'string))
	   do (error "BOXEN::CAT got something that is not a string.")
         do (write-string str out))))

(defmacro arm ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (set-macro-character #\┌ #'boxen:string-reader)
    (set-macro-character #\╔ #'boxen:special-reader)))
