;;; boxen.el

;;; This file contains some ELisp functions to make using boxy syntax
;;; easier.  These should really turn into a minor mode some day.

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

(defun rxs (&rest args)
  "Like RX but does not quote arguments.
   Like RX-TO-STRING but takes multiple arguments, making a SEQ.
   This makes it reasonable to include RXen in RXen.
   <put 'Yo dawg' joke here>"
  (rx-to-string `(seq ,@args)))

(defun fill-rosebud-string (&optional justify)
  "Fill current rosebud string.
   If we're not in a rosebud string, just return nil so that the
   caller can take care of the filling.  JUSTIFY may happen
   someday, as in FILL-PARAGRAPH."
  (interactive)
  (let* ((start-white   '(seq line-start (group (* whitespace))))
	 (line-rx       (rxs start-white '(any "┌│└")))
	 (first-line-rx (rxs 'line-start '(* (not (any "│\""))) "┌"))
	 (last-line-rx  (rxs start-white '"└")) )
    (save-excursion
      (beginning-of-line)
      (if (not (looking-at line-rx))
	  nil
	  (progn 
	    (re-search-backward first-line-rx)
	    (let ((index 0))
	      ;; There's probably a better way than this to get
	      ;; REPLACEMENT set below.
	      (beginning-of-line)
	      (while (not (looking-at "┌"))
		(forward-char)
		(incf index))
	      (forward-line)
	      (let ((replacement (make-string index ?\s)))
		(beginning-of-line)
		(while (not (looking-at last-line-rx))
		  (when (looking-at line-rx)
		    ;; The 1 in this REPLACE-MATCH refers to the GROUP
		    ;; in the START-WHITE rx set by the LOOKING-AT in
		    ;; the WHEN predicate above.
		    (replace-match replacement nil nil nil 1))
		  (forward-line)
		  (beginning-of-line))
		;; The 1 in this REPLACE-MATCH refers to the GROUP in
		;; the START-WHITE rx set by the LOOKING-AT in the
		;; WHILE predicate above.
		(replace-match replacement nil nil nil 1)
		t)))))))

(defun rosebudify-string ()
  "Turn a string in double quotes into the same rosebud style
string."
  (interactive)
  (save-excursion
    (re-search-backward (rx (not (any "\\")) (group "\"")))
    (replace-match "┌─\n│" nil nil nil 1)
    (while (not (looking-at (rx (* nonl) (not (any "\\")) "\"")))
      (forward-line)
      (beginning-of-line)
      (insert-string "│"))
    (re-search-forward (rx (not (any "\\")) (group "\"")))
    (replace-match "\n└─" nil nil nil 1)
    (forward-line -1) ; backward one line
    (fill-rosebud-string)))
      