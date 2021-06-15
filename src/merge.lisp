;; #!/usr/bin/env /usr/local/bin/sbcl --script

;; Copyright 2020 IBM

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.


;; (load (merge-pathnames ".sbclrc" (user-homedir-pathname)))
(ql:quickload '(:str :cl-fad :cl-conllu :cl-ppcre) :silent t)

(defpackage :merge-pb
  (:use :cl :cl-conllu :cl-ppcre))

(in-package :merge-pb)

;; utilities


(defun format-token (tk)
  "for use with conllu.draw:tree-sentence function."
  (let ((args (cadr (assoc "Args"  
			   (mapcar (lambda (e) (str:split #\= e))
				   (str:split #\| (token-misc tk)))
			   :test #'equal))))
    (format nil "~a ~a ~a  ~a" 
	    (token-form tk)
	    (token-upostag tk)
	    (token-deprel tk)
	    (cl-ppcre:regex-replace "(\\*/?)+$" args ""))))


(defun token-misc-alist (tk)
  (mapcar (lambda (p)
	    (let ((pair (str:split #\= p)))
	      (ecase (length pair)
		(2 (cons (car pair) (cadr pair)))
		(1 (cons (car pair) nil)))))
	  (str:split #\| (token-misc tk))))

(defun alist-update (alist key value &optional (test #'equal))
  "Update the value of a key or add a cell."
  (let ((cell (assoc key alist :test test)))
    (if cell
        (progn (setf (cdr cell) value) alist)
        (acons key value alist))))

(defun update-token-misc (tk alist)
  (setf (token-misc tk)
	(format nil "~{~a~^|~}"
		(mapcar (lambda (e)
			  (if (cdr e)
			      (format nil "~a=~a" (car e) (cdr e))
			      (format nil "~a" (car e))))
			alist))))


(defun extract-token-misc (s field)
  (mapcar (lambda (tk)
	    (cdr (assoc field (token-misc-alist tk) :test #'equal)))
	  (sentence-tokens s)))


(defun merge-misc (m1 m2)
  (let* ((m1 (str:split #\| m1))
	 (m2 (str:split #\| m2))
	 (m  (remove-duplicates (append m1 m2) :test #'equal)))
    (format nil "~{~a~^|~}"
	    (if (> (length m) 1) (remove "_" m :test #'equal) m))))


;; main code

(defun merge-sentences (ud pb)
  "update the UD sentence with information from the misc fileds of the
   tokens from PB and combine the metadata."
  (if (not (equal (length (sentence-tokens ud))
		  (length (sentence-tokens pb)))) 
      (progn 
	(set (sentence-meta ud)
	     (append (sentence-meta ud)
		     (list (cons "error" "different number of tokens from pb")
			   (cons "text" (sentence->text ud)))))
	ud)
      (loop for a in (sentence-tokens ud)
	    for b in (sentence-tokens pb)
	    do (setf (token-misc a)
		     (merge-misc (token-misc a) (token-misc b)))
	    finally (progn
		      (setf (sentence-meta ud)
			    (append (sentence-meta ud)
				    (append (sentence-meta pb)
					    (list (cons "text" (sentence->text ud))))))
		      (return ud)))))


(defun parse-args (alist)
  (labels ((tk-type (s)
	     (let ((cpat "^\\(([^()]+)\\*\\)$")
		   (ipat "^\\(([^()]+)\\*$"))
	       (cond
		 ((equal s "*")  (cons :star nil))
		 ((equal "*)" s) (cons :end nil))
		 ((cl-ppcre:scan-to-strings cpat s)
		  (multiple-value-bind (m r)
		      (cl-ppcre:scan-to-strings cpat s)
		    (declare (ignore m))
		    (cons :complete (aref r 0))))
		 ((cl-ppcre:scan-to-strings ipat s)
		  (multiple-value-bind (m r)
		      (cl-ppcre:scan-to-strings ipat s)
		    (declare (ignore m))
		    (cons :ini (aref r 0))))
		 (t (error "non-expected token [~a]!" s)))))
	   (aux (alist pos curr res)
	     (if (null alist)
		 (reverse res)
		 (let ((tk (car alist)))
		   (ecase (car (tk-type tk))
		     (:star
		      (aux (cdr alist) (1+ pos) curr res))
		     (:end
		      (aux (cdr alist) (1+ pos) nil (cons (list (car curr) (cdr curr) pos) res)))
		     (:ini
		      (aux (cdr alist) (1+ pos) (cons (cdr (tk-type tk)) pos) res))
		     (:complete
		      (aux (cdr alist) (1+ pos) nil (cons (list (cdr (tk-type tk)) pos pos) res))))))))
    (aux alist 0 nil nil)))


(defun span-head (s ini-p end-p)
  (let ((all-tokens (sentence-tokens s)))
    (labels ((get-token-head (tk)
	       (nth (1- (token-head tk)) all-tokens))
	     (aux (tks heads)
	       (cond
		 ((null tks)
		  (remove-duplicates heads :key #'token-id :test #'equal))

		 ((equal (token-head (car tks)) 0)
		  (aux (cdr tks) (cons (car tks) heads)))

		 ((<= ini-p (1- (token-head (car tks))) end-p)
		  (aux (cons (get-token-head (car tks)) (cdr tks)) heads))

		 (t (aux (cdr tks) (cons (car tks) heads))))))
      (aux (subseq (sentence-tokens s) ini-p (1+ end-p)) nil))))


(defun srl-sentence (s)
  (let* ((preds  (remove-if (lambda (p) (or (null (cdr p)) (equal "-" (cdr p))))
			    (mapcar #'cons
				    (sentence-tokens s)
				    (extract-token-misc s "Roleset"))))
	 (args   (make-array (list 2 (length (sentence-tokens s)) (length preds))
			     :initial-element nil)))

    (if preds
	(destructuring-bind (i rt ct)
	    (array-dimensions args)
	  (declare (ignore i))
	  (let ((vls (extract-token-misc s "Args")))
	    (assert (equal (length vls) rt))
	    (loop for rv in vls
		  for ri from 0 below rt
		  do (loop for cv in (str:split #\/ rv)
			   for ci from 0 below ct
			   do (assert (equal (length (str:split #\/ rv)) ct)
				      (rt ct rv cv)
				      "rt = ~s  ct = ~s rv = ~s cv = ~s" rt ct rv cv)
			   do (setf (aref args 0 ri ci) cv))))

	  (dotimes (c ct)
	    (mapc (lambda (a)
		    (let* ((heads (span-head s (cadr a) (caddr a))))
		      ;; (assert (equal (length heads) 1))
		      (dolist (head heads)
			(setf (aref args 1 (1- (token-id head)) c)
			      (car a)))))
		  (parse-args (loop for r from 0 below rt collect (aref args 0 r c)))))

	  (loop for tk in (sentence-tokens s)
		do (let ((al (token-misc-alist tk))
			 (vs (loop for c from 0 below ct collect (or (aref args 1 (1- (token-id tk)) c) "_"))))
		     (update-token-misc tk
					(alist-update al "Args" (format nil "~{~a~^/~}" vs))))
		finally (return s)))
	s)))


(defun main (file-ud file-pb file-out)
  (let ((ud-lst (read-conllu file-ud))
	(pb-lst (read-conllu file-pb)))
    (if (not (equal (length ud-lst) (length pb-lst)))
	(format *error-output* "error: files ~a and ~s have different number of sentences~%" file-ud file-pb)
	(with-open-file (out file-out :direction :output :if-exists :supersede)
	  (write-conllu-to-stream (loop for pb in pb-lst
					for ud in ud-lst
					when (null pb)
					  do (format *error-output* "error: one invalid sentence in ~a~%" file-pb)
					when (null ud)
					  do (format *error-output* "error: one invalid sentence in ~a~%" file-ud)
					when (and ud pb)
					  collect (srl-sentence (merge-sentences ud pb)))
				  out)))))
  

(defun run ()
  (cl-fad:walk-directory "/Users/ar/work/propbank/propbank-release/data/ontonotes/"
			 (lambda (fn)
			   (main fn
				 (make-pathname :type "gold_conllu" :defaults fn)
				 (make-pathname :type "merged_conllu" :defaults fn)))
			 :directories t 
			 :test (lambda (fn) (equal "conllu" (pathname-type fn)))))

;; (main (nth 1 sb-ext:*posix-argv*) (nth 2 sb-ext:*posix-argv*) (nth 3 sb-ext:*posix-argv*))


;; pending: error with invalid sentence on the conllu files
