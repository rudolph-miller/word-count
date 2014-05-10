#!/usr/bin/env clisp

;input comes from STDIN
;then split to each words
;<word, 1>

(defvar *input* nil)
(defvar *output* nil)

(setf *input* "My name is My name is My name is Tomoya Tomoya")

(defun sen->list (sen)
  (labels ((iter (before after acc)
				 (if (not (equal before ""))
				   (if (equal (first-chr before) " ")
					 (iter (cdr-chrs before) (cons acc after) "")
					 (iter (cdr-chrs before) after (concatenate 'string acc (first-chr before))))
				   (reverse (cons acc after)))))
	(iter sen nil "")))

(defun slice (chrs start &optional (end nil))
  (if (null end)
	(setf end (1+ start)))
  (labels ((iter (chrs acc result)
				 (if (< acc start)
				   (iter (cdr-chrs chrs) (1+ acc) result)
				   (if (< acc end)
					 (iter (cdr-chrs chrs) (1+ acc) (concatenate 'string result (first-chr chrs)))
					 result))))
	(iter chrs 0 "")))

(defun first-chr (chrs)
  (subseq chrs 0 1))

(defun cdr-chrs (chrs)
  (if (not (equal chrs ""))
	(subseq chrs 1)
	nil))


(defun mapper (input)
  "define output format"
  (let ((line (sen->list input)))
	(mapcar #'(lambda (x) (list x 1)) line)))

(defun get-key (key-val)
  "'key	val'-> key"
  (car key-val))

(defun get-val (key-val)
  (cadr key-val))

(defun sort-f (lst)
  (sort lst #'string< :key #'get-key))

(defun reducer (lst)
  (labels ((iter (lst acc result)
				 (if (not (null (cdr lst)))
				   (if (equal (get-key (car lst)) (get-key acc))
					 (iter (cdr lst) (incr acc) result)
					 (iter (cdr lst) (car lst) (if (null acc)  nil (cons acc result))))
				   (if (equal (get-key (car lst)) (get-key acc))
					 (cons (incr acc) result)
					 (cons (car lst) (cons acc result))))))
	(iter lst nil nil)))

(defun incr (key-val)
  (list (get-key key-val) (1+ (get-val key-val))))

(defun f-input (fname)
  (with-open-file (f fname :direction :input)
	(read-all f)))

(defun read-all (fname)
  (loop for i in (range 100)
		do (read-line fname)))


(defun range (num)
  (labels ((iter (cnt acc)
				 (if (not (> cnt num))
				   (iter (1+ cnt) (cons cnt acc))
				   (nreverse acc))))
	(iter 0 nil)))

(setf *input* (f-input "4300.txt"))

(defun main ()
  (print (sort-result (reducer (sort-f (mapper *input*)))) *output*))

(defun read-file (fname)
  (with-open-file (f fname :direction :input)
	(let ((buf (make-string (floor (file-length f) 1000))))
	  (read-sequence buf f)
	  buf)))

(defun sort-result (lst)
  (sort lst #'sort-fn))

(defun sort-fn (item1 item2)
  (if (> (get-val item1) (get-val item2))
	t
	nil))

(setf *input* (read-file "4300.txt"))
(setf *output*  (open "log.txt" :direction :output :if-exists :supersede))


(main)


(print "hi")

