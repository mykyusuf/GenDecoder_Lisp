; *********************************************             
; *  Fall 2019                                *
; *  Author: M.Yusuf Koçer                    *
; *********************************************

(defparameter *dictionary* (make-hash-table :test #'equal))
(defparameter *document* (make-hash-table :test #'equal))
(defparameter *alfabe* (make-hash-table :test #'equal))
(defparameter *decoded* (make-hash-table :test #'equal))

;dosyadan liste olarak okur
(defun read-as-list (filename)
	
	(with-open-file (stream filename)
      (loop while (peek-char nil stream nil nil)
           collect (read stream)))

)

;Sozlugu hash table a ekler
(defun read-dict (dict)
	(print "Reading dictionary...")
	(with-open-file (s dict)
        (do ((l (read-line s) (read-line s nil 'eof)))
            ((eq l 'eof) "Done.")
        (add l)))

)
;Kelimede olan harfleri belirtilen harflerle degistirir
(defun replace-all (string part replacement &key (test #'char=))
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))
)
;Alfabe icin tum permutasyonları olusturur
(defun permutasyon (fun lst)
  (if (null lst) (funcall fun nil)
    (map nil
       (lambda (x)
         (permutasyon
          (lambda (l) (funcall fun (cons x l))) 
          (remove x lst)))
       lst))
  )

;hash tabledaki key degelerini getirir

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key)
  )

;permutasyondan gelen alfabeye gore sifre cozmeye calisir
(defun changeAlfabe (lst)

	(setq b 0)
	(loop for a from 0 to (- (length (hash-keys *document*)) 1)
   		do (

   			if (equal (spell-checker-0 (print (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (string (nth a (hash-keys *document*))) "a" (nth 0 lst)) "b" (nth 1 lst)) "c" (nth 2 lst)) "d" (nth 3 lst)) "e" (nth 4 lst)) "f" (nth 5 lst))
 "g" (nth 6 lst)) "h" (nth 7 lst)) "i" (nth 8 lst)) "j" (nth 9 lst)) "k" (nth 10 lst)) "l" (nth 11 lst)) "m" (nth 12 lst)) "n" (nth 13 lst)) "o" (nth 14 lst)) "p" (nth 15 lst)) "q" (nth 16 lst)) "r" (nth 17 lst)) "s" (nth 18 lst)) "t" (nth 19 lst)) "u" (nth 20 lst)) "v" (nth 21 lst)) "w" (nth 22 lst)) "x" (nth 23 lst)) "y" (nth 24 lst)) "z" (nth 25 lst)) )) T)
				   (and (setq b (+ b 1))) 
				   (return nil)
			)
	)
	(if (= b (length (hash-keys *document*)) )
   		(print  "Kullanılan Alfabe:") 
	)
	(if (= b (length (hash-keys *document*)) )
   		(print  lst) 
	)
	(if (= b (length (hash-keys *document*)) )
   		(loop for a from 0 to (- (length (hash-keys *document*)) 1)
   		do (

   			print (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (replace-all (string (nth (- (- (length (hash-keys *document*)) 1) a) (hash-keys *document*))) "a" (nth 0 lst)) "b" (nth 1 lst)) "c" (nth 2 lst)) "d" (nth 3 lst)) "e" (nth 4 lst)) "f" (nth 5 lst))
 "g" (nth 6 lst)) "h" (nth 7 lst)) "i" (nth 8 lst)) "j" (nth 9 lst)) "k" (nth 10 lst)) "l" (nth 11 lst)) "m" (nth 12 lst)) "n" (nth 13 lst)) "o" (nth 14 lst)) "p" (nth 15 lst)) "q" (nth 16 lst)) "r" (nth 17 lst)) "s" (nth 18 lst)) "t" (nth 19 lst)) "u" (nth 20 lst)) "v" (nth 21 lst)) "w" (nth 22 lst)) "x" (nth 23 lst)) "y" (nth 24 lst)) "z" (nth 25 lst)) )
					
	)	
	)
	(if (= b (length (hash-keys *document*)) )
   		(quit)
	)
)
;sozluku hash table a atar 

(defun add (word)
	(setf (gethash (string-downcase word) *dictionary*) t))

;documenti hash table a ekler

(defun adddoc (lst)

	(let ((res ()))
  	(dolist (x lst (nreverse res))
      (setf (gethash (string-downcase x) *document*) t)) ))

;Brtue force ile arama yapar ancak digerine gore cok yavas
(defun spell-checker-0 (word)
	;you should implement this function
	(setf x (read-as-list "dictionary2.txt"))

	(loop for a from 0 to (- (length x) 1)
   		do (
   			if (equal (string-downcase word) (string-downcase(nth a x)))
   				(return T)
		)	
	)
)

;Hash ile hızlı sekilde arama yapar
(defun spell-checker-1 (word)
 	(gethash (string-downcase word) *dictionary*)	
)

;Brute force veya hash table ile cozumleme yapar, permutasyondan gelen alfabeleri surekli deneyerek dogru kelimeyi bulmaya calisir
(defun Gen-Decoder-A (paragraph)	
	(read-dict "dictionary2.txt")
	(adddoc (read-as-list paragraph))
	(permutasyon #'changeAlfabe '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

)

(defun Gen-Decoder-B-0 (paragraph)
  	;you should implement this function
)

(defun Gen-Decoder-B-1 (paragraph)
  	;you should implement this function
)

;Code-Breaker gelen secime gore Decoderi belirler
(defun Code-Breaker (document decoder)
  	;you should implement this function
  	(if (equal decoder "Gen-Decoder-A")
   		(Gen-Decoder-A document)
	)
	(if (equal decoder "Gen-Decoder-B")
   		(Gen-Decoder-B document)
	)

)

(defun test_on_test_data ()

	;document icin alttaki ismi degistirin
	;spell checker icin changeAlfabe icerisinedeki spell checkeri degisebilirsiniz

	(Code-Breaker "document2.txt" "Gen-Decoder-A")

	;spell checkerlar icin testler
	;(spell-checker-0 "this")
	;(spell-checker-1 "this")

)

;; test code...
(test_on_test_data)

