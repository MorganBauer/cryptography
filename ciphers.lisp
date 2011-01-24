(defun numbers->text (numbers)
  (coerce (loop for code in numbers
             collect (code-char (+ code 97))) 'string))

(defun text->numbers (text)
  (loop for char across (string-downcase (remove #\Space text))
     collect (- (char-code char) 97)))

(defun c-shift (text)
  (loop for shift from 1 to 24
     collect (numbers->text (loop for num in (text->numbers text)
                               collect (mod (+ num shift) 26)))))

(defun vignere-encipher (text key)
  (numbers->text
   (loop for char in (text->numbers text)
      for loc from 0
      with key = (text->numbers key)
      ;; do (print (nth (mod loc (length key)) key))
      collect (mod (+ char (nth (mod loc (length key)) key)) 26))))

(defun vignere-decipher (text key)
  (numbers->text
   (loop for char in (text->numbers text)
      for loc from 0
      with key = (text->numbers key)
      ;; do (print (nth (mod loc (length key)) key))
      collect (mod (- char (nth (mod loc (length key)) key)) 26))))


(defun affine-encipher (text a b)
  "a and b must be relatively prime"
  (numbers->text
   (loop for char in (text->numbers text)
      collect (mod (+ (* a char) b) 26))))

(defun affine-decipher (text a b)
  "a and b must be relatively prime"
  (let ((a (loop for i from 1 by 2
             until (= (mod (* a i) 26) 1)
              finally (return i))))
    (numbers->text
     (loop for char in (text->numbers text)
        collect (mod (* a (- char b)) 26)))))

(defun vignere-cipher-test ()
  (let* ((plaintext "attack at dawn")
         (key "lemon")
         (ciphertext (vignere-encipher plaintext key))
         (dec-text (vignere-decipher ciphertext key)))
    (print plaintext)
    (print ciphertext)
    (print dec-text)))



;; playfair from stallings 5e pg 61
;; kxjey urebe zwehe wrytu heyfs krehe goyfi wtttu olksy cajpo botei zontx bybnt goney cuzwr gdson sxbou ywrhe baahy usedq
;; KX JE YU RE BE ZW EH EW RY TU HE YF SK RE HE GO YF IW TT TU OL KS YC AJ PO BO TE IZ ON TX BY BN TG ON EY CU ZW RG DS ON SX BO UY WR HE BA AH YU SE DQ
;; with tt into tt
;; pt iw um on eo we ni ne la st in uz ti on in bl uz ke tt st ry it af tw il es sw me re su fo ve xc re zo ft we lv ex re qu es mu na in co rk um io nx
