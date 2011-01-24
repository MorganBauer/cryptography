(defparameter +test-R0+ #b11110000101010101111000010101010)
(defparameter m #x0123456789ABCDEF)

(defun test-expansion ()
  (= (expansion-permutation +test-R0+)
     #b011110100001010101010101011110100001010101010101))

(defun initial-permutation-test ()
  (= (initial-permutation m)
     #b1100110000000000110011001111111111110000101010101111000010101010))

(defun initial-feistel-test ()
  (= (logxor (expansion-permutation +test-R0+)
             #b000110110000001011101111111111000111000001110010)
     #b011000010001011110111010100001100110010100100111))

(defun feistel-permutation-test ()
  (= (feistel-permutation #b1011100100000101011010110010111)
     #b00100011010010101010100110111011))

(defun f-test ()
  (= (f +test-r0+ #b000110110000001011101111111111000111000001110010)
     (feistel-permutation #b1011100100000101011010110010111)
     #b00100011010010101010100110111011))

(defun ecb-encrypt-block-test ()
  (and
   (= (ecb-encrypt-block #x8787878787878787 #x0E329232EA6D0D73) 0) ;; from The DES Algorithm Illustrated by J. Orlin Grabbe, initial example
   (= (ecb-encrypt-block #x0123456789abcdef #x133457799BBCDFF1) #x85E813540F0AB405) ;; ditto, worked example
   (= (ecb-encrypt-block #x02468aceeca86420 #x0f1571c947d9e859) #xda02ce3a89ecac3b) ;; from stallings 5e pg 85 example
   (= (ecb-encrypt-block #x12468aceeca86420 #x0f1571c947d9e859) #x057cde97d7683f2a) ;; from stallings 5e pg 86 alternate plaintext - single bit difference
   (= (ecb-encrypt-block #x02468aceeca86420 #x1f1571c947d9e859) #xEE92B50606B62B0B) ;; from stallings 5e pg 87 alternate key - single bit difference
   ))

(defun ecb-message-test ()
  (with-output-to-string (*standard-output*)
    (loop for char across "Your lips are smoother than vaseline" ; example message from DES illustrated
       do (format t "~x" (char-code char)))))
;; (list  596F7572206C6970 732061726520736D 6F6F746865722074 68616E2076617365 6C696E65)


(defun run-all-tests ()
  (and (test-expansion)
       (initial-permutation-test)
       (initial-feistel-test)
       (feistel-permutation-test)
       (f-test)
       (ecb-encrypt-block-test)))
