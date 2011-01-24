(defvar +IP+
  (list
   58 50 42 34 26 18 10 2
   60 52 44 36 28 20 12 4
   62 54 46 38 30 22 14 6
   64 56 48 40 32 24 16 8
   57 49 41 33 25 17  9 1
   59 51 43 35 27 19 11 3
   61 53 45 37 29 21 13 5
   63 55 47 39 31 23 15 7))

(defvar +inverse-IP+
  (list
   40  8  48  16  56 24  64 32
   39  7  47  15  55 23  63 31
   38  6  46  14  54 22  62 30
   37  5  45  13  53 21  61 29
   36  4  44  12  52 20  60 28
   35  3  43  11  51 19  59 27
   34  2  42  10  50 18  58 26
   33  1  41   9  49 17  57 25))


(defvar +S1+
  (list
   14  4  13  1   2 15  11  8   3 10   6 12   5  9   0  7
   0  15   7  4  14  2  13  1  10  6  12 11   9  5   3  8
   4   1  14  8  13  6   2 11  15 12   9  7   3 10   5  0
   15 12   8  2   4  9   1  7   5 11   3 14  10  0   6 13))

(defvar +S2+
  (list
   15  1   8 14   6 11   3  4   9  7   2 13  12  0   5 10
   3  13   4  7  15  2   8 14  12  0   1 10   6  9  11  5
   0  14   7 11  10  4  13  1   5  8  12  6   9  3   2 15
   13  8  10  1   3 15   4  2  11  6   7 12   0  5  14  9))

(defvar +S3+
  (list
   10  0   9 14   6  3  15  5   1 13  12  7  11  4   2  8
   13  7   0  9   3  4   6 10   2  8   5 14  12 11  15  1
   13  6   4  9   8 15   3  0  11  1   2 12   5 10  14  7
   1  10  13  0   6  9   8  7   4 15  14  3  11  5   2 12))

(defvar +S4+
  (list
   7  13  14  3   0  6   9 10   1  2   8  5  11 12   4 15
   13  8  11  5   6 15   0  3   4  7   2 12   1 10  14  9
   10  6   9  0  12 11   7 13  15  1   3 14   5  2   8  4
   3  15   0  6  10  1  13  8   9  4   5 11  12  7   2 14))

(defvar +S5+
  (list
   2  12   4  1   7 10  11  6   8  5   3 15  13  0  14  9
   14 11   2 12   4  7  13  1   5  0  15 10   3  9   8  6
   4   2   1 11  10 13   7  8  15  9  12  5   6  3   0 14
   11  8  12  7   1 14   2 13   6 15   0  9  10  4   5  3))

(defvar +S6+
  (list
   12  1  10 15   9  2   6  8   0 13   3  4  14  7   5 11
   10 15   4  2   7 12   9  5   6  1  13 14   0 11   3  8
   9  14  15  5   2  8  12  3   7  0   4 10   1 13  11  6
   4   3   2 12   9  5  15 10  11 14   1  7   6  0   8 13))

(defvar +S7+
  (list
   4  11   2 14  15  0   8 13   3 12   9  7   5 10   6  1
   13  0  11  7   4  9   1 10  14  3   5 12   2 15   8  6
   1   4  11 13  12  3   7 14  10 15   6  8   0  5   9  2
   6  11  13  8   1  4  10  7   9  5   0 15  14  2   3 12))

(defvar +S8+
  (list
   13  2   8  4   6 15  11  1  10  9   3 14   5  0  12  7
   1  15  13  8  10  3   7  4  12  5   6 11   0 14   9  2
   7  11   4  1   9 12  14  2   0  6  10 13  15  3   5  8
   2   1  14  7   4 10   8 13  15 12   9  0   3  5   6 11))

(defvar +sboxes+ (list +s1+ +s2+ +s3+ +s4+ +s5+ +s6+ +s7+ +s8+))

(defvar +pc-1+
  (list
   57 49  41 33  25  17  9
   1  58  50 42  34  26 18
   10  2  59 51  43  35 27
   19 11   3 60  52  44 36
   63 55  47 39  31  23 15
   7  62  54 46  38  30 22
   14  6  61 53  45  37 29
   21 13   5 28  20  12  4))

(defvar +pc-2+
  (list
   14  17 11  24   1  5
   3   28 15   6  21 10
   23  19 12   4  26  8
   16   7 27  20  13  2
   41  52 31  37  47 55
   30  40 51  45  33 48
   44  49 39  56  34 53
   46  42 50  36  29 32))

(defvar +ep+
  (list
   32   1  2   3   4  5
   4    5  6   7   8  9
   8    9 10  11  12 13
   12  13 14  15  16 17
   16  17 18  19  20 21
   20  21 22  23  24 25
   24  25 26  27  28 29
   28  29 30  31  32  1))

(defvar +P+
  (list  16   7  20  21
         29  12  28  17
         1   15  23  26
         5   18  31  10
         2    8  24  14
         32  27   3   9
         19  13  30   6
         22  11   4  25))

(defun left-circular-shift ()
  "a more specialized version of 'rotate-byte'"
  (print "Not implemented yet"))

(defun rotate-byte (count bytespec integer)
  (let ((size (byte-size bytespec)))
    (when (= size 0)
      (return-from rotate-byte integer))
    (let ((count (mod count size)))
      (flet ((rotate-byte-from-0 (count size integer)
               (let ((bytespec (byte size 0)))
                 (if (> count 0)
                     (logior (ldb bytespec (ash integer count))
                             (ldb bytespec (ash integer (- count size))))
                     (logior (ldb bytespec (ash integer count))
                             (ldb bytespec (ash integer (+ count size))))))))
        (dpb (rotate-byte-from-0 count size (ldb bytespec integer))
             bytespec
             integer)))))

(defparameter *key* #x133457799BBCDFF1)

(defun numberToKey (num)
  (loop for number in
       (loop for hexDigit across (with-output-to-string (*standard-output*)
                                   (format t "~x" num))
          collect (with-output-to-string (*standard-output*)
                    (format t "~b" hexDigit)))
     collect (parse-integer number :radix 16)))



(defun nTK (num)
  (parse-integer (reverse (format nil "~{~A~^~}"
                                  (loop for hexDigit across (with-output-to-string (*standard-output*)
                                                              (format t "~x" num))
                                     collect (with-output-to-string (*standard-output*)
                                               (format t "~4,'0',b"  (parse-integer (string hexDigit) :radix 16)))))) :radix 2))

;; for all permutaions, the most sig bit is bit 1, i.e. it is big-endian

(defun key->subkeypairs (origkey)
  (let (;(origkey #x133457799BBCDFF1)
        (place 0)
        (newkey 0))
    (loop for oldplace in +pc-1+
       for newplace from 55 downto 0
       do (setf oldplace (- 64 oldplace))
       do (setf (ldb (byte 1 oldplace) place) 1)  ;set single bit at where bit value from presubkey is coming from
       ;;do (format t "~&~64,'0',,b" origkey) ;; show original key value
       ;;do (format t "  ~3d~&" oldplace)
       ;;do (format t "~&~64,'0',,b" place) ;; set to 1 under the place we are looking at
       ;; do (format t "  newplace ~3D from oldplace ~3D~2&"
       ;;            (ldb (byte 1 newplace) newkey)
       ;;            (ldb (byte 1 oldplace) origkey))
       do (setf (ldb (byte 1 newplace) newkey)
                (ldb (byte 1 oldplace) origkey))
       do (setf (ldb (byte 1 oldplace) place) 0))
    (format t "final K+ is hex ~7x, bin ~56b" newkey newkey)
    (let ((highKeyC0 (ldb (byte 28 28) newkey))
          (lowKeyD0 (ldb (byte 28 0) newkey)))
      (format t "~2&highkey ~28,'0b ~& lowkey ~28,'0b" highKeyC0 lowKeyD0)
      (values highKeyC0 lowKeyD0))))


(defparameter +keyScheduleRotations+
  (list 1
        2
        4
        6
        8
        10
        12
        14
        15
        17
        19
        21
        23
        25
        27
        28))

(defun create-presubkeypairs (C0 D0)
  ;;(format t "~&~x" c0)
  ;;(format t "~&~x" d0)
  (loop for shift in +keyScheduleRotations+
     collect (rotate-byte shift (byte 28 0) c0) into c-keys
     collect (rotate-byte shift (byte 28 0) d0) into d-keys
     finally (return (values c-keys d-keys))))

(defun presubkeypairs->presubkeys (c-presubkeys d-presubkeys)
  (assert (= (length c-presubkeys) (length d-presubkeys)))
  (loop
     for c in c-presubkeys
     for d in d-presubkeys
     do (setf (ldb (byte 28 28) d) c)
     collect d))

(defun view-presubkeys (c-subkeys d-subkeys) ; &optional print-mode ;  hopefully for binary vs
  (loop
     for c-key in c-subkeys
     for c-index from 0
     for d-key in d-subkeys
     for d-index from 0
     do (format t "~&C~2d = ~28,'0',b~&" c-index c-key)
     do (format t "~&D~2d = ~28,'0b~2&" d-index d-key)))

(defun presubkeys->subkeys (presubkeys)
  "takes a list of presubkeys (i.e. before permuted-choice 2 (+pc-2+))"
  (loop
     for origkey in presubkeys
     for newkeynum from 0
     collect
       (let ((newkey 0))
         (declare (type (unsigned-byte 48) newkey))
         (loop for oldplace in +pc-2+
            for newplace from (1- (length +pc-2+)) downto 0 ;final is 48 bits
            do (setf oldplace (- 56 oldplace)) ;entering has 56 bits
            do (setf (ldb (byte 1 newplace) newkey) ; set the bit in the new key, to the appropriate value from the appropriate place in the old key, as determined by pc2
                     (ldb (byte 1 oldplace) origkey)))
         (format t "~&final newkey K~2d is hex ~7,'0x, bin ~56,'0b" newkeynum newkey newkey)
         newkey)))


(defun key->subkeys (origkey)
  (presubkeys->subkeys (multiple-value-call #'presubkeypairs->presubkeys
                         (multiple-value-call #'create-presubkeypairs
                           (key->subkeypairs origkey)))))

(defun initial-permutation (message-block)
  "performs initial permutation (IP) of 64-bit message-block passed in."
  (let ((permutation 0))
    (declare (type (unsigned-byte 64) permutation))
    (loop for oldplace in +ip+
       for newplace from 63 downto 0           ;final is 64 bits
       do (setf oldplace (- 64 oldplace))      ;entering has 64 bits
       do (setf (ldb (byte 1 newplace) permutation) ; set the bit in the new key, to the appropriate value from the appropriate place in the old key, as determined by pc2
                (ldb (byte 1 oldplace) message-block)))
    (format t "~&final permutation is hex ~8,'0x, bin ~64,'0b" permutation permutation)
    permutation))

(defun inverse-initial-permutation (preoutput-block)
  "performs inverse initial permutation (IP) of 64-bit message-block passed in."
  (let ((output-block 0))
    (declare (type (unsigned-byte 64) output-block))
    (loop for oldplace in +inverse-ip+
       for newplace from 63 downto 0           ;final is 64 bits
       do (setf oldplace (- 64 oldplace))      ;entering has 64 bits
       do (setf (ldb (byte 1 newplace) output-block) ; set the bit in the new key, to the appropriate value from the appropriate place in the old key, as determined by pc2
                (ldb (byte 1 oldplace) preoutput-block)))
    (format t "~&final output-block is hex ~8,'0x, bin ~64,'0b" output-block output-block)
    output-block))

(defun encrypt-block (ip subkeys)
  (let ((l0 (ldb (byte 32 32) ip))
        (r0 (ldb (byte 32 0) ip)))
    (format t "beginning feistel rounds~&")
    (format t "l0 is ~32,'0b~&" l0)
    (format t "r0 is ~32,'0b~&" r0)
    (loop for n from 0 to 15 ;; doing feistel rounds
       for ln-1 = l0 then l ;; remember previous l
       for rn-1 = r0 then r ;; remember previous r
       for l = r0 then rn-1 ;; update l and r
       for r = (logxor l0 (f r0 (nth n subkeys))) then (logxor ln-1 (f rn-1 (nth n subkeys)))
       do (format t "L~2,'0d = ~32,'0b, R~2,'0d = ~32,'0b~&" (1+ n) l (1+ n) r)
       finally (return (logior (ash r 32) l)))))

(defun feistel-permutation (sbox-replacements)
  (declare (type (unsigned-byte 32) sbox-replacements))
  (let ((f-perm 0))
    (declare (type (unsigned-byte 32) f-perm))
    (loop for oldplace in +p+
       for newplace from 31 downto 0           ;final is 32 bits
       do (setf oldplace (- 32 oldplace))      ;entering has 32 bits
       do (setf (ldb (byte 1 newplace) f-perm) ; set the bit in the new key, to the appropriate value from the appropriate place in the old key, as determined by pc2
                (ldb (byte 1 oldplace) sbox-replacements)))
    ;; (format t "~&final f-perm is hex ~8,'0x, bin ~32,'0b" f-perm f-perm)
    f-perm))

(defun f (r key)
  (declare (type (unsigned-byte 48) r key))
  "feistel function"
  (feistel-permutation
   (let ((sbox-addresses
          ;; calculate sbox addresses
          (let* ((exp-perm-r (expansion-permutation r))
                 (r-xor-k (logxor exp-perm-r key)))
            ;;(format t "~&expant permutaion of R is ~48,'0b" exp-perm-r)
            ;;(format t "~& key xor exp perm of R is ~48,'0b" r-xor-k)
            (loop
               for x from 7 downto 0 ; loop across, from left to right
               for bs = (byte 6 (* x 6)) ; 6 bits at a time
               collect (ldb bs r-xor-k)))))
     ;; apply sbox substitutions
     (loop for addr in sbox-addresses
        for sbox in +sboxes+
        for x downfrom 28 by 4 to 0
        for replacement = (sbox-replacement addr sbox)
        ;;do (format t "~&subbing replacement ~6,'0b @ location ~d~&" replacement x)
        with final-substitutions = 0
        ;;do (format t "subst is ~32b~&" final-substitutions)
        do (setf (ldb (byte 4 x) final-substitutions)
                 replacement)
        finally (return final-substitutions)))))

(defun sbox-replacement (addr sbox)
  (let ((row (logior (ash (ldb (byte 1 5) addr) 1) (ldb (byte 1 0) addr)))
        (col (ldb (byte 4 1) addr)))
    (nth (+ col (* row 16)) sbox)))

(defun expansion-permutation (block)
  "expands 32-bit input 'block' into 48-bits"
  (declare (type (unsigned-byte 32) block))
  (let ((expansion 0))
    (declare (type (unsigned-byte 48) expansion))
    (loop for oldplace in +ep+
       for newplace from (1- (length +ep+)) downto 0 ;final is 48 bits
       do (setf oldplace (- 32 oldplace))      ;entering has 32 bits
       do (setf (ldb (byte 1 newplace) expansion) ; set the bit in the new key, to the appropriate value from the appropriate place in the old key, as determined by pc2
                (ldb (byte 1 oldplace) block)))
    ;; (format t
    ;;         "~&final expansion is hex ~6,'0x
    ;;                bin ~48,'0b" expansion expansion)
    expansion))

(defun ecb-encrypt-block (message key)
  (inverse-initial-permutation (encrypt-block (initial-permutation message)
                                              (key->subkeys key))))

(defun ecb-decrypt-block (message key)
  (inverse-initial-permutation (encrypt-block (initial-permutation message)
                                              (reverse (key->subkeys key)))))


;; (defun read-file-blocks (filename)
;;   (with-open-file (stream filename :element-type '(unsigned-byte 8))
;;     (loop with x = (read-file-block stream)
;;          while x = (not nil)
;;          collect x)))

(defun read-file-block (stream)
  (loop for x from 7 downto 0
     with block = 0
     do (setf (ldb (byte 8 (* x 8)) block) (read-byte stream))
     finally (return block)))