#|
    CSE341 - PROGRAMMING LANGUAGES
    ASSIGNMENT - G++ LEXER
    
    Author: Ahmet Furkan Ekinci
|#

(setq exitControl1 0) ;exitControl1 will control writing (exit). When user entered "(exit)" then program finish.
(setq exitControlFile 0) ;File ı okuyup çalıştırıp programdan çıkmak için kullandım.



;--------------------------------------------------------------------------

(defun gppinterpreter(&optional filename)
  (loop
    (if (equal exitControl1 1) ;Control for exit
        (progn
            (setq exitControl1 0)
            (format t "You want to out from this program..")
            (return)
        )
    )
    (let ((str "x")  ; define str
          (words (list)) ;bir satırdaki tüm stringgleri bir list içinde tutmak için kullandım
          (wordsFile (list)) ;File dan okuma yapınca tüm line ları list şeklinde tutar.
          (paranthesList (list)) ;bir word içinde parantez varsa parçalanan halini list olarak tutar.
          (wholeList (list))) words ile paranthesList in birleşimlerini loop boyunca tutar ve loop sonunda words liste aktarır.
        (if filename ; eğer file varsa
            (progn
                (incf exitControlFile) ;file varsa 1 kez arttırıyorum
                (if (equal exitControlFile 2) ;eğer ikinci kez arttırılırsa burada programı sonlandırcam.Çünkü okuma bir kez yapılır.
                    (quit) ; programdan çık
                    (progn ;if else gibi blokların bütünlüğünü sağlar
                        (setq wordsFile (read-file-into-lines filename)) ;dosyadan okuyup wordsFile listine attım.
                        ;(setq words (split_string_endline str))
                    )
                )
            )
            (progn
                (setq str (read-line)) ;dosya yok ise satır okur
                (setq words (split_string str)) ;satırı stringlere böler ve liste atar.
            )
        )
        
        (if (equal exitcontrolFile 0) ;eğer file dan okuma yoksa T aksi takdirde nil
            (progn ; if true durumunu kontrol eder
                (whole-operations words)
            )
            (progn
                (loop for myString in wordsFile
                do
                    (setq words (split_string myString)) ;wordsFile satır olarak tutuyordu dosyadan çektiği veriyi burada ise o satırı stringlere ayırıp words listine atıyorum.
                    (whole-operations words)
                )
            ) ;progn son
        )
    )
  )
)

;Dosyadan satır satır okuma yapar.
(defun read-file-into-lines (file-path)
  (with-open-file (file file-path
                       :direction :input
                       :if-does-not-exist :error)
    (let ((lines '())
         (line '()))
      (loop for char = (read-char file nil)
            while char
            do (if (char= char #\Newline)
                   (progn
                     (push (coerce (reverse line) 'string) lines)
                     (setq line '()))
                   (push char line)))
      (push (coerce (reverse line) 'string) lines)
      (nreverse lines)))
)


; function that splits line to words
; parameters: string
; optional: nl (return string)
(defun split_string(str &optional nl)
    (let((i
            (position " " str  ;Boşluk karakterini ararız
                    :from-end t ; Aramaya sondan başlarız
                    :test #'(lambda (a b) (find b a :test #'string=)) ;arama işleminin gerçekleşmesini özelleştirdik
            )
         )
        )
        (if i ; Eğer bir boşluk karakteri bulunursa
            (split_string (subseq str 0 i) ; İlk bölümü alırız
                          (cons (subseq str (1+ i)) nl)) ; Geriye kalanı ve sonuca ekleriz
            (cons str nl) ;Boşluk bulunmazsa, girdi stringini sonuca ekleriz
        )
    )
)


;let ifadesi, bir dizi yerel değişkeni tanımlamak için kullanılır. Bu durumda, last-char adında bir yerel değişken tanımlanıyor. Bu değişken son karakteri temsil edecektir.
;last-char değişkenine değer atamak için char işlevi kullanılır. char işlevi, belirli bir karakterin kodunu döndürür.
;İkinci parametre olarak string işlevi kullanılır. Bu işlev, belirli bir karakter kodunu bir karaktere dönüştürür.
;elt işlevi, bir stringin belirli bir pozisyonundaki karakteri alır. length işlevi ise bir stringin uzunluğunu döndürür. 1- işlemi son karakterin indeksini hesaplar.
;char= işlevi, iki karakterin eşit olup olmadığını kontrol eder. last-char ile #\) (parantez karakteri) karşılaştırılır.
;char= last-char #\) ifadesi, son karakterin ")" (parantez) olup olmadığını kontrol eder ve sonucu döndürür.
(defun is-last-char-parenthesis (str)
    (unless (string= str "")
        (let ((last-char (char (string (elt str (1- (length str)))) 0)))
            (char= last-char #\)))
    )
)

;str stringinin son karakteri b mi değil mi
(defun is-last-char-b (str)
    (unless (string= str "") ; Eğer gelen string boş değilse
        (let ((last-char (char (string (elt str (1- (length str)))) 0))) ; Gelen stringin son karakterini alırız ve last-char değişkenine atarız.
            (char= last-char #\b)) ; Son karakterin "b" karakterine eşit olup olmadığını kontrol ederiz.
    )
)


;kontrollerin ana merkezi burası.
(defun word-control (str)
    (progn
        (operator-control str)
        (valuef-control str)
        (unless (keyword-control str)
            (identifier-control str)
        )
        (if (are-all-digits str)
            (if (> (length str) 0);str stringinin uzunluğunun 0 dan uzun olduğu durumu kontrol ettim.
                (progn
                    (print str)
                    (write-line "Lexical Syntax ERROR. It is just integer value..!")
                    ;(quit)
                )
            )
        )
        (error-valuef-control str)
        (unless (lexical-error-control-widely str)
            (progn
                (write-line "Lexical Syntax ERROR..!")
                ;(quit)
            )
        )
    )
)

;uzunluğu birden fazla olan her stringin lexical olarak dilimize uygun olması için sayı harf ya da _ ve parantez içermesi gerekir. Başka karakterler içeren her string lexical error almalıdır.
(defun lexical-error-control-widely (str)
    (every #'(lambda (char)
        (or (alpha-char-p char)
            (digit-char-p char)
            (char= char #\_)
            (char= char #\()
            (char= char #\))
            (char= char #\+)
            (char= char #\-)
            (char= char #\*)
            (char= char #\/)
            (char= char #\,)))
        str
    )
)

;keywordlerin hepsi tek tek kontrol ediliyor.
(defun keyword-control (str)
    (let ((count 0))
        (if (string= "and" str)
            (progn
                (print str)
                (write-line "KW_AND")
                (incf count)
            )
        )
        (if (string= "or" str)
            (progn
                (print str)
                (write-line "KW_OR")
                (incf count)
            )
        )
        (if (string= "not" str)
            (progn
                (print str)
                (write-line "KW_NOT")
                (incf count)
            )
        )
        (if (string= "equal" str)
            (progn
                (print str)
                (write-line "KW_EQUAL")
                (incf count)
            )
        )
        (if (string= "less" str)
            (progn
                (print str)
                (write-line "KW_LESS")
                (incf count)
            )
        )
        (if (string= "nil" str)
            (progn
                (print str)
                (write-line "KW_NIL")
                (incf count)
            )
        )
        (if (string= "list" str)
            (progn
                (print str)
                (write-line "KW_LIST")
                (incf count)
            )
        )
        (if (string= "append" str)
            (progn
                (print str)
                (write-line "KW_APPEND")
                (incf count)
            )
        )
        (if (string= "concat" str)
            (progn
                (print str)
                (write-line "KW_CONCAT")
                (incf count)
            )
        )
        (if (string= "set" str)
            (progn
                (print str)
                (write-line "KW_SET")
                (incf count)
            )
        )
        (if (string= "def" str)
            (progn
                (print str)
                (write-line "KW_DEF")
                (incf count)
            )
        )
        (if (string= "for" str)
            (progn
                (print str)
                (write-line "KW_FOR")
                (incf count)
            )
        )
        (if (string= "if" str)
            (progn
                (print str)
                (write-line "KW_IF")
                (incf count)
            )
        )
        (if (string= "exit" str)
            (progn
                (print str)
                (write-line "KW_EXIT")
                (incf count)
            )
        )
        (if (string= "load" str)
            (progn
                (print str)
                (write-line "KW_LOAD")
                (incf count)
            )
        )
        (if (string= "display" str)
            (progn
                (print str)
                (write-line "KW_DISPLAY")
                (incf count)
            )
        )
        (if (string= "true" str)
            (progn
                (print str)
                (write-line "KW_TRUE")
                (incf count)
            )
        )
        (if (string= "false" str)
            (progn
                (print str)
                (write-line "KW_FALSE")
                (incf count)
            )
        )
        (if (< count 1)
            nil
            T
        )
    )
)

;operator kontrollerinin hepsini burada yaptım.
(defun operator-control (str)
    (if (string= "+" str)
            (progn
                (print str)
                (write-line "OP_PLUS")
            )
    )
    (if (string= "-" str)
            (progn
                (print str)
                (write-line "OP_MINUS")
            )
    )
    (if (string= "/" str)
            (progn
                (print str)
                (write-line "OP_DIV")
            )
    )
    (if (string= "*" str)
            (progn
                (print str)
                (write-line "OP_MULT")
            )
    )
    (if (string= "," str)
            (progn
                (print str)
                (write-line "OP_COMMA")
            )
    )
)

;yorum için kontrol fonksiyonu(kullanıp kullanmadığna en son tekrar bak.)
(defun control-comment (str)
    (if (string= ";;" str)
            (progn
                (print str)
                (write-line "OP_COMMA")
            )
    )
)


;identifier kontrolünü burada yaptım
(defun identifier-control (str)
    (if (is-first-character-letter str)
        (if (are-all-alphanumeric str)
            (progn
                (print str)
                (write-line "IDENTIFIER")
            )
        )
    )
    (if (is-first-character-underscore str)
        (if (are-all-alphanumeric str)
            (progn
                (print str)
                (write-line "IDENTIFIER")
            )
        )
    )
)

;str stringinin ilk karakteri harf mi değil mi -> alpha-char-p fonksiyonundan faydalandım.
(defun is-first-character-letter (str)
    (and (> (length str) 0)
        (alpha-char-p (char str 0))
    )
)

;str stringinin ilk karakteri sayı mi değil mi -> digit-char-p fonksiyonundan faydalandım.
(defun is-first-character-digit (str)
    (and (> (length str) 0)
        (digit-char-p (char str 0))
    )
)

;stringin ilk karakteri alt tre mi kontrolü yapar.
(defun is-first-character-underscore (str)
    (and (> (length str) 0)
        (char= (char str 0) #\_)
    )
)

;tüm elemanlar sayı, alt tre ve harf mi kontrolünü yaptım.
(defun are-all-alphanumeric (str)
    (if (stringp str)
        (every #'(lambda (char) ;"every"-> her karakterin alpha-char-p veya digit-char-p işlemleri tarafından kontrol edilmesini sağlar.
                 (or (alpha-char-p char)
                     (digit-char-p char)
                     (char= char #\_)))
             str)
        nil
    )
)

;tüm elemanlar sayı mı kontrolü yaptım.
(defun are-all-digits (str)
    (if (stringp str)
        (every #'digit-char-p str)
        nil
    )
)

;sayı ile başlıyor ancak içerisinde sadece sayı ve b harfi yok ise o zaman hata verdirtmeliyiz.
(defun error-valuef-control (str)
    (if (is-first-character-digit str)
        (unless (helper-valuef str)
            (progn
                (write-line "Lexical Syntax ERROR. It is not a VALUEF but its first character is digit.")
                ;(quit)
            )
        )
    )
    (if (is-first-character-digit str) ;34b gibi b nin sonda olduğu örneklerde hata vermek için kullandım.
        (if (helper-valuef str)
            (if (is-last-char-b str)
                (progn
                    (write-line "Lexical Syntax ERROR. It is not a VALUEF but its first character is digit.")
                    ;(quit)
                )
            )
        )
    )
)


(defun valuef-control (str)
    (if (> (length str) 2)
        (unless (string= (subseq str 0 1) "b") ;kelimenin en başına bakar.
            (unless (is-last-char-b str) ;kelimenin sonu b mi diye bakar
                (if (helper-valuef str)
                    (are-there-multiple-b str);Bu fonksiyon içinde VALUEF yazdırdım.
                )
            )
        )
    )
    (if (> (length str) 2) ;fonksiyonun sonrası parantezleri kontrol etmeye başladığım için kullanılmamaya başlandı ancak işlevsel olduğundan kodumdan çıkartmadım.
        (unless (string= (subseq str 0 1) "b") ;kelimenin en başına bakar.
            (unless (is-last-char-b str) ;kelimenin sonu b mi diye bakar
                (if (is-last-char-parenthesis str) ;eğer kelimenin sonunda parantez varsa yine de valuef olarak değerlendirdim.
                    (are-there-multiple-b str);Bu fonksiyon içinde VALUEF yazdırdım.
                )
            )
        )
    )
)

;verilen stringin tamamen b harfi ve sayılardan oluşup oluşmadığına bakar.
(defun helper-valuef (str)
    (if (stringp str)
        (every #'(lambda (char)
                    (or (digit-char-p char) ;sayı içermesi kontrolü
                        (char= char #\b))) ;b harfini içermesi kontrolü
                            str)
        nil
    )
)


;birden fazla b harfi var mı stringde kontrol yapar.
(defun are-there-multiple-b (str)
    (unless (string= str "")
        (let ((b-count 0))
            (loop for char across str
                do
                (if (char= #\b char)
                    (incf b-count) ;b -count değişkeninin değerini bir arttır.
                )
            )
            (if (< b-count 2) ;eğer string içinde 1 den fazla b harfi varsa VALUEF yazdırıyorum.
                (if (> b-count 0)
                    (progn
                        (print str)
                        (write-line "VALUEF")
                    )
                )
            )
            nil
        )
    )
)

;noktalı virgül var mı yok mu kontrolü yapar.
(defun has-semicolon-p (str)
    (loop for letter across str
    do
        (if (char= #\; letter)
            (return t)
        )
    )
)

;verilen stringde türkçe karakter var mı diye kontrol eder
(defun turkish-characters-p (str)
    (let ((turkish-chars "çğıöşüÇĞİÖŞÜ"))
        (loop for char across str
        when (find char turkish-chars)
            do (return t)
            finally (return nil)
        )
    )
)


;bir stringin içinde bir yerde parantez var mı diye kontrol ettim.
(defun has-parentheses-inside (str)
  (let ((start 0)
        (end (1- (length str))))
    (loop for i from start below (1+ end)
          when (char= (char str i) #\()
          do (return t) ; Parantez bulundu
          when (char= (char str i) #\))
          do (return t) ; Parantez bulundu
          finally (return nil)) ; Parantez bulunamadı
    )
)

(defun has-plus-inside (str)
  (let ((start 0)
        (end (1- (length str))))
    (loop for i from start below (1+ end)
          when (char= (char str i) #\+)
          do (return t) ; Parantez bulundu
          finally (return nil)) ; Parantez bulunamadı
    )
)

(defun has-minus-inside (str)
  (let ((start 0)
        (end (1- (length str))))
    (loop for i from start below (1+ end)
          when (char= (char str i) #\-)
          do (return t) ; Parantez bulundu
          finally (return nil)) ; Parantez bulunamadı
    )
)

(defun has-mult-inside (str)
  (let ((start 0)
        (end (1- (length str))))
    (loop for i from start below (1+ end)
          when (char= (char str i) #\*)
          do (return t) ; Parantez bulundu
          finally (return nil)) ; Parantez bulunamadı
    )
)

(defun has-over-inside (str)
  (let ((start 0)
        (end (1- (length str))))
    (loop for i from start below (1+ end)
          when (char= (char str i) #\/)
          do (return t) ; Parantez bulundu
          finally (return nil)) ; Parantez bulunamadı
    )
)

(defun has-comma-inside (str)
  (let ((start 0)
        (end (1- (length str))))
    (loop for i from start below (1+ end)
          when (char= (char str i) #\,)
          do (return t) ; Parantez bulundu
          finally (return nil)) ; Parantez bulunamadı
    )
)


;verilen stringi paranteze göre parçalara böler ve sonuçları bir liste ile dönderir.
;Bu fonksiyonda devamlı olarak biz verilen stringin içindeki karakterlere bakıyoruz ve parantez olmadığı sürece karakterleri birleştiriyoruz, bir paratez ile karşılaştığımızda parantezden öncesini list e kaydediyoruz ardından karşılaştığımız parantezi list e kaydediyoruz ve stringin kalan karakterlerini kontrol etmeye devam ediyoruz.
(defun split-string-by-parentheses (str)
  (let ((result '()) ; Boş bir liste oluştururuz, böylece parantez içeriğini saklayabiliriz.
        (current "") ; Şu anki karakterleri bir arada tutmak için boş bir dize oluştururuz.
        (depth 0)) ; Parantez içeriği seviyesini izlemek için bir derinlik sayacı tanımlarız.
        (loop for char across str ; str yi karakter karakter döngüye alırız.
              do (if (char= char #\() ;içinde parantez var mı diye kontrol ediyoruz
                     (progn
                       (if (/= depth 0) ; Eğer daha önceden karakterler toplandıysa, sonuca ekleriz.
                           (push current result))
                       (setq depth (1+ depth)) ; Parantez içeriği seviyesini artırırız.
                       (push "(" result) ; "(" karakterini sonuca ekleriz.
                       (setq current "")) ; Şu anki karakterleri temizleriz.
                   (if (char= char #\)) ;içinde parantez var mı diye kontrol ediyoruz
                       (progn
                         (setq depth (1- depth))
                         (push current result) ; Şu ana kadar topladığımız karakterleri sonuca ekleriz.
                         (push ")" result) ; ")" karakterini sonuca ekleriz.
                         (setq current "")) ; Şu anki karakterleri temizleriz.
                       (setq current (concatenate 'string current (string char))))) ; Diğer karakterleri şu anki karaktere ekleriz.
              finally (push current result) ; Döngü sonunda, şu andaki karakteri sonuca ekleriz.
        )
        (nreverse result) ; Sonucu ters çeviririz, çünkü karakterleri baştan itibaren ekledik ve bu yüzden ters sıralanmışlardı.
    )
)

(defun is-length-bigger-than-one (str)
    (if (> (length str) 1)
        T
        nil
    )
)

(defun general-operator-control (str)
    (or(has-plus-inside str)
      (has-minus-inside str)
      (has-mult-inside str)
      (has-over-inside str)
      (has-comma-inside str))
)

;bir liste alır ve tüm operasyonları içinde yapar.
(defun whole-operations (words)
    (let (paranthesList (list)) ;bir word içinde parantez varsa parçalanan halini list olarak tutar.
        (loop for word in words ;splint_string fonksiyonunun dönderdiği list e eşitlendi. içinde birçok string ifade bulundurur.
        do
            (if (has-parentheses-inside word)
                (progn ;içinde parantez olan stringler için kontrol mekanizması başlangıcı.
                    (setq paranthesList (split-string-by-parentheses word))
                    (whole-control-with-parenthes paranthesList)
                ) ;progn bitiş
                (progn ;içinde parantez içeremeyecek olan stringler için kontrol mekanizması başlangıcı
                    (if (> (length word) 1)
                        (if (string= (subseq word 0 2) ";;") ;ayrılmış kelimelerin en başına bakar.
                            (progn
                                (print ";;")
                                (write-line "COMMENT")
                                (return)
                            )
                        )
                    )
                    (if (string= word "-exit-")
                        (progn
                            (setq exitControl1 1)
                            (return)
                        )
                    )
                    (if (turkish-characters-p word)
                        (progn
                            (print word)
                            (write-line "Error! Cannot include any turkish letters in this lexical syntax..!")
                            ;(quit)
                        )
                    )
                    (if (has-semicolon-p word)
                        (progn
                            (print ";")
                            (write-line "Error! Cannot include only one semicolon..!")
                            ;(quit)
                        )
                    )
                    (if (general-operator-control word)
                        (if (is-length-bigger-than-one word)
                            (progn
                                (print word)
                                (write-line "Error! Operators cannot be adjacent nothing, must be space..!")
                            )
                        )
                    )
                    (word-control word)
                );progn bitiş
            )
        )
    )
)

;içinde parantez içeren list parametre olarak verilir ve tüm akışı kontrol eder.
(defun whole-control-with-parenthes (words)
    (loop for word in words ;splint_string fonksiyonunun dönderdiği list e eşitlendi. içinde birçok string ifade bulundurur.
    do
        (if (> (length word) 1)
            (if (string= (subseq word 0 2) ";;") ;ayrılmış kelimelerin en başına bakar.
                (progn
                    (print ";;")
                    (write-line "COMMENT")
                    (return)
                )
            )
        )
        (if (turkish-characters-p word)
            (progn
                (write-line "Error! Cannot include any turkish letters in this lexical syntax..!")
                (return)
            )
        )
        (if (has-semicolon-p word)
            (progn
                (print ";")
                (write-line "Error! Cannot include only one semicolon..!")
                ;(quit)
            )
        )
        (if (string= word "(")
            (progn
                (print "(")
                (write-line "OP_OP")
            )
        )
        (if (string= word ")")
            (progn
                (print ")")
                (write-line "OP_CP")
            )
        )
        (if (general-operator-control word)
            (if (is-length-bigger-than-one word)
                (progn
                    (print word)
                    (write-line "Error! Operators cannot be adjacent nothing, must be space..!")
                )
            )
        )
        (word-control word)
    )
)




(write-line "--------------------------------------------------")
(write-line "")
(write-line "Welcome to Lexical Analysis")
(write-line "")
(write-line "--------------------------------------------------")
(write-line "")
(write-line "")
(write-line "Enter your line for analysis: ")

(gppinterpreter) ;alttaki satırın yorumunu açınca bu satırı yoruma alınız
;(gppinterpreter "example.txt") ;Dosyadan okuma yapmak için yorumdan çıkarınız


