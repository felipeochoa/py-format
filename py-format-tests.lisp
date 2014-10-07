
(defpackage :py-format-tests
  (:use :common-lisp :lisp-unit :py-format))

(in-package #:py-format-tests)

(define-test py-format
  (assert-equal "ABC" (py-format:py-format :abc ""))
  (assert-equal "1" (py-format:py-format 1 ""))
  (assert-equal "NIL" (py-format:py-format nil ""))
  (assert-equal "(1 2 3)" (py-format:py-format '(1 2 3) ""))
  (assert-equal "string" (py-format:py-format "string" "")))

(define-test py-str
  (assert-equal "ABC" (py-format:py-str 'abc))
  (assert-equal "1" (py-format:py-str 1))
  (assert-equal "NIL" (py-format:py-str nil))
  (assert-equal "(1 2 3)" (py-format:py-str '(1 2 3)))
  (assert-equal "string" (py-format:py-str "string")))

(define-test py-repr
  (assert-equal ":ABC" (py-format:py-repr :abc))
  (assert-equal "1" (py-format:py-repr 1))
  (assert-equal "NIL" (py-format:py-repr nil))
  (assert-equal "(1 2 3)" (py-format:py-repr '(1 2 3)))
  (assert-equal "\"string\"" (py-format:py-repr "string")))

(define-test py-getitem
  (let ((string-to-square (make-hash-table :test #'equal)))
    (loop for i from 1 to 10
       do (setf (gethash (write-to-string i) string-to-square) (* i i)))
    (assert-equal 1 (py-format:py-getitem string-to-square "1"))
    (assert-equal 25 (py-format:py-getitem string-to-square "5"))
    (assert-equal 100 (py-format:py-getitem string-to-square "10"))
    (assert-error 'key-error (py-format:py-getitem string-to-square 2))
    (assert-error 'key-error (py-format:py-getitem string-to-square "12"))))

(define-test py-formatter-convert-field
  (assert-equal "ABC" (py-format::py-formatter-convert-field :abc #\s))
  (assert-equal "1" (py-format::py-formatter-convert-field 1 #\s))
  (assert-equal "NIL" (py-format::py-formatter-convert-field nil #\s))
  (assert-equal "(1 2 3)" (py-format::py-formatter-convert-field '(1 2 3) #\s))
  (assert-equal "string" (py-format::py-formatter-convert-field "string" #\s))

  (assert-equal ":ABC" (py-format::py-formatter-convert-field :abc #\r))
  (assert-equal "1" (py-format::py-formatter-convert-field 1 #\r))
  (assert-equal "NIL" (py-format::py-formatter-convert-field nil #\r))
  (assert-equal "(1 2 3)" (py-format::py-formatter-convert-field '(1 2 3) #\r))
  (assert-equal "\"string\"" (py-format::py-formatter-convert-field "string" #\r))

  (assert-equal 'abc (py-format::py-formatter-convert-field 'abc nil))
  (assert-equal 1 (py-format::py-formatter-convert-field 1 nil))
  (assert-equal nil (py-format::py-formatter-convert-field nil nil))
  (assert-equal '(1 2 3) (py-format::py-formatter-convert-field '(1 2 3) nil))
  (assert-equal "string" (py-format::py-formatter-convert-field "string" nil))

  (assert-error 'type-error (py-format::py-formatter-convert-field 'abc #\o)))

(define-test py-formatter-parse-field
  (assert-equal '("0" "" nil) (py-format::py-formatter-parse-field "0"))
  (assert-equal '("0" "" #\s) (py-format::py-formatter-parse-field "0!s"))
  (assert-equal '("0" "d" #\s) (py-format::py-formatter-parse-field "0!s:d"))
  (assert-equal '("0" "d" nil) (py-format::py-formatter-parse-field "0:d"))
  (assert-equal '("" "d" #\s) (py-format::py-formatter-parse-field "!s:d"))
  (assert-equal '("" "" #\s) (py-format::py-formatter-parse-field "!s"))
  (assert-equal '("" "d" nil) (py-format::py-formatter-parse-field ":d"))
  (assert-equal '("0.attr[attr2]" "" nil) (py-format::py-formatter-parse-field "0.attr[attr2]"))
  (assert-equal '("0.attr[attr2]" "d" nil) (py-format::py-formatter-parse-field "0.attr[attr2]:d"))
  (assert-equal '("0.attr[attr2]" "d" #\s) (py-format::py-formatter-parse-field "0.attr[attr2]!s:d"))
  (assert-equal '("0.attr[attr2]" "" #\s) (py-format::py-formatter-parse-field "0.attr[attr2]!s")))


(define-test py-formatter-parse-format-string
  (assert-equal '(( "text" nil nil nil)) (py-format::py-formatter-parse-format-string "text"))
  (assert-equal '(("text" "" "" nil)) (py-format::py-formatter-parse-format-string "text{}"))
  (assert-equal '(("text" "0" "" nil)) (py-format::py-formatter-parse-format-string "text{0}"))
  (assert-equal '(("text" "0" "" #\s)) (py-format::py-formatter-parse-format-string "text{0!s}"))
  (assert-equal '(("text" "0" "d" #\s)) (py-format::py-formatter-parse-format-string "text{0!s:d}"))
  (assert-equal '(("text" "" "d" #\s)) (py-format::py-formatter-parse-format-string "text{!s:d}"))
  (assert-equal '(("text" "" "d" nil)) (py-format::py-formatter-parse-format-string "text{:d}"))
  (assert-equal '(("text" "" "" #\s)) (py-format::py-formatter-parse-format-string "text{!s}"))

  (assert-equal nil (py-format::py-formatter-parse-format-string ""))
  (assert-equal '(("" "" "" nil)) (py-format::py-formatter-parse-format-string "{}"))
  (assert-equal '(("" "0" "" nil)) (py-format::py-formatter-parse-format-string "{0}"))
  (assert-equal '(("" "0" "" #\s)) (py-format::py-formatter-parse-format-string "{0!s}"))
  (assert-equal '(("" "0" "d" #\s)) (py-format::py-formatter-parse-format-string "{0!s:d}"))
  (assert-equal '(("" "" "d" #\s)) (py-format::py-formatter-parse-format-string "{!s:d}"))
  (assert-equal '(("" "" "d" nil)) (py-format::py-formatter-parse-format-string "{:d}"))
  (assert-equal '(("" "" "" #\s)) (py-format::py-formatter-parse-format-string "{!s}"))

  (assert-error 'simple-error (py-format::py-formatter-parse-format-string "asdf}basdf"))
  (assert-error 'simple-error (py-format::py-formatter-parse-format-string "asdf}"))
  (assert-error 'simple-error (py-format::py-formatter-parse-format-string "{}asdf}"))
  (assert-error 'simple-error (py-format::py-formatter-parse-format-string "{0!s:d}asdf}"))
  (assert-error 'simple-error (py-format::py-formatter-parse-format-string "}---{0!s:d}"))

  (assert-error 'simple-error (py-format::py-formatter-parse-format-string "asdf{basdf"))
  (assert-error 'simple-error (py-format::py-formatter-parse-format-string "asdf{"))
  (assert-error 'simple-error (py-format::py-formatter-parse-format-string "{}asdf{"))
  (assert-error 'simple-error (py-format::py-formatter-parse-format-string "{0!s:d}asdf{"))
  (assert-error 'simple-error (py-format::py-formatter-parse-format-string "{---0!s:d{")))

(define-test py-formatter-field-name-split
  (assert-equal '((t . "0")) (py-format::py-formatter-field-name-split "0"))
  (assert-equal '((t . "0") (t . "shrubbery")) (py-format::py-formatter-field-name-split "0.shrubbery"))
  (assert-equal '((t . "123") (t . "modest")) (py-format::py-formatter-field-name-split "123.modest"))
  (assert-equal '((t . "123") (nil . "modest")) (py-format::py-formatter-field-name-split "123[modest]"))
  (assert-equal '((t . "0") (nil . "modest") (t . "ni"))
                (py-format::py-formatter-field-name-split "0[modest].ni"))
  (assert-equal '((t . "0") (t . "modest") (nil . "ni"))
                (py-format::py-formatter-field-name-split "0.modest[ni]"))
  (assert-equal '((t . "0") (nil . "modest") (t . "ni") (nil . "ni"))
                (py-format::py-formatter-field-name-split "0[modest].ni[ni]"))
  (assert-equal '((t . "0") (t . "modest") (nil . "ni") (t . "ni"))
                (py-format::py-formatter-field-name-split "0.modest[ni].ni"))
  ; The next two are weird examples, but Python does it this way
  (assert-equal '((t . "0") (nil . "abc["))
                (py-format::py-formatter-field-name-split "0[abc[]"))
  (assert-equal '((t . "0") (nil . "abc."))
                (py-format::py-formatter-field-name-split "0[abc.]"))

  (assert-error 'simple-error (py-format::py-formatter-field-name-split "0.abc]"))
  (assert-error 'simple-error (py-format::py-formatter-field-name-split "0abc]"))
  (assert-error 'simple-error (py-format::py-formatter-field-name-split "0[abc."))
  (assert-error 'simple-error (py-format::py-formatter-field-name-split "0[abc")))

(define-test py-formatter-get-field
  (assert-equal #\a (py-format::py-formatter-get-field "0" "abc"))
  (assert-equal #\b (py-format::py-formatter-get-field "1" "abc"))
  (assert-equal "b" (py-format::py-formatter-get-field "1" '("a" "b" "c")))
  (assert-equal 3 (py-format::py-formatter-get-field "2" '(1 2 3)))
  (assert-equal 2 (py-format::py-formatter-get-field "0[1]" '((1 2 3) "abc")))
  (assert-equal #\c (py-format::py-formatter-get-field "1[2]" '((1 2 3) "abc")))
  (let ((temp-class (ccl::ensure-class-for-defclass
                     'temp-class
                     :direct-superclasses 'nil
                     :direct-slots '((:name temp-slot :initargs ':temp-slot)))))
    (assert-equal "c" (py-format::py-formatter-get-field
                       "0.temp-slot"
                       (list (make-instance temp-class :temp-slot "c")))))
  (let ((temp-hash (make-hash-table :test 'equal)))
    (setf (gethash "coconut" temp-hash) 2)
    (assert-equal 2 (py-format::py-formatter-get-field "0[coconut]" (list temp-hash)))
    (assert-error 'key-error (py-format::py-formatter-get-field "0[other]" (list temp-hash)))
    (setf (gethash "1" temp-hash) "string")
    (setf (gethash 1 temp-hash) "int")
    (assert-equal "int" (py-format::py-formatter-get-field "0[1]" (list temp-hash)))
    (setf (gethash "1.2" temp-hash) "string")
    (setf (gethash 1.2 temp-hash) "float")
    (assert-equal "string" (py-format::py-formatter-get-field "0[1.2]" (list temp-hash)))))

(define-test py-vformat
  (assert-equal "foo" (py-format::py-vformat "foo" () 2))
  (assert-equal "foobar" (py-format::py-vformat "foo{0}" '("bar") 2))
  (assert-equal "foo6bar-6" (py-format::py-vformat "foo{1}{0}-{1}" '("bar" 6) 2))
  (assert-equal "-\"test\"-" (py-format::py-vformat "-{0!r}-" '("test") 2))

  (assert-equal "foo" (py-format::py-vformat "{0:{1}}" '("foo" "s") 2))

  (assert-error 'simple-error (py-format::py-vformat "foo" () -1))
  (assert-error 'simple-error (py-format::py-vformat "{}" '("foo") 0))
  (assert-error 'simple-error (py-format::py-vformat "{0:{1}}" '("foo" "bar") 1))
  (assert-error 'simple-error (py-format::py-vformat "{0:{1:{2}}}" '("foo" "bar" "baz") 2))

  (assert-error 'simple-error (py-format::py-vformat "{}-{1}" '("foo" "bar") 2))
  (assert-error 'simple-error (py-format::py-vformat "{0}-{}" '("foo" "bar") 2)))

(define-test py-str-format
  (assert-equal "foo" (py-str-format "foo" ()))
  (assert-equal "foobar" (py-str-format "foo{0}" "bar"))
  (assert-equal "foo6bar-6" (py-str-format "foo{1}{0}-{1}" "bar" 6))
  (assert-equal "-\"test\"-" (py-str-format "-{0!r}-" "test"))

  (assert-equal "foo" (py-str-format "{0:{1}}" "foo" "s"))

  (assert-error 'simple-error (py-str-format "{0:{1:{2}}}" "foo" "bar" "baz"))
  (assert-error 'simple-error (py-str-format "{}-{1}" "foo" "bar"))
  (assert-error 'simple-error (py-str-format "{0}-{}" "foo" "bar")))
