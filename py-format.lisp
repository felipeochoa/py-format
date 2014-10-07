
(in-package :py-format)

(defmethod py-format (obj (format-spec string))
  (py-str obj))

(defmethod py-str (obj)
  (format nil "~A" obj))

(defmethod py-repr (obj)
  (format nil "~S" obj))

(defmethod py-getitem (obj key)
  (elt obj key))

(define-condition key-error (error)
  ((key :initarg :key))
  (:report (lambda (condition stream) (format stream "Key error: ~S" (slot-value condition 'key)))))

(defmethod py-getitem ((obj hash-table) key)
  (multiple-value-bind (ret foundp) (gethash key obj)
    (if foundp ret (signal 'key-error :key key))))

(defun py-formatter-convert-field (obj conversion)
  "Conversion must be one of 's' or 'r', equivalent to calling str(obj) or repr(obj)"
  (if conversion
      (ecase conversion
        (#\s (py-str obj))
        (#\r (py-repr obj)))
      obj))

(defun py-formatter-parse-field (field-string)
  "Parses the interior of a format string field (i.e., the part between curly-braces).
Returns a list (field-name format-spec conversion)"
  (declare (type string field-string))
  ; <field>[!conversion][:<field-spec>]
  ;        ^ field-end    ^ field-spec-start
  (let* ((conversion nil)
        (field-len (length field-string))
        (format-spec-start field-len))
    (declare (type integer field-len format-spec-start))
    (if (eql field-len 0) '("" "" nil)
    (destructuring-bind (field-end c)
        (loop
           for i from 1
           for c across field-string
           when (or (eql c #\:) (eql c #\!) (= i field-len))
           return (list i c))
      (declare (type integer field-end))
      (declare (type character c))
      (when (or (eql c #\:) (eql c #\!))
        (setq format-spec-start field-end)
        (decf field-end)
        (when (eql c #\!)
          (when (>= format-spec-start field-len)
            (error "End of format while looking for conversion specifier"))
          (setf conversion (char field-string format-spec-start))
          (incf format-spec-start)
          (when (< format-spec-start field-len)
            (unless (eql (char field-string format-spec-start) #\:)
              (error "Expected ':' after format specifier"))
            (incf format-spec-start))))
      (list (subseq field-string 0 field-end)
            (subseq field-string format-spec-start field-len)
                conversion)))))

(defun py-formatter-parse-format-string (format-string)
  "Generates a list with items of the form (literal-text field-name format-spec conversion).
Where literal-text is text to print literally, field-name is the reference to the field,
format-spec is the formatting string to use for the field, and conversion is one of 's' or 'r'"
  (declare (type string format-string))
  (loop
     for base-ptr from 0
     for markup-follows = nil
     with fmt-len = (the integer (length format-string))
     while (< base-ptr fmt-len)
     collect
       (let* ((ptr (1+ (loop
                          for i from base-ptr to (1- fmt-len)
                          for c = (the character (char format-string i))
                          when (or (eql c #\{) (eql c #\}))
                          do (setf markup-follows t)
                          and return i
                          maximize i)))
              (at-end (>= ptr fmt-len))
              (literal-end ptr)
              (c (the character (char format-string (1- ptr)))))
         (when (and (eql c #\}) (or at-end (not (eql c (char format-string ptr)))))
           (error "Single '}' encountered in format string"))
         (when (and at-end (eql c #\{))
           (error "Single '{' encountered in format string"))
         (if (not at-end)
             (if (eql c (char format-string ptr))
                 (progn (incf ptr) (setf markup-follows nil))
                 (decf literal-end)))
         (cons (subseq format-string base-ptr literal-end)
               (if markup-follows
                   (loop
                      for i from ptr to (1- fmt-len)
                      for c = (the character (char format-string i))
                      with count = 1
                      when (eql #\{ c) do (incf count)
                      when (and (eql #\} c) (<= (decf count) 0))
                      do (setf base-ptr i)
                      and return (py-formatter-parse-field (or (subseq format-string ptr i) ""))
                      finally (error "unmatched '{' in format"))
                   (progn (setf base-ptr literal-end) '(nil nil nil)))))))

(defun py-formatter-field-name-split (field-name)
  "Given a field name, parse it into getattrs and getitems."
  (declare (type string field-name))
  ; abcd.efg[hijk].lmno
  ; ^    ^   ^     ^
  (maplist #'(lambda (lst)
               (let ((first (car lst))
                     (second (second lst)))
                 (when (not second) (setf second (cons (1+ (length field-name)) nil)))
                 (cons (not (cdr first))
                       (subseq
                        field-name
                        (car first)
                        (- (car second) (if (cdr first) 2 1))))))
           (cons '(0 . nil)
                 (loop
                    for c across field-name
                    for i from 0
                    for delim-c = (or (eql c #\[) (eql c #\.))
                    with in-brackets = nil
                    when (eql c #\])
                    do (if in-brackets (setf in-brackets nil) (error "Unmatched ']' in format string"))
                    when (and in-brackets delim-c)
                    do (error "Missing ']' in format string")
                    when delim-c
                    collect (cons (1+ i) (setf in-brackets (eql c #\[)))))))

(defun py-formatter-get-field (field-name args)
  "Given a field name, find the object it references"
  (let* ((fields (py-formatter-field-name-split field-name))
         (first (car fields))
         (rest (cdr fields))
         (obj (elt args (parse-integer (cdr first)))))
    (loop
       for (is-attr . next) in rest
       do (setf obj (if is-attr
                        (slot-value obj (intern (string-upcase next)))
                        (py-getitem
                         obj
                         (handler-case (parse-integer next)
                           ('parse-integer-not-integer-string () next))))))
    obj))

(defun py-vformat (format-string args recursion-depth)
  (declare (type fixnum recursion-depth))
  (when (< recursion-depth 0) (error "Max string formatting recursion exceeded"))
  (apply #'concatenate
         (cons 'string
               (loop
                  with manual-numbering = nil
                  with auto-numbering = nil
                  for (literal-text field-name format-spec conversion)
                  in (py-formatter-parse-format-string format-string)
                  for field-number from 0
                  when literal-text collect literal-text
                  when field-name
                  collect (py-format
                           (py-formatter-convert-field
                            (py-formatter-get-field
                             (if (string= field-name "") (write-to-string field-number) field-name)
                             args)
                            conversion)
                           (py-vformat format-spec args (the fixnum (1- recursion-depth))))
                  and do (progn
                           (if (string= field-name "")
                               (if manual-numbering
                                   (error "Cannot switch from manual field specification to automatic field numbering")
                                   (setf auto-numbering t))
                               (if auto-numbering
                                   (error "Cannot switch from automatic field numbering to manual field specification")
                                   (setf manual-numbering t))))))))

(defun py-str-format (format-string &rest args)
  "Formats a python-style format string"
  (py-vformat format-string args 2))
