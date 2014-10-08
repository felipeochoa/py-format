py-format
=========

Common Lisp implementation of Python's
[PEP 3101](http://legacy.python.org/dev/peps/pep-3101/) formatting
system.

Sample use:

```lisp
(asdf:load-system :py-format)
(let ((temp '(HELLO WORLD)))
     (py-format:py-str-format "{0[0]}-{0[1]}" temp))
; => "HELLO-WORLD"
```

This small library aims to replicate the functionality of Python's
built-in `str.format` method. To do so, it exports one function,
`py-str-format`, and five methods, `py-getattr`, `py-format`,
`py-str`, `py-repr`, and `py-getitem`, corresponding to Python's
`str.format`, `getattr`, `__format__`, `__str__`, `__repr__`, and
`__getitem__`.

Currently, only positional arguments to `py-str-format` are
supported. This is a feature I would like to add, but since Common
Lisp doesn't (as far as I know) have a great way to mix positional and
keyword arguments, it will require some design choices before doing
so.

## Features

* The format string uses Python-like syntax, where dots (`.`) indicate
  field access and brackets (`[]`) indicate list indexing or
  hashtable lookups.

* Supports Python's automatic field numbering, so you can do
  `(py-str-format "{}-{}; {}-{}" 'one 'swallow 'two 'swallows); =>
  "ONE-SWALLOW; TWO-SWALLOWS"`

* The exported methods provide hooks into the formatting mechanism and
  allow extending the format specification language like in Python

* Uses braces `{}` instead of tildes `~` for formatting commands,
  which in my experience appear less frequently and tend to be more
  readable.


## Usage

### [function] py-str-format

```lisp
(py-str-format format-string &rest args)
```

This function corresponds to Python's `str.format`. It returns a
string.

* `format-string` is a string formatted according to [Python's syntax]
  (https://docs.python.org/3.4/library/string.html#format-string-syntax).
  The format is based strongly on Python's syntax, and basically
  features three operations for argument selection:

  * **Attribute access**, using the dot `.` operator
    (e.g. `obj.field`), uses lisp's `slot-value` to access fields on
    an object. Therefore `"{0.silly-walks}"` would refer to the field
    `silly-walks` on the first object in the argument list
    (`(slot-value (nth 0 args) silly-walks)`).

  * **Sequence indexing**, using the bracket `[]` syntax (e.g.
    `obj[index]`), uses lisp's `nth` to return an item from a list.
    Therefore `"{0[2]}"` would refer to the third item in the first
    object in the argument list (`(elt (nth 0 args) 2)`).

  * **Hashtable access**, using the bracket `[]` syntax (e.g.
    `obj[key]`), uses lisp's `gethash` to return the associated value
    from a lisp hashtable. Therefore `"{0[silly-walks]}"` would refer
    to the value stored under `"silly-walks"` under the first object
    in the argument list (`(gethash "silly-walks" (nth 0 args))`).[1]

  These operations can be combined for deeply-nested access. (e.g.
  `0.ministry[of][silly].walks`)

* `args` are the arguments that will be used by the function as
  specified above

### [method] py-getattr

```lisp
(py-getattr obj attr-name)
```

This method corresponds to Python's `getattr` function. It returns an
object.

* `obj` is the object whose slots we wish to access. The default
  method is not specialized to any type of object.
* `attr-name` is specialized to `string` and contains the name of the
  slot to read. The string will match slots by checking the slot
  names' values under `princ`.

### [method] py-format

```lisp
(py-format obj format-spec)
```

This method corresponds to Python's `__format__` magic method, which
is called by [Python's builtin function `format`]
(https://docs.python.org/3.4/library/functions.html#format).  It
returns a string.

* `obj` is the object to format. The default method is not specialized
  to any type of object
* `format-spec` is specialized to `string` and contains the format
  spec to use.

The default implementation calls `(py-str obj)` regardless of the
format string.


### [method] py-str

```lisp
(py-str obj)
```

This method corresponds to Python's `__str__` magic method, which is
called by [Python's builtin function `str`]
(https://docs.python.org/3.4/library/functions.html#func-str). It
returns a string. It is called by `py-str-format` when the field
conversion `s` is specified. (E.g. `"{0!s}"`)

* `obj` is the object to convert to a string.

The default implementation calls `(format t "~A" obj)`


### [method] py-repr

```lisp
(py-repr obj)
```

This method corresponds to Python's `__repr__` magic method, which is
called by [Python's builtin function `repr`]
(https://docs.python.org/3.4/library/functions.html#repr). It
returns a string. It is called by `py-str-format` when the field
conversion `r` is specified. (E.g. `"{0!r}"`)

* `obj` is the object to convert to a string representation.

The default implementation calls `(format t "~S" obj)`

## Footnotes

1. Note: since Python's formatting syntax does not parse quotes
   inside brackets, using numeric arguments leads to ambiguity. E.g.,
   for a hashtable, does `"{0[1]}"` refer to the value stored under
   the integer `1` or the string `"1"`? Python resolves it by
   assuming that everything that looks like an integer is an integer.

## To-do

* Currently the default implementation of `py-format` ignores the
  format spec. Need to implement the ["Format Specification
  Mini-Language"]
  ([https://docs.python.org/3.4/library/string.html#format-specification-mini-language),
  especially for numeric types
