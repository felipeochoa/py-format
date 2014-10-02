py-format
=========

Common Lisp implementation of Python's
[PEP 3101](http://legacy.python.org/dev/peps/pep-3101/) formatting
system.

Sample use:

```lisp
(load "py-format-package.lisp") ; has defpackage
(load "py-format.lisp")
(let ((temp '(HELLO WORLD)))
     (py-format:py-str-format "{0[0]}-{0[1]}" temp))
; => "HELLO-WORLD"
```

This small library aims to replicate the functionality of Python's
built-in `str.format` method. To do so, it exports one function,
`py-str-format`, and three methods, `py-format`, `py-str`, and
`py-repr`, corresponding to Python's `str.format`, `__format__`,
`__str__`, and `__repr__`.

Currently, only positional arguments to `py-str-format` are
supported. This is a feature I would like to add, but since Common
Lisp doesn't (as far as I know) have a great way to mix positional and
keyword arguments, it will require some design choices before doing
so.

## Features

* The format string uses Python-like syntax, where dots (`.`) indicate
  variable access and brackets (`[]`) indicate list indexing or
  hashtable lookups.

* Supports Python's automatic field numbering, so you can do
  `(py-str-format "{}: {}-{}" 'ni 'ni 'ni)`

* The exported methods provide hooks into the formatting mechanism and
  allow extending the format specification language like in Python

* Uses braces `{}` instead of tildes `~` for formatting commands,
  which in my experience appear less frequently and tend to be more
  readable.

* The code contains type declarations in the performance critical
  areas, so compiling with the appropriate optmization declarations
  should yield very speedy operations.


## Usage

### py-str-format [function]

```lisp
(py-str-format format-string &rest args)
```

This function corresponds to Python's `str.format`. It returns a
string. It is called on every object using the format spec specified
on the field.

* `format-string` is a string formatted according to [Python's syntax]
  (https://docs.python.org/3.4/library/string.html#format-string-syntax).
  The format is based strongly on Python's syntax, and basically
  features three operations for argument selection:

  * **Attribute access**, using the dot `.` operator
    (e.g. `obj.field`), uses lisp's `slot-value` to access fields on
    an object. Therefore `"{0.silly-walks}"` would refer to the field
    `silly-walks` on the first object in the argument list
    (`(slot-value (nth 0 args) field)`).

  * **Sequence indexing**, using the bracket `[]` syntax (e.g.
    `obj[index]`), uses lisp's `nth` to return an item from a list.
    Therefore `"{0[2]}"` would refer to the third item in the first
    object in the argument list (`(nth 2 (nth 0 args))`).

  * **Hashtable access**, using the bracket `[]` syntax (e.g.
    `obj[key]`), uses lisp's `gethash` to return the associated value
    from a lisp hashtable. Therefore `"{0[silly-walks]}"` would refer
    to the value stored under `"silly-walks"` under the first object
    in the argument list (`(gethash "silly-walks" (nth 0 args))`).

  These operations can be combined for deeply-nested access. (e.g.
  `0.ministry[of][silly].walks`)

* `args` are the arguments that will be used by the function as
  specified above

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
conversion `s` is specified. (E.g. `"{0!s}")

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
conversion `r` is specified. (E.g. `"{0!r}")

* `obj` is the object to convert to a string representation.

The default implementation just calls `(format t "~S" obj)`


## To-do

* Currently the default implementation of `py-format` ignores the
  format spec. Need to implement the ["Format Specification
  Mini-Language"]
  ([https://docs.python.org/3.4/library/string.html#format-specification-mini-language),
  especially for numeric types
* Create `py-getitem` to properly index lists and vectors and access
  hashtables (equivalent to Python dictionaries)
