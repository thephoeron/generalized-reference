# GENERALIZED-REFERENCE

[![Quicklisp](http://quickdocs.org/badge/generalized-reference.svg)](http://quickdocs.org/generalized-reference/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

Generalized reference over structured data by pairwise reduction of arbitrary
place identifiers for Common Lisp.

## About

The interface is simple and minimalist by design, primarily intended for
streamlined work with hierarchical data, such as that imported automatically
from another language or format that may not follow Lisp style conventions and
best-practices. Support is also included for the SERIES library because I happen
to use it a lot.

Inspired by, and an alternative to, the [ACCESS][] library, and [SERAPEUM][]'s
`vref` and `href` forms.

[ACCESS]: https://github.com/sharplispers/access
[SERAPEUM]: https://github.com/ruricolist/serapeum

## Usage

Reference a place by the sequence of place identifiers in desending order, using
the `ref` function or the `$` convenience macro. Referenced places are typically
SETF-able where it makes sense for them to be so.

Support is included for the following types:

- arbitrary place names identified by symbol or string
- symbol values, lexical and dynamic, by package
- symbol p-list values
- pathname components to navigate directories and files like any other structured data
- CLOS objects and slot values, with or without accessor functions
- structs and struct slots by key
- hash-tables by key
- p-lists by key
- a-lists by key
- proper lists by index
- vectors by index
- arrays by indices
- series objects by selection, index, range, or function-key

To extend for your custom data types, specialize the generic function `%ref`.

## Examples

Download the software to a place ASDF can find it, such as `#P"~/common-lisp"`.

Then in your favourite REPL:

```lisp
(ql:quickload :generalized-reference)

(use-package :generalized-reference)
```

### Association Lists

Association Lists allow lookups by quoted-symbol or name-string. Note that the
name-string is case-sensitive.

Keyword-symbol lookup for Association Lists is not presently supported, but may
be in a future update.

```lisp
(defparameter *alist* '((a . 1) (b . 2) (c . 3)))

($ *alist* 'a)
=> 1

($ *alist* "A")
=> 1

($ *alist* :a)
=> NIL

($ *alist* "a")
=> NIL
```

### Property Lists

Property Lists allow lookups by keyword-symbol, quoted-symbol, or name-string.
Note that the name-string is case-sensitive.

```lisp
(defparameter *plist* '(:a 1 :b 2 :c 3))

($ *plist* :a)
=> 1

($ *plist* 'a)
=> 1

($ *plist* "A")
=> 1

($ *plist* "a")
=> NIL
```

### Hash Tables

Hash Tables currently support lookup by their internal hash-key. To allow lookup
for string-based hash-keys, remember to set the equality test to `equal` when
defining the hash table.

When a hash-key isn't found in a hash table, `ref` returns the keyword symbol
`:NOT-FOUND`, while returning `NIL` means the hash-key is present in the table
but has its value set to `NIL`.

Note: For this example we're also going to use the `DICT` constructor function
from the Serapeum library, a dependency of generalized-reference already
available in your Lisp Image. By default it uses `equal` comparison for hash-key
lookups, which is exactly what we need.

```lisp
(use-package :serapeum)

(defparameter *hash* (dict :a 1 :b 2 :c 3 "D" 4))

($ *hash* :a)
=> 1

($ *hash* "D")
=> 4

($ *hash* :d)
=> :NOT-FOUND
```

### Vectors

Vectors allow lookup by index.

```lisp
(defparameter *vector* (vector 1 2 3 4))

($ *vector* 0)
=> 1
```

### Arrays

Arrays allow lookup by a list of indices.

```lisp
(defparameter *array* (make-array '(2 2) :initial-contents '((1 2) (3 4))))

($ *array* '(0 0))
=> 1

($ *array* '(1 1))
=> 4
```

## Support

This library has been built and tested successfully on macOS using the following
Lisp implementations:

- LispWorks 8.0.0 (64-bit)
- SBCL 2.2.4
- Clozure-CL 1.12.1
- GNU CLISP 2.49.92
- ECL 21.2.1

And builds with warnings on:

- ABCL 1.9.0

## License

Copyright &copy; 2022, "the Phoeron" Colin J.E. Lupton

Released under the MIT License. See [generalized-reference/LICENSE](./LICENSE)
for more information.
