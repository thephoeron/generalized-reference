# GENERALIZED-REFERENCE

Generalized reference over structured data by pairwise reduction of arbitrary
place identifiers for Common Lisp.

## About

The interface is simple and minimalist by design, primarily intended for
streamlined work with hierarchical data, such as that imported automatically
from another language or format that may not follow Lisp style conventions and
best-practices. Support is also included for the SERIES library because I happen
to use it a lot.

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

To extend for your custom data types, specialize the generic function `%ref`

## License

Copyright &copy; 2022, "the Phoeron" Colin J.E. Lupton

Released under the MIT License. See [generalized-reference/LICENSE.md](LICENSE.md)
for more information.
