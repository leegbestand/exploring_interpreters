# Revision history for exploring-interpreters

## 0.2.0.0 -- 2021-03-15
* First official version.

## 0.3.0.0 -- 2021-03-16
* Require that definitional interpreters return configurations in the Maybe monad.
  This adds support for run-time errors by returning Nothing when an errors occurs.

## 0.3.1.0 -- 2021-04-18
* This version adds the 'leaves' function to the exploring interpreter.

## 0.3.2.0 -- 2021-06-29
* This version adds functionality to support exporting and importing of execution environments.
This functionality is provided via the 'toExport' and 'fromExport' functions.
* Furthermore, this version also exports the initial reference.

## 0.3.2.1 -- 2021-01-10
* Add the toExport and fromExport functions to the exported functions in the pure module.

## 0.4.0.0 -- 2021-01-10
* fromExport function now correctly determines the referece to use for generation of new nodes.