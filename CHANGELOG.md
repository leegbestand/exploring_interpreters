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

## 0.3.2.1 -- 2021-10-01
* Add the toExport and fromExport functions to the exported functions in the pure module.

## 0.4.0.0 -- 2021-10-01
* fromExport function now correctly determines the referece to use for generation of new nodes.

## 1.0.0.0 -- 2021-10-06
* Change explorer model to the new model where the exploration is always reported
by a tree and sharing is possible via the optional shadow graph.
Furthermore, the *jump* action is introduced to allow jumping to any node in the tree without the 
destructive property. In addition, the *revert* action is now always destructive and can only operate 
on the current trace.
* Add the Tools module.
This module includes an implementation of the exploring interpreter protocol and an implementation
of an language parametric REPL.