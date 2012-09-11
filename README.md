Introduction
==

This will implement a scheme interpreter in two language, one is in scheme itself, and one in C, so this project will have two branchs, `master` for C version and `scheme-version` for scheme version.

I am doing this project because I want to know the internal structure of interpreter, so that this projcet __won't__ be a product qualited project, but merely a toy, and also I will first implement scheme version, which is much easier to implement, and after I have fully understand the functionality of the interpreter I will start to implement the C version, which involve so much memory manipluation.

Goal
==

Even though this will be just a toy implemention, but it will still has a goal, which include:
* implement a basic functionality of scheme, but __not__ include `call/cc` call
* implement a CL like macro system, which include `defmacro`, `backquote`, `comma` and `comma-at` primitive
* implement a hygiene macro system which is embedded in `defmacro`, so that user will never required to deal with such issue

Something I will __not__ implement:
* compiler, so that si will only be a interpreter, __not__ a compiler
* `define-syntax` like macro system, which I think is much more ugly than defmacro
* large set of system related call interface like `open` or read/write