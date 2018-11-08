# `detflow`
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]


 * Travis: [![Build](https://img.shields.io/travis/iu-parfunc/detmonad.svg)](https://travis-ci.org/iu-parfunc/detmonad)
 * Jenkins: [![Build Status](http://tester-lin.soic.indiana.edu:8080/buildStatus/icon?job=detmonad)](http://tester-lin.soic.indiana.edu:8080/job/detmonad)


[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

Here we'll sketch the beginnings of a Det monad that can replace IO in the main function.
It will use a basic, low-performance version of a deterministic logical clock initially.

