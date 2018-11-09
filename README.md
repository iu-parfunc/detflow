# `detflow`
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]


<!--
 * Travis: [![Build](https://img.shields.io/travis/iu-parfunc/detmonad.svg)](https://travis-ci.org/iu-parfunc/detmonad)
 * Jenkins: [![Build Status](http://tester-lin.soic.indiana.edu:8080/buildStatus/icon?job=detmonad)](http://tester-lin.soic.indiana.edu:8080/job/detmonad)
-->


[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

This is the source code for our OOPSLA 2017 paper [Monadic composition for deterministic, parallel batch processing](https://dl.acm.org/citation.cfm?id=3152284.3133897). 

Our _DetFlow_ system allows for deterministic execution of batch processing workloads like software builds and bioinformatics data pipelines. _DetFlow_ consists of two parts: 1) a parallel coordinator process written in a deterministic language (in our case, Haskell) to support low-overhead deterministic parallelism and 2) a sandbox for black-box binary software that supports legacy programs but runs them sequentially. For a software build, the _make_ process is the coordinator, and each build rule (running `gcc`, say) runs in a sandbox. _DetFlow_ allows multiple sandbox instances to run in parallel, thereby achieving good scalability on multicore hardware.
