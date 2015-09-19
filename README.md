Reed-Solomon Error Correction
=============================

This project implements a Reed-Solomon error correction code over arbitrary finite fields. The `edu.jingw.algebra` package provides classes to facilitate mathematical operations with arbitrary groups, rings, and fields. Most importantly, polynomial rings are implemented for arbitrary rings or fields through generics. The `edu.jingw.reedsolomon` uses these features to concisely implement the Reed-Solomon scheme.

Building and Running
====================
[![Build Status](https://travis-ci.org/jingw/reedsolomon.png)](https://travis-ci.org/jingw/reedsolomon)
[![codecov.io](http://codecov.io/github/jingw/reedsolomon/coverage.svg?branch=master)](http://codecov.io/github/jingw/reedsolomon?branch=master)

We use `sbt` for building.

1. Check out the code:

		$ git clone git://github.com/jingw/reedsolomon.git
		$ cd reedsolomon

2. Run `sbt` tasks:

		$ sbt clean compile test doc

3. Run a simulation of the error correction performance with

		$ sbt run

   The simulation creates random messages, encodes them using two different RS codes, and randomly corrupts the codewords. It then attempts to decode them and collects statistics on the success rate. The data is output to CSV files.

History
=======
This project began under Caltech's error-correction coding course (EE/Ma/CS 127).
