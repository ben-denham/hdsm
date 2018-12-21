# HDSM

This repository contains the prototype implementation of the
Hierarchical Distributed Stream Classifier (HDSM), which is a
distributed data mining approach to classifying heterogeneous data
streams.

## Quickstart

If you have Docker installed, you can run the experiments contained
within this codebase by executing `make jupyter`, opening the returned
URL in a web browser, and executing the contents of the provided
Jupyter notebooks (This has only been tested on an Ubuntu 16.04 host
running Docker 17.05.0-ce).

Note that the results of each experiment are saved to disk to prevent
the need to re-execute the experiments when re-viewing an experiment's
results.

## Dependencies

* Java (>= 1.8.0)
* Leiningen (>= 2.0)
* For ClojureCLR:
    * Mono
    * Nuget (apt-get install)

## Running Tests

`make test`

Tests can also be run repeatedly from a Clojure REPL:

1. `lein repl`
2. `(use 'midje.repl)`
3. `(autotest)`

## Further Usage

See Makefile commands

## CLR

This project was originally constructed to run on both JVM and CLR
platforms (in order to use different underlying classifiers), but has
since become dependent on java packages.

ClojureCLR 1.9.0 was used because 1.7.0 triggers the wrong reader
conditional (it fires on :clj, which is used for JVM Clojure -
https://dev.clojure.org/jira/browse/CLJCLR-76), and because 1.8.0 does
not run successfully on Mono.
