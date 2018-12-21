#!/bin/bash

# https://stackoverflow.com/a/246128
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
CLOJURE_DIR="$DIR/$1"

cd $CLOJURE_DIR

# http://charsequence.blogspot.co.nz/2015/07/running-clojureclr-17-on-mac-os-x.html

# This is the list of core Clojure libraries in Clojure 1.9.0
CLOJURE_LIBS=(
    clojure.clr.io.clj.dll
    clojure.core.clj.dll
    clojure.core.protocols.clj.dll
    clojure.core.server.cljc.dll
    clojure.core.specs.cljc.dll
    clojure.core_clr.clj.dll
    clojure.core_deftype.clj.dll
    clojure.core_print.clj.dll
    clojure.core_proxy.clj.dll
    clojure.edn.cljc.dll
    clojure.genclass.clj.dll
    clojure.gvec.clj.dll
    clojure.instant.clj.dll
    clojure.main.clj.dll
    clojure.pprint.cl_format.clj.dll
    clojure.pprint.clj.dll
    clojure.pprint.column_writer.clj.dll
    clojure.pprint.dispatch.clj.dll
    clojure.pprint.pprint_base.clj.dll
    clojure.pprint.pretty_writer.clj.dll
    clojure.pprint.print_table.clj.dll
    clojure.pprint.utilities.clj.dll
    clojure.repl.clj.dll
    clojure.set.clj.dll
    clojure.spec.cljc.dll
    clojure.spec.gen.cljc.dll
    clojure.stacktrace.clj.dll
    clojure.string.clj.dll
    clojure.template.clj.dll
    clojure.test.clj.dll
    clojure.uuid.clj.dll
    clojure.walk.clj.dll
)

DOTNETS=`ls -1 $CLOJURE_DIR/lib`
for DOTNET in $DOTNETS; do
    rm -rf all/$DOTNET
    mkdir -p all/$DOTNET
    cp -R lib/$DOTNET/* all/$DOTNET
    cp -R tools/$DOTNET/* all/$DOTNET

    cd all/$DOTNET
    for CLOJURE_LIB in ${CLOJURE_LIBS[@]}; do
        ln -s Clojure.dll $CLOJURE_LIB
    done
    cd ../..
done
