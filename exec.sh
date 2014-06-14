#/usr/bin/env bash

tmpfile=tmp.run
scalabin=scala
systemlib=target/scala-2.10/looneesha-assembly-0.0.1.jar

rm -f $tmpfile

for i in $@; do
    cat $i >> $tmpfile
done

$scalabin -cp $systemlib $tmpfile
rm -f $tmpfile
