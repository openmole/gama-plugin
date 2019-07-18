#!/bin/bash

cd openmole

git submodule init
git submodule update
git lfs fetch

git checkout v9.1

(cd build-system && sbt clean publishLocal)
(cd libraries && sbt clean publishLocal)
(cd openmole && sbt clean publishLocal)
(cd openmole && sbt "project openmole" assemble)

cd ..

./gradlew assemble

rm -f /get-gama-p2/build/p2asmaven/p2/p2/plugins/org.eclipse.osgi*
