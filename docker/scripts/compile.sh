#!/bin/sh

cd openmole
git submodule init
git submodule update
git lfs fetch
git checkout v9.1

(cd build-system && sbt publishLocal)
(cd libraries && sbt publishLocal)
(cd openmole && sbt publishLocal)
(cd openmole && sbt "project openmole" assemble)

(cd openmole && ls -l openmole/bin/openmole/target/assemble/)

cd ..

./gradlew assemble
