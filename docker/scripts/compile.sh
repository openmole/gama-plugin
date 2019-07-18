#!/bin/sh

cd openmole
git submodule init
git submodule update
git lfs fetch

(cd build-system && sbt publishLocal)
(cd libraries && sbt publishLocal)
(cd openmole && sbt publishLocal)
(cd openmole && sbt "project openmole" assemble)

(cd openmole && ls -l openmole/bin/openmole/target/assemble/)

cd ..

ls -la

./gradlew assemble
