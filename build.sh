#!/bin/bash

cd openmole

git lfs fetch

cd build-system && sbt clean publishLocal
cd ..
cd libraries && sbt clean publishLocal
cd ..
cd openmole && sbt clean publishLocal

cd ..

./gradlew assemble
