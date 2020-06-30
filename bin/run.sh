#!/bin/bash

if [ $# -eq 0 ]; then
  echo "=========================================================="
  echo "ERROR: Insufficient args"
  echo "ERROR: Usage: ./bin/run.sh <yaml-file>"
  echo "=========================================================="
  exit 1
fi

export JAVA_PROGRAM_ARGS=`echo "$@"`
# to enable debug logs add -Dorg.slf4j.simpleLogger.log.terraform_generator=debug
# make sure you configure log level to debug in log4j.properties as well.
./mvnw clean
./mvnw -B compile exec:java -Dexec.mainClass="com.sumologic.terraform_generator.TerraformGenerator" -Dexec.args="$JAVA_PROGRAM_ARGS"
