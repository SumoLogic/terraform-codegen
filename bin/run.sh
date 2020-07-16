#!/bin/bash

if [ $# -eq 0 ]; then
  echo "=========================================================="
  echo "ERROR: Insufficient args"
  echo "ERROR: Usage: ./bin/run.sh <yaml-file>"
  echo "=========================================================="
  exit 1
fi

export JAVA_PROGRAM_ARGS=`echo "$@"`
# Enable debug logs by adding -Dorg.slf4j.simpleLogger.log.terraform_generator=debug
# make sure you configure log level to debug in log4j.properties as well.
# Set -Dorg.slf4j.simpleLogger.defaultLogLevel=WARN to change maven log level.
./mvnw -q clean
./mvnw -B compile exec:java  -Dexec.mainClass="com.sumologic.terraform_generator.TerraformGenerator" -Dexec.args="$JAVA_PROGRAM_ARGS"
