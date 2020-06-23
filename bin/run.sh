#!/bin/bash

if [ $# -eq 0 ]; then
  echo "=========================================================="
  echo "ERROR: Insufficient args"
  echo "ERROR: Usage: ./bin/run.sh <yaml-file>"
  echo "=========================================================="
  exit 1
fi

export JAVA_PROGRAM_ARGS=`echo "$@"`
./mvnw -B compile exec:java -Dexec.mainClass="com.sumologic.terraform_generator.TerraformGenerator" -Dexec.args="$JAVA_PROGRAM_ARGS"
