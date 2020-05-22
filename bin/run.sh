#!/bin/bash

export JAVA_PROGRAM_ARGS=`echo "$@"`

if [ $# -eq 0 ]
    then
	echo "Must supply input yaml file"
elif [ $# -eq 1 ]
    then
	./mvnw compile exec:java -Dexec.mainClass="com.sumologic.terraform_generator.TerraformGenerator" -Dexec.args="$JAVA_PROGRAM_ARGS" -Duser.home="SwaggerToTerraformGenerator"
else
    ./mvnw compile exec:java -Dexec.mainClass="com.sumologic.terraform_generator.TerraformGenerator" -Dexec.args="$JAVA_PROGRAM_ARGS" -Duser.home="SwaggerToTerraformGenerator"
fi
