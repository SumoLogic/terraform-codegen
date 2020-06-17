#!/bin/bash

set -e

$GENERATOR_DIRECTORY/bin/run.sh $YAML_PATH

mv $GENERATOR_DIRECTORY/target/resources/*scheduled* $TF_SUMOLOGIC_PROVIDER/sumologic

mv $GENERATOR_DIRECTORY/target/resources/*role* $TF_SUMOLOGIC_PROVIDER/sumologic

mv $GENERATOR_DIRECTORY/target/resources/*extraction* $TF_SUMOLOGIC_PROVIDER/sumologic

mv $GENERATOR_DIRECTORY/target/resources/*partition* $TF_SUMOLOGIC_PROVIDER/sumologic

cd $TF_SUMOLOGIC_PROVIDER

make fmt

make install

make testacc
