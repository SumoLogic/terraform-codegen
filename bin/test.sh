#!/bin/bash

set -e

# setup
if [ $# -eq 0 ]; then
  echo "================================================================"
  echo "ERROR: Insufficient args"
  echo "ERROR: Usage: ./bin/test.sh <yaml-file> [clone-dir-for-provider]"
  echo "================================================================"
  exit 1
fi

YAML_FILE="$1"
TF_GENERATOR_DIR="$(dirname "$0")/.."
TF_PROVIDER_CLONE_DIR=${2:-"/tmp"}
TF_SUMOLOGIC_PROVIDER_DIR="$TF_PROVIDER_CLONE_DIR/terraform-provider-sumologic"


validateDependencies() {
  if ! [ -x "$(command -v terraform)" ]; then
    echo "Terraform is not installed."
    echo "See https://learn.hashicorp.com/terraform/getting-started/install.html"
    exit 1
  fi

  if ! [ -x "$(command -v go)" ]; then
    echo "golang is not installed."
    echo "See https://golang.org/doc/install"
    exit 1
  fi
}

validateEnv() {
  if [ ! -n "$SUMOLOGIC_ACCESSID" ]; then
    echo "Env var 'SUMOLOGIC_ACCESSID' must be set"
    exit 1
  fi

  if [ ! -n "$SUMOLOGIC_ACCESSKEY" ]; then
    echo "Env var 'SUMOLOGIC_ACCESSID' must be set"
    exit 1
  fi

  if [ ! -n "$SUMOLOGIC_BASE_URL" ]; then
    echo "Env var 'SUMOLOGIC_BASE_URL' must be set"
    echo "Set it to API server  e.g. for nite, set it to 'https://nite-api.sumologic.net/api/'"
    exit 1
  else
    export SUMOLOGIC_ENVIRONMENT="us2"
  fi
}


# generate provider files
runGenerator() {
  $TF_GENERATOR_DIR/bin/run.sh $YAML_FILE
}

# move generated files to terraform provider
cloneProvider() {
  echo "Cloning terraform-provider-sumologic repo in '$TF_SUMOLOGIC_PROVIDER_DIR'"
  if [ ! -d $TF_SUMOLOGIC_PROVIDER_DIR ]; then
    git clone https://github.com/terraform-providers/terraform-provider-sumologic --quiet $TF_SUMOLOGIC_PROVIDER_DIR
  fi
}

setupProvider() {
  rm -fr $TF_SUMOLOGIC_PROVIDER_DIR
  mkdir -p $TF_PROVIDER_CLONE_DIR
  cloneProvider
  mv -vf $TF_GENERATOR_DIR/target/resources/resource_sumologic_*.go $TF_SUMOLOGIC_PROVIDER_DIR/sumologic
  mv -vf $TF_GENERATOR_DIR/target/resources/sumologic_*.go $TF_SUMOLOGIC_PROVIDER_DIR/sumologic
  mv -vf $TF_GENERATOR_DIR/target/resources/provider.go $TF_SUMOLOGIC_PROVIDER_DIR/sumologic
}

installGoImport() {
  if ! [ -x "$(command -v go)" ]; then
    go get golang.org/x/tools/cmd/goimports
  fi
}

fmtProvider() {
  cd $TF_SUMOLOGIC_PROVIDER_DIR
  goimports -w sumologic
}

runAcceptanceTests() {
  echo "------------------------------------------------------------------------"
  echo "Running Acceptance Tests"
  echo "------------------------------------------------------------------------"
  cd $TF_SUMOLOGIC_PROVIDER_DIR
  make install
  make testacc
  echo "------------------------------------------------------------------------"
}

# Runs tests for a particular resource.
runResourceTests() {
  cd $TF_SUMOLOGIC_PROVIDER_DIR/sumologic
  resourceName=$1
  echo "------------------------------------------------------------------------"
  echo "Running Acceptance Tests for resource '$resourceName'"
  echo "------------------------------------------------------------------------"
  TF_ACC=1 go test -v -run TestAccSumologic${TF_RESOURCE_NAME}_basic
  TF_ACC=1 go test -v -run TestAcc${TF_RESOURCE_NAME}_create
  TF_ACC=1 go test -v -run TestAcc${TF_RESOURCE_NAME}_update
  echo "------------------------------------------------------------------------"
}


validateDependencies && validateEnv && installGoImport

runGenerator && setupProvider && fmtProvider

if [ -n "$TF_RESOURCE_NAME" ]; then
  # TF_RESOURCE_NAME must be set to same value same as x-tf-resource-name extension.
  runResourceTests $TF_RESOURCE_NAME
else
  runAcceptanceTests
fi
