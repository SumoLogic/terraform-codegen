#!/bin/bash

set -e

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

  if [ ! -n "$GOPATH" ]; then
    echo "Env var 'GOPATH' must be set."
    echo "Add 'export GOPATH=$HOME/.go' to ~/.profile file and launch a new shell"
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

installGoImport() {
  if ! [ -x "$(command -v goimports)" ]; then
    if $(go get golang.org/x/tools/cmd/goimports); then
      export PATH=$PATH:$GOPATH/bin
    else
      echo "Failed to install goimports"
      echo "Try installing manually - https://godoc.org/golang.org/x/tools/cmd/goimports"
      echo "Make sure to update the PATH var ('export PATH=$PATH:$GOPATH/bin')."
      exit 1
    fi
  fi
}

# generate provider files
runGenerator() {
  echo "-------------------------------------------------------------------------------"
  echo "Running generator"
  echo "-------------------------------------------------------------------------------"
  $TF_CODEGEN_DIR/bin/run.sh $YAML_FILE
}

# move generated files to terraform provider
cloneProvider() {
  echo "Cloning terraform-provider-sumologic repo in '$TF_SUMOLOGIC_PROVIDER_DIR'"
  if [ ! -d $TF_SUMOLOGIC_PROVIDER_DIR ]; then
    git clone https://github.com/terraform-providers/terraform-provider-sumologic --quiet $TF_SUMOLOGIC_PROVIDER_DIR
  fi
}

setupProvider() {
  echo "-------------------------------------------------------------------------------"
  echo "Setting up Terraform Provider"
  echo "-------------------------------------------------------------------------------"
  rm -fr $TF_SUMOLOGIC_PROVIDER_DIR
  mkdir -p $TF_PROVIDER_OUTPUT_DIR
  cloneProvider
  mv -vf $TF_CODEGEN_DIR/target/resources/resource_sumologic_*.go $TF_SUMOLOGIC_PROVIDER_DIR/sumologic
  mv -vf $TF_CODEGEN_DIR/target/resources/sumologic_*.go $TF_SUMOLOGIC_PROVIDER_DIR/sumologic
  mv -vf $TF_CODEGEN_DIR/target/resources/provider.go $TF_SUMOLOGIC_PROVIDER_DIR/sumologic
}

fmtProvider() {
  cd $TF_SUMOLOGIC_PROVIDER_DIR
  goimports -w sumologic
}

runAcceptanceTests() {
  echo "-------------------------------------------------------------------------------"
  echo "Running Acceptance Tests"
  echo "-------------------------------------------------------------------------------"
  cd $TF_SUMOLOGIC_PROVIDER_DIR
  make install && make testacc
}

# Runs tests for a particular resource.
runResourceTests() {
  echo "-------------------------------------------------------------------------------"
  echo "Running Acceptance Tests for resource '$TF_RESOURCE_NAME'"
  echo "-------------------------------------------------------------------------------"
  cd $TF_SUMOLOGIC_PROVIDER_DIR
  make install

  cd $TF_SUMOLOGIC_PROVIDER_DIR/sumologic
  TF_ACC=1 go test -v -run "TestAcc(Sumologic)?${TF_RESOURCE_NAME}.*"
}



# setup

test=""
yaml=""
cloneDir="/tmp"

usage() {
  echo "
    Generates Sumo Logic Terraform provider.

    If output directory is not specified (-d option), the provider will be generated
    in '/tmp' directory.

    Use '-t' option to run acceptance tests. Set env var 'TF_RESOURCE_NAME' to a
    resource name if you want to run acceptance tests only for a particular resource.
    The resource name must be same as the value for x-tf-resource-name extension. For
    example, setting TF_RESOURCE_NAME to 'Role' will run acceptance tests only for
    the 'Role' resource.


    Usage: $(basename $0) [OPTIONS]

    Options:
      -f <yaml-file>                Input yaml file
      -d <output-dir-for-provider>  Output directory for the provider
      -t                            Run acceptance tests
      -h                            Display this help message
  "
  exit 0
}

while getopts 'tf:d:h' flag; do
  case "${flag}" in
    t) test="true" ;;
    f) yaml="${OPTARG}" ;;
    d) cloneDir="${OPTARG}" ;;
    h | *) usage ;;
  esac
done

if [ -z "$yaml" ]; then
  usage
fi

YAML_FILE="$yaml"
TF_CODEGEN_DIR="$(dirname "$0")/.."
TF_PROVIDER_OUTPUT_DIR="$cloneDir"
TF_SUMOLOGIC_PROVIDER_DIR="$TF_PROVIDER_OUTPUT_DIR/terraform-provider-sumologic"


validateDependencies && validateEnv && installGoImport

runGenerator && setupProvider && fmtProvider

if [ -n "$test" ]; then
  if [ -n "$TF_RESOURCE_NAME" ]; then
    # TF_RESOURCE_NAME must be set to same value same as x-tf-resource-name extension.
    runResourceTests
  else
    runAcceptanceTests
  fi
fi
