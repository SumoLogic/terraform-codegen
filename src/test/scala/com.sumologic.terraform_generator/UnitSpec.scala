package com.sumologic.terraform_generator

import org.scalatest.concurrent.Eventually
import org.scalatest.{Matchers, WordSpec}

abstract class UnitSpec
  extends WordSpec
    with Eventually
    with Matchers {

}
