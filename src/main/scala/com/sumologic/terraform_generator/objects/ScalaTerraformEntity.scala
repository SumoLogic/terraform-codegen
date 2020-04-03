package com.sumologic.terraform_generator.objects

import com.sumologic.terraform_generator.StringHelper

abstract class ScalaTerraformEntity extends StringHelper {
  def indent = "    "
  def terraformify(): String = ""
}
