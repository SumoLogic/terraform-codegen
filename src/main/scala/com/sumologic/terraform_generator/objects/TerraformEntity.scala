package com.sumologic.terraform_generator.objects

import com.sumologic.terraform_generator.StringHelper

abstract class TerraformEntity extends StringHelper {
  def indent = "    "
  def terraformify(baseTemplate: TerraformResource): String = ""
}
