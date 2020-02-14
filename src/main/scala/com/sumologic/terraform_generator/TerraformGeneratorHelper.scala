package com.sumologic.terraform_generator

trait TerraformGeneratorHelper {
  def removeCamelCase(propName: String): String = {
    if (propName.exists(_.isUpper)) {
      propName.map(char => if (char.isUpper) {
        "_" + char.toLower
      } else {
        char
      }).mkString
    } else {
      propName
    }
  }
}
