package com.sumologic.terraform_generator

trait TerraformGeneratorHelper {
  def removeCamelCase(propName: String): String = {
    if (propName.exists(_.isUpper)) {
      lowerCaseFirstLetter(propName).map(char => if (char.isUpper) {
        "_" + char.toLower
      } else {
        char
      }).mkString
    } else {
      propName
    }
  }

  def lowerCaseFirstLetter(name: String): String = {
    name.substring(0, 1).toLowerCase() + name.substring(1)
  }

  def addSpace(name:String): String = {
    if (name.exists(_.isUpper)) {
      name.map(char => if (char.isUpper) {
        if (name.indexOf(char) == 0) {
          char
        } else {
          " " + char
        }
      } else {
        char
      }).mkString
    } else {
      name
    }
  }
}
