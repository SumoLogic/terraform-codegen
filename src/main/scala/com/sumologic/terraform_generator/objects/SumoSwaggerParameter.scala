package com.sumologic.terraform_generator.objects

case class SumoSwaggerParameter(paramType: String, param: SumoSwaggerObject) extends SumoTerraformEntity {
  override def terraformify(): String = {
    s"${param.getName()} ${param.getType().name}"
  }

  def toTerraformFuncArg(): String = {
    param.getAsTerraformFunctionArgument()
  }
}
