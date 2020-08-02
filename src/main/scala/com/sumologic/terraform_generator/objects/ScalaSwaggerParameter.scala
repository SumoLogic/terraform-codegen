package com.sumologic.terraform_generator.objects

case class ScalaSwaggerParameter(paramType: String, param: ScalaSwaggerObject)
    extends ScalaTerraformEntity {

  override def terraformify(baseTemplate: ScalaSwaggerTemplate): String = {
    s"${param.getName} ${param.getType.name}"
  }

  def toTerraformFuncArg: String = {
    param.getAsTerraformFunctionArgument
  }
}
