package com.sumologic.terraform_generator.objects

case class OpenApiParameter(paramType: String, param: OpenApiObject)
    extends TerraformEntity {

  override def terraformify(baseTemplate: TerraformResource): String = {
    s"${param.getName} ${param.getType.name}"
  }

  def toTerraformFuncArg: String = {
    param.getAsTerraformFunctionArgument
  }
}
