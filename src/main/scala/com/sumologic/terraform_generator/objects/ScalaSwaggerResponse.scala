package com.sumologic.terraform_generator.objects

case class ScalaSwaggerResponse(respTypeName: String, respTypeOpt: Option[ScalaSwaggerType]) extends ScalaTerraformEntity {
  override def terraformify(baseTemplate: ScalaSwaggerTemplate): String = {
    if(respTypeOpt.isEmpty) {
      "empty_resp_body"
    } else {
      respTypeOpt.get.name
    }
  }

  override def toString = {
    s"ScalaSwaggerResponse(name=$respTypeName, respTypeOpt=$respTypeOpt)"
  }
}
