package com.sumologic.terraform_generator.objects

case class SumoSwaggerResponse(respTypeName: String, respTypeOpt: Option[SumoSwaggerType]) extends SumoTerraformEntity {
  override def terraformify(): String = {
    if(respTypeOpt.isEmpty) {
      "empty_resp_body"
    } else {
      respTypeOpt.get.name
    }
  }
}
