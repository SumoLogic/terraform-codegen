package com.sumologic.terraform_generator.objects

case class OpenApiResponse(respTypeName: String, respTypeOpt: Option[OpenApiType])
    extends TerraformEntity {

  override def terraformify(resource: TerraformResource): String = {
    if(respTypeOpt.isEmpty) {
      "empty_resp_body"
    } else {
      respTypeOpt.get.name
    }
  }

  override def toString: String = {
    s"OpenApiResponse(name=$respTypeName, respTypeOpt=$respTypeOpt)"
  }
}
