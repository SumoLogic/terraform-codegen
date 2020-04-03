package com.sumologic.terraform_generator.objects

case class ScalaSwaggerType(name: String, props: List[ScalaSwaggerObject] = List[ScalaSwaggerObject]())
  extends ScalaTerraformEntity {
  override def terraformify(): String = {
    if (props.isEmpty) {
      ""
    } else {
      val terraProps = props.map(indent + _.terraformify()).toSet
      if (name.toLowerCase == "errorresponse" || name.toLowerCase == "errordescription") {
        ""
      } else {
        s"type $name struct {\n" + terraProps.mkString("") + "}\n"
      }
    }
  }

  def getAsTerraformSchemaType(): String = {
    if (props.isEmpty) {
      ""
    } else {
      val terraProps = props.map(indent + _.terraformify())
      s"type $name struct {\n" + terraProps.mkString("") + "}\n"
    }
  }

  def isCompositeType(): Boolean = {
    !props.isEmpty
  }
}
