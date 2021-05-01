package com.sumologic.terraform_generator.objects

case class OpenApiType(name: String, props: List[OpenApiObject] = List[OpenApiObject]())
  extends TerraformEntity {

  override def terraformify(baseTemplate: TerraformResource): String = {
    if (props.isEmpty) {
      ""
    } else {
      val terraProps = props.map(_.terraformify(baseTemplate)).toSet
      s"type $name struct {\n" + terraProps.mkString("") + "}\n"
    }
  }

  def getAsTerraformSchemaType(baseTemplate: TerraformResource): String = {
    if (props.isEmpty) {
      ""
    } else {
      val terraProps = props.map(indent + _.terraformify(baseTemplate))
      s"type $name struct {\n" + terraProps.mkString("") + "}\n"
    }
  }

  def isCompositeType: Boolean = {
    props.nonEmpty
  }

  override def toString = {
    val propNames = props.map(_.getName)
    s"OpenApiType(name=$name, props=$propNames)"
  }
}
