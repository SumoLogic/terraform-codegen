package com.sumologic.terraform_generator.objects

case class OpenApiType(name: String, props: List[OpenApiObject] = List[OpenApiObject]())
  extends TerraformEntity {

  override def terraformify(resource: TerraformResource): String = {
    if (props.isEmpty) {
      ""
    } else {
      val terraProps = props.map(_.terraformify(resource)).toSet
      s"type $name struct {\n" + terraProps.mkString("") + "}\n"
    }
  }

  def getAsTerraformSchemaType(resource: TerraformResource): String = {
    if (props.isEmpty) {
      ""
    } else {
      val terraProps = props.map(indent + _.terraformify(resource))
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
