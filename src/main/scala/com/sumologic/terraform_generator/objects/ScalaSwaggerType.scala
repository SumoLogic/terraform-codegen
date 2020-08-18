package com.sumologic.terraform_generator.objects

case class ScalaSwaggerType(name: String, props: List[ScalaSwaggerObject] = List[ScalaSwaggerObject]())
  extends ScalaTerraformEntity {

  override def terraformify(baseTemplate: ScalaSwaggerTemplate): String = {
    if (props.isEmpty) {
      ""
    } else {
      val terraProps = props.map(_.terraformify(baseTemplate)).toSet
      s"type $name struct {\n" + terraProps.mkString("") + "}\n"
    }
  }

  def getAsTerraformSchemaType(baseTemplate: ScalaSwaggerTemplate): String = {
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
    s"ScalaSwaggerType(name=$name, props=$propNames)"
  }
}
