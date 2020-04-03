package com.sumologic.terraform_generator.objects

abstract class SumoSwaggerObject(name: String,
                                        objType: SumoSwaggerType,
                                        required: Boolean,
                                        defaultOpt: Option[AnyRef],
                                        description: String,
                                        example: String,
                                        createOnly: Boolean = false) extends SumoTerraformEntity {
  // TODO Assumption of NO collision without namespaces is probably wrong - should fix
  def getAllTypes(): List[SumoSwaggerType] = {
    List(objType) ++ objType.props.flatMap(_.getAllTypes())
  }

  def getName(): String = { name }
  def getType(): SumoSwaggerType = { objType }
  def getRequired(): Boolean = { required }
  def getDescription(): String = { description }
  def getExample(): String = { example }
  def getCreateOnly(): Boolean = { createOnly }

  def getAsTerraformFunctionArgument(): String

  def getAsTerraformSchemaType(forUseInDataResource: Boolean): String = {
    val schemaType = if (this.isInstanceOf[SumoSwaggerObjectArray]) {
      SumoTerraformSchemaTypes.swaggerTypeToTerraformSchemaType("array")
    } else {
      SumoTerraformSchemaTypes.swaggerTypeToTerraformSchemaType(objType.name)
    }
    val requiredTxt = if (required) { // TODO: check why Frank did it this way
      "Required: true"
    } else {
      "Optional: true"
    }
    val specifics = if (forUseInDataResource) {
      "Computed: true"
    } else {
      defaultOpt match { // This probably won't work for composite types
        case Some(defaultValue) => "ForceNew: false,\nDefault: " + defaultValue.toString
        case None => "ForceNew: false"
      }
    }

    val elementType = if (this.isInstanceOf[SumoSwaggerObjectArray]) {
      s"""Elem:  &schema.Schema{
         |            Type: ${SumoTerraformSchemaTypes.swaggerTypeToTerraformSchemaType(objType.name)},
         |           },""".stripMargin
    } else {
      ""
    }
    val noCamelCaseName = removeCamelCase(name)
    "\"" + noCamelCaseName + "\"" + s": {\n           Type: $schemaType,\n          $requiredTxt,\n           $specifics,\n           $elementType\n         }"
  }
}

case class SumoSwaggerObjectSingle(name: String,
                                   objType: SumoSwaggerType,
                                   required: Boolean,
                                   defaultOpt: Option[AnyRef],
                                   description: String,
                                   example: String,
                                   createOnly: Boolean = false) extends
  SumoSwaggerObject(name: String, objType: SumoSwaggerType, required: Boolean, defaultOpt: Option[AnyRef], description, example, createOnly) {
  override def terraformify(): String = {
    val req = if (name.toLowerCase != "id") {
      ""
    } else {
      ",omitempty"
    }
    if (name.toLowerCase == "id") {
      s"${name.toUpperCase} ${objType.name} " + "`" + "json:\"" + name + req + "\"" + "`" + "\n"
    } else {
      s"${name.capitalize} ${objType.name} " + "`" + "json:\"" + name + req + "\"" + "`" + "\n"
    }
  }

  def getAsTerraformFunctionArgument(): String = {
    s"$name ${objType.name}"
  }
}

case class SumoSwaggerObjectArray(name: String,
                                  objType: SumoSwaggerType,
                                  required: Boolean,
                                  defaultOpt: Option[AnyRef],
                                  description: String,
                                  example: String,
                                  createOnly: Boolean = false) extends
  SumoSwaggerObject(name: String, objType: SumoSwaggerType, required: Boolean, defaultOpt: Option[AnyRef], description, example, createOnly) {
  override def terraformify(): String = {
    val req = if (required) {
      ""
    } else {
      ",omitempty"
    }

    s"${name.capitalize} []${objType.name} " + "`" + "json:\"" + name + req + "\"" + "`" + "\n"
  }

  def getAsTerraformFunctionArgument(): String = {
    s"$name []${objType.name}"
  }


}
