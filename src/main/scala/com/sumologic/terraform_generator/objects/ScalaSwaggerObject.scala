package com.sumologic.terraform_generator.objects

abstract class ScalaSwaggerObject(name: String,
                                  objType: ScalaSwaggerType,
                                  required: Boolean,
                                  defaultOpt: Option[AnyRef],
                                  description: String,
                                  example: String = "",
                                  pattern: String = "",
                                  format: String = "",
                                  attribute: String = "",
                                  createOnly: Boolean = false) extends ScalaTerraformEntity {

  // TODO Assumption of NO collision without namespaces is probably wrong - should fix
  def getAllTypes: List[ScalaSwaggerType] = {
    List(objType) ++ objType.props.flatMap(_.getAllTypes)
  }

  def getName: String = { name }
  def getType: ScalaSwaggerType = { objType }
  def getRequired: Boolean = { required }
  def getDescription: String = { description }
  def getExample: String = { example }
  def getCreateOnly: Boolean = { createOnly }
  def getPattern: String = { pattern }
  def getDefault: Option[AnyRef] = { defaultOpt}
  def getFormat: String = { format }
  def getAttribute: String = { attribute }

  def getAsTerraformFunctionArgument: String

  def getGoType: String

  def getAsTerraformSchemaType(forUseInDataResource: Boolean): String = {
    val schemaType = if (this.isInstanceOf[ScalaSwaggerArrayObject]) {
      TerraformSchemaTypes.swaggerTypeToTerraformSchemaType("array")
    } else {
      TerraformSchemaTypes.swaggerTypeToTerraformSchemaType(objType.name)
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
        case Some(defaultValue) => if (objType.name == "string") {
          s"""ForceNew: false,
            |Default: "${defaultValue.toString}"""".stripMargin
        } else {
          s"""ForceNew: false,
             |Default: ${defaultValue.toString}""".stripMargin
        }
        case None => "ForceNew: false"
      }
    }

    val validationAndDiffSuppress = if (!this.isInstanceOf[ScalaSwaggerArrayObject] && this.getType.props.nonEmpty) {
      """ValidateFunc:     validation.StringIsJSON,
        |				DiffSuppressFunc: suppressEquivalentJsonDiffs,""".stripMargin
    } else {
      ""
    }

    val elementType = if (this.isInstanceOf[ScalaSwaggerArrayObject]) {
      if (this.getType.props.nonEmpty) {
        s"""Elem:  &schema.Schema{
           |            Type: ${TerraformSchemaTypes.swaggerTypeToTerraformSchemaType(objType.name)},
           |            ValidateFunc:     validation.StringIsJSON,
           |				    DiffSuppressFunc: suppressEquivalentJsonDiffs,
           |           },""".stripMargin
      } else {
        s"""Elem:  &schema.Schema{
           |            Type: ${TerraformSchemaTypes.swaggerTypeToTerraformSchemaType(objType.name)},
           |           },""".stripMargin
      }
    } else {
      ""
    }

    val noCamelCaseName = removeCamelCase(name)
    s"""
      |"$noCamelCaseName": {
      |   Type: $schemaType,
      |   $requiredTxt,
      |   $specifics,
      |   $validationAndDiffSuppress
      |   $elementType
      |}""".stripMargin
  }
}


case class ScalaSwaggerSimpleObject(name: String,
                                    objType: ScalaSwaggerType,
                                    required: Boolean,
                                    defaultOpt: Option[AnyRef],
                                    description: String,
                                    example: String = "",
                                    pattern: String = "",
                                    format: String = "",
                                    attribute: String = "",
                                    createOnly: Boolean = false) extends
  ScalaSwaggerObject(name: String,
    objType: ScalaSwaggerType,
    required: Boolean,
    defaultOpt: Option[AnyRef],
    description,
    example,
    pattern,
    format,
    attribute,
    createOnly) {

  override def terraformify(baseTemplate: ScalaSwaggerTemplate): String = {
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

  override def getGoType: String = {
    TerraformSchemaTypes.swaggerTypeToGoType(objType.name)
  }

  def getAsTerraformFunctionArgument: String = {
    s"$name ${objType.name}"
  }
}


case class ScalaSwaggerArrayObject(name: String,
                                   objType: ScalaSwaggerType,
                                   required: Boolean,
                                   defaultOpt: Option[AnyRef],
                                   description: String,
                                   example: String = "",
                                   pattern: String = "",
                                   format: String = "",
                                   attribute: String = "",
                                   createOnly: Boolean = false) extends
  ScalaSwaggerObject(name: String,
    objType: ScalaSwaggerType,
    required: Boolean,
    defaultOpt: Option[AnyRef],
    description,
    example,
    pattern,
    format,
    attribute,
    createOnly) {

  override def terraformify(baseTemplate: ScalaSwaggerTemplate): String = {
    val req = if (required) {
      ""
    } else {
      ",omitempty"
    }

    s"${name.capitalize} $getGoType " + "`" + "json:\"" + name + req + "\"" + "`" + "\n"
  }

  override def getGoType: String = {
    s"[]${objType.name}"
  }

  def getAsTerraformFunctionArgument: String = {
    s"$name $getGoType"
  }
}


case class ScalaSwaggerRefObject(name: String,
                                 objType: ScalaSwaggerType,
                                 required: Boolean,
                                 defaultOpt: Option[AnyRef],
                                 description: String,
                                 example: String = "",
                                 pattern: String = "",
                                 format: String = "",
                                 attribute: String = "",
                                 createOnly: Boolean = false) extends
    ScalaSwaggerObject(name: String,
      objType: ScalaSwaggerType,
      required: Boolean,
      defaultOpt: Option[AnyRef],
      description,
      example,
      pattern,
      format,
      attribute,
      createOnly) {

  override def getAsTerraformFunctionArgument: String = {
    s"$name $getGoType"
  }

  override def getGoType: String = {
    s"${objType.name.capitalize}"
  }


  override def getAsTerraformSchemaType(forUseInDataResource: Boolean): String = {
    val schemaType = TerraformSchemaTypes.swaggerTypeToTerraformSchemaType("array")

    val requiredTxt = if (required) {
      "Required: true"
    } else {
      "Optional: true"
    }

    val specifics = if (forUseInDataResource) {
      // TODO I am not sure if this is all we need.
      "Computed: true"
    } else {
      "MaxItems: 1"
    }

    // TODO Add support for validateFunc and DiffSuppressFunc

    // get schema of referenced type
    val refSchemaType = this.getType.props.map { prop =>
      prop.getAsTerraformSchemaType(forUseInDataResource)
    }.mkString(",").concat(",")

    val elementType =
      s"""Elem: &schema.Resource{
         |    Schema: map[string]*schema.Schema{
         |        $refSchemaType
         |    },
         |},""".stripMargin

    val noCamelCaseName = removeCamelCase(name)
    s"""
       |"$noCamelCaseName": {
       |   Type: $schemaType,
       |   $requiredTxt,
       |   $specifics,
       |   $elementType
       |}""".stripMargin
  }
}
