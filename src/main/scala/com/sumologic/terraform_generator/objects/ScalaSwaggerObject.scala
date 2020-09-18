package com.sumologic.terraform_generator.objects

import org.openapitools.codegen.utils.StringUtils

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

  def getTerraformElementSchema: String

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
          s"""Default: "${defaultValue.toString}"""".stripMargin
        } else {
          s"""Default: ${defaultValue.toString}""".stripMargin
        }
        case None => ""
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

    val schemaFieldName = StringUtils.underscore(name)
    s"""
       |"$schemaFieldName": {
       |    Type: $schemaType,
       |    $requiredTxt,
       |    $specifics,
       |    $validationAndDiffSuppress
       |    $elementType
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

  override def getTerraformElementSchema: String = {
    val itemType = TerraformSchemaTypes.swaggerTypeToTerraformSchemaType(objType.name)
    s"""Elem:  &schema.Schema{
       |    Type: $itemType,
       |},""".stripMargin
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

  // Captures the type of item contained with in the array object.
  var items: ScalaSwaggerObject = _

  override def terraformify(baseTemplate: ScalaSwaggerTemplate): String = {
    val req = if (required) {
      ""
    } else {
      ",omitempty"
    }

    s"${name.capitalize} $getGoType " + "`" + "json:\"" + name + req + "\"" + "`" + "\n"
  }

  override def getGoType: String = {
    s"[]${items.getGoType}"
  }

  def getAsTerraformFunctionArgument: String = {
    s"$name $getGoType"
  }

  override def getTerraformElementSchema: String = {
    val schemaType = TerraformSchemaTypes.swaggerTypeToTerraformSchemaType("array")
    val itemSchema = this.items.getTerraformElementSchema
    s"""Elem:  &schema.Schema{
       |    Type: $schemaType,
       |    $itemSchema
       |},""".stripMargin
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
      // NOTE: Hack to avoid multiline and long descriptions. Need a better solution.
      val idx = this.getDescription.indexOf(". ")
      val end = if (idx != -1) idx else this.getDescription.length
      val description = this.getDescription.substring(0, end).replace('\n', ' ')
      s"""Description: "$description" """
      // TODO add specific for array items like max items, min items
    }

    // TODO Add support for validateFunc and DiffSuppressFunc

    val elementSchema = this.items.getTerraformElementSchema

    val schemaFieldName = StringUtils.underscore(name)
    s"""
       |"$schemaFieldName": {
       |    Type: $schemaType,
       |    $requiredTxt,
       |    $specifics,
       |    $elementSchema
       |}""".stripMargin
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

  override def getTerraformElementSchema: String = {
    val itemSchema = this.getType.props.map { prop =>
      prop.getAsTerraformSchemaType(false)
    }.mkString(",\n").concat(",")

    s"""Elem: &schema.Resource{
       |    Schema: map[string]*schema.Schema{
       |        $itemSchema
       |    },
       |},""".stripMargin
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
      // NOTE: Hack to avoid multiline and long descriptions. Need a better solution.
      val idx = this.getDescription.indexOf(". ")
      val end = if (idx != -1) idx else this.getDescription.length
      val description = this.getDescription.substring(0, end).replace('\n', ' ')
      s"""|MaxItems: 1,
          |Description: "$description"""".stripMargin
    }

    // TODO Add support for validateFunc and DiffSuppressFunc

    // get schema of referenced type
    val refSchemaType = this.getType.props.map { prop =>
      prop.getAsTerraformSchemaType(forUseInDataResource)
    }.mkString(",\n").concat(",")

    val elementSchema =
      s"""Elem: &schema.Resource{
         |    Schema: map[string]*schema.Schema{
         |        $refSchemaType
         |    },
         |},""".stripMargin

    val schemaFieldName = StringUtils.underscore(name)
    s"""
       |"$schemaFieldName": {
       |    Type: $schemaType,
       |    $requiredTxt,
       |    $specifics,
       |    $elementSchema
       |}""".stripMargin
  }

  override def terraformify(baseTemplate: ScalaSwaggerTemplate): String = {
    val req = if (required) {
      ""
    } else {
      ",omitempty"
    }

    s"${name.capitalize} $getGoType " + "`" + "json:\"" + name + req + "\"" + "`" + "\n"
  }
}
