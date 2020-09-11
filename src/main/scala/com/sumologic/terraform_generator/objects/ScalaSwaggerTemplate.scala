package com.sumologic.terraform_generator.objects

import com.sumologic.terraform_generator.objects.TerraformSupportedOperations.crud

case class ScalaSwaggerTemplate(sumoSwaggerClassName: String,
                                supportedEndpoints: List[ScalaSwaggerEndpoint]) extends ScalaTerraformEntity {

  def getAllTypesUsed: Set[ScalaSwaggerType] = {
    // FIXME: There is only one tf resource. We should add it as class member of ScalaSwaggerTemplate instead of
    //  traversing all endpoints.
    val swaggerResponse = supportedEndpoints.flatMap { endpoint =>
      endpoint.responses.find { response =>
        response.respTypeName == sumoSwaggerClassName
      }
    }.head
    assert(swaggerResponse.respTypeOpt.isDefined, s"ScalaSwaggerType missing for ${swaggerResponse.respTypeName}")

    val mainClassType = swaggerResponse.respTypeOpt.get

    val otherTypes = mainClassType.props.filter {
      prop => prop.getType.props.nonEmpty
    }

    Set(mainClassType) ++ otherTypes.map(_.getType).toSet
  }

  // FIXME: Get rid of this method. There is only one terraform resource and we already know about that.
  //  Don't look at create and update request response models, instead make resource a data member of
  //  ScalaSwaggerTemplate class.
  def getUpdateAndCreateRequestBodyType: List[ScalaSwaggerType] = {
    val endpoints = supportedEndpoints.filter { op: ScalaSwaggerEndpoint =>
      op.endpointName.equalsIgnoreCase(TerraformSupportedOperations.UPDATE + sumoSwaggerClassName) ||
        op.endpointName.equalsIgnoreCase(TerraformSupportedOperations.CREATE + sumoSwaggerClassName) ||
        op.responses.map(_.respTypeName).contains(sumoSwaggerClassName)
    }

    val types = endpoints.flatMap { endpoint =>
      val filteredParams = endpoint.parameters.filter(_.paramType == TerraformSupportedParameterTypes.BodyParameter)
      filteredParams.flatMap(_.param.getAllTypes) ++
          endpoint.responses.filter(_.respTypeOpt.isDefined).map(_.respTypeOpt.get)
    }

    types.filter(_.name.toUpperCase.equals(sumoSwaggerClassName.toUpperCase())).distinct
  }

  def getDataSourceFuncMappings: String = {
    val funcMappings: String = getFunctionMappings(
      List[String](TerraformSupportedOperations.GET)).mkString(",\n").concat(",")

    val mainClass = getMainObjectClass

    val mainClassProps = mainClass.props.map {
      obj: ScalaSwaggerObject =>
        obj.getAsTerraformSchemaType(true)
    }.mkString(",\n").concat(",")

    s"""func dataSourceSumologic$sumoSwaggerClassName() *schema.Resource {
       |    return &schema.Resource{
       |        $funcMappings
       |        Schema: map[string]*schema.Schema{
       |            $mainClassProps
       |        },
       |    }
       |}""".stripMargin
  }


  def getFunctionName(opName: String, prefix: String): String = {
    val camelOp = opName.substring(0, 1).toUpperCase() + opName.substring(1)
    if (camelOp == "Get") {
      s"$prefix${sumoSwaggerClassName.capitalize}Read"
    } else {
      s"$prefix${sumoSwaggerClassName.capitalize}$camelOp"
    }
  }

  def getFunctionMappings(interestedInOps: List[String]): List[String] = {
    interestedInOps.map {
      op: String =>
        val camelOp = op.substring(0, 1).toUpperCase() + op.substring(1)
        if (camelOp == "Get") {
          s"Read: ${getFunctionName(op, "resourceSumologic")}"
        } else {
          s"$camelOp: ${getFunctionName(op, "resourceSumologic")}"
        }
    }
  }

  def getMainObjectClass: ScalaSwaggerType = {
    val typesUsed: Set[ScalaSwaggerType] = getAllTypesUsed

    typesUsed.find {
      t => t.name.toUpperCase == sumoSwaggerClassName.toUpperCase() ||
           t.name.toUpperCase.contains(sumoSwaggerClassName.toUpperCase)
    }.getOrElse {
      throw new RuntimeException("No Main Class. This should not happen in getMainObjectClass ")
    }
  }

  def getResourceFuncMappings: String = {
    val funcMappings: String = getFunctionMappings(
      crud.filter(_.toLowerCase != "exists")
    ).mkString(",\n      ").concat(",")

    // TODO This assumption is too optimistic
    val classes = getUpdateAndCreateRequestBodyType

    val classesProps = classes.flatMap {
      _.props.filter { prop =>
        !prop.getName.toLowerCase.equals("id")
      }
    }.toSet

    val propsObjects = classesProps.map { swaggerObject =>
      swaggerObject.getAsTerraformSchemaType(false)
    }.toList.toSet.mkString(",\n").concat(",")

    // Only supporting query params for now. Assuming path parameters in CRUD endpoints will only be id.
    // Not supporting header parameters yet.
    val requestMaps = supportedEndpoints.filter { endpoint =>
      val paramTypes = endpoint.parameters.map(_.paramType)
      paramTypes.exists { x =>
        x == TerraformSupportedParameterTypes.QueryParameter || x == TerraformSupportedParameterTypes.HeaderParameter
      }
    }.map {
      endpoint =>
          """
          |"${endpoint.httpMethod.toLowerCase}_request_map": {
          |   Type: schema.TypeMap,
          |   Optional: true,
          |   Elem: &schema.Schema{
          |       Type: schema.TypeString,
          |   },
          |}
          |""".stripMargin
    }

    val requestMapsString = if (requestMaps.nonEmpty) {
      requestMaps.mkString(",\n").concat(",")
    } else {
      ""
    }

    s"""func resourceSumologic${sumoSwaggerClassName.capitalize}() *schema.Resource {
       |    return &schema.Resource{
       |        $funcMappings
       |        Importer: &schema.ResourceImporter{
       |            State: schema.ImportStatePassthrough,
       |        },
       |
       |        Schema: map[string]*schema.Schema{
       |            $propsObjects
       |            $requestMapsString
       |        },
       |    }
       |}""".stripMargin
  }

}
