package com.sumologic.terraform_generator.objects

import com.sumologic.terraform_generator.objects.SumoSwaggerSupportedOperations.crud

case class SumoSwaggerTemplate(sumoSwaggerClassName: String,
                               supportedEndpoints: List[SumoSwaggerEndpoint]) extends SumoTerraformEntity {
  def getAllTypesUsed(): Set[SumoSwaggerType] = {
    val responsesProps = supportedEndpoints.flatMap {
      endpoint =>
        endpoint.responses.flatMap {
          response =>
            response.respTypeOpt.map {
              respType => respType.props.flatMap(_.getAllTypes())
            }
        }
    }.flatten

    val parameterProps = supportedEndpoints.flatMap {
      endpoint =>
        endpoint.parameters.flatMap {
          parameter => parameter.param.getAllTypes
        }
    }

    val endpointsSet = supportedEndpoints.flatMap { endpoint =>
      endpoint.parameters.flatMap(_.param.getAllTypes()) ++
        endpoint.responses.flatMap(_.respTypeOpt) ++
      responsesProps ++
      parameterProps

    }.toSet

    val mainClassType = endpointsSet.filter {
      sType => sType.name.toLowerCase.contains(sumoSwaggerClassName.toLowerCase)
    }.head

    val otherTypes = mainClassType.props.filter {
      prop => !prop.getType().props.isEmpty
    }

    Set(mainClassType) ++ otherTypes.map(_.getType()).toSet
  }

  def getUpdateAndCreateRequestBodyType(): List[SumoSwaggerType] = {
    val endpoints = supportedEndpoints.filter { op: SumoSwaggerEndpoint =>
      op.endpointName.equalsIgnoreCase(SumoSwaggerSupportedOperations.UPDATE + sumoSwaggerClassName) ||
        op.endpointName.equalsIgnoreCase(SumoSwaggerSupportedOperations.CREATE + sumoSwaggerClassName) ||
        op.responses.map(_.respTypeName).contains(sumoSwaggerClassName)
    }

    val types = endpoints.flatMap {
      endpoint => endpoint.parameters.filter(_.paramType == SumoTerraformSupportedParameterTypes.BodyParameter)
        .flatMap(_.param.getAllTypes())
    }

    types.filter(_.name.toUpperCase.contains(sumoSwaggerClassName.toUpperCase())).toSet.toList
  }

  def getAllRequestBodyTypesUsed(): Set[SumoSwaggerType] = {
    supportedEndpoints.flatMap { endpoint =>
      endpoint.parameters.filter(_.paramType == SumoTerraformSupportedParameterTypes.BodyParameter).
        flatMap(_.param.getAllTypes())
    }.toSet
  }

  def getDataSourceFuncMappings(): String = {
    val funcMappings: String = getFunctionMappings(
      List[String](SumoSwaggerSupportedOperations.GET)).mkString(",\n").concat(",")

    val mainClass = getMainObjectClass()

    val mainClassProps = mainClass.props.map {
      case sobj: SumoSwaggerObject =>
        sobj.getAsTerraformSchemaType(true)
    }.mkString(",\n").concat(",")

    s"""func dataSourceSumologic$sumoSwaggerClassName() *schema.Resource {
       |  return &schema.Resource{
       |    $funcMappings
       |    Schema: map[string]*schema.Schema{
       |      $mainClassProps
       |    },
       |  }
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

  def getMainObjectClass(): SumoSwaggerType = {
    val typesUsed: Set[SumoSwaggerType] = getAllTypesUsed()

    typesUsed.find {
      t => t.name.toUpperCase == sumoSwaggerClassName.toUpperCase() || t.name.toUpperCase.contains(sumoSwaggerClassName.toUpperCase)
    }.getOrElse {
      throw new RuntimeException("No Main Class. This should not happen in getMainObjectClass ")
    }
  }

  def getResourceFuncMappings(): String = {
    // TODO Each type used needs to be generated somewhere for this to work, for now...
    // ... hoping that this is all basic types

    val funcMappings: String = getFunctionMappings(crud.filter(_.toLowerCase != "exists")).mkString(",\n      ").concat(",")

    // TODO This assumption is too optimistic
    val classes = getUpdateAndCreateRequestBodyType()

    val classesProps = classes.flatMap(_.props.filter(prop => !prop.getName().toLowerCase.contains("created") &&
      !prop.getName().toLowerCase.contains("modified") && !prop.getName().toLowerCase.contains("system") &&
      !prop.getName().toLowerCase.equals("id"))).toSet //.filter(_.getRequired()).toSet


    val propsObjects = classesProps.map {
      sumoSwaggerObject: SumoSwaggerObject =>
        sumoSwaggerObject.getAsTerraformSchemaType(false)
    }.toList.toSet.mkString(",\n         ").concat(",")

    // Only supporting query params for now. Assuming path parameters in CRUD endpoints will only be id. Not supporting header parameters yet.
    val requestMaps = supportedEndpoints.filter {
      endpoint => endpoint.parameters.map(_.paramType).contains(SumoTerraformSupportedParameterTypes.QueryParameter)
    }.map {
      endpoint =>
        "\"" + s"${endpoint.httpMethod.toLowerCase}_request_map" + "\"" + s": {\n           Type: schema.TypeMap,\n          Optional: true,\n           Elem: &schema.Schema{\n            Type: schema.TypeString,\n            },\n         }"
    }.mkString(",\n         ").concat(",")



    s"""func resourceSumologic${sumoSwaggerClassName.capitalize}() *schema.Resource {
       |    return &schema.Resource{
       |      $funcMappings
       |      Importer: &schema.ResourceImporter{
       |        State: schema.ImportStatePassthrough,
       |      },
       |
      |       Schema: map[string]*schema.Schema{
       |        $propsObjects
       |        $requestMaps
       |    },
       |  }
       |}""".stripMargin
  }

}
