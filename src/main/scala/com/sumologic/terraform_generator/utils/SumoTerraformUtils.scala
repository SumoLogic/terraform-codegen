package com.sumologic.terraform_generator.utils

import com.sumologic.terraform_generator.TerraformGeneratorHelper
import com.sumologic.terraform_generator.objects.{SumoSwaggerEndpoint, SumoSwaggerObject, SumoSwaggerObjectArray, SumoSwaggerObjectSingle, SumoSwaggerParameter, SumoSwaggerResponse, SumoSwaggerTemplate, SumoSwaggerType}
import com.sumologic.terraform_generator.objects.SumoSwaggerSupportedOperations.crud
import com.sumologic.terraform_generator.utils.SumoTerraformPrinter.{freakOut, maybePrint}
import io.swagger.v3.oas.models.PathItem.HttpMethod
import io.swagger.v3.oas.models.media.{ArraySchema, ComposedSchema, Schema}
import io.swagger.v3.oas.models.parameters.{Parameter, PathParameter, QueryParameter}
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.{OpenAPI, Operation, PathItem}

import scala.collection.JavaConverters._

object SumoTerraformPrinter {
  final val shouldPrint = false

  def freakOut(msg: String): Unit = {
    println("###### Freaking out about [" + msg + "]\n")
    println("###### Freaking out about [" + msg + "]\n")
    println("###### Freaking out about [" + msg + "]\n")
    println("###### Freaking out about [" + msg + "]\n")
    println("###### Freaking out about [" + msg + "]\n")
    println("###### Freaking out about [" + msg + "]\n")
    println("###### Freaking out about [" + msg + "]\n")
  }

  def maybePrint(txt: String): Unit = {
    if (shouldPrint) {
      println(txt)
    }
  }
}

object SumoTerraformSchemaTypes {
  final val TypeBool = "schema.TypeBool"
  final val TypeInt = "schema.TypeInt"
  final val TypeFloat = "schema.TypeFloat"
  final val TypeString = "schema.TypeString"
  // Date is represented using TypeString
  final val TypeMap = "schema.TypeMap"
  final val TypeList = "schema.TypeList"
  final val TypeSet = "schema.TypeSet"

  def swaggerTypeToTerraformSchemaType(swaggerType: String): String = {
    swaggerType match {
      case "string" => TypeString
      case "int" => TypeInt
      case "bool" => TypeBool
      case "float" => TypeFloat // TODO does float exist in swagger?
      case "array" => TypeList
      case _ => TypeMap // TODO this generalization is way too naiive
      //TODO Date type? translates to TypeString
    }
  }

  def swaggerTypeToGoType(swaggerType: String): String = {
    swaggerType match {
      case "boolean" => "bool"
      case "object" => "map[string]string"
      case _ => swaggerType
    }
  }
}

object SumoTerraformSupportedParameterTypes {
  final val PathParameter = "PathParameter"
  final val BodyParameter = "BodyParameter"
  final val QueryParameter = "QueryParameter"
}

object SumoTerraformSupportedPrintfTypes {
  final val StringFormat = "string"
  final val IntFormat = "int"

  def toPrintfType(formatType: String): String = {
    formatType match {
      case StringFormat => "%s"
      case IntFormat => "%d"
      case _ => throw new RuntimeException(s"Unsupported Printf Type ($formatType)")
    }
  }
}

object SumoTerraformUtils extends TerraformGeneratorHelper {
  val packageName = "sumologic"

  def varNameFromTypeName(typeName: String): String = {
    typeName.substring(0, 1).toLowerCase() + typeName.substring(1)
  }

  def httpMethodToTerraformHttpClientFunctionName(httpMethod: String): String = {
    httpMethod.substring(0, 1).toUpperCase() + httpMethod.substring(1).toLowerCase
  }

  def getTaggedComponents(openAPI: OpenAPI): Map[String, Schema[_]] = {
    openAPI.getComponents.getSchemas.asScala.toList.filter {
      schema => schema._2.getExtensions != null
    }.toMap
  }

  def getTaggedProperties(openAPI: OpenAPI, model: Schema[_]): List[(String, Schema[_])] = {
    model.getProperties.asScala.toList.filter {
      prop => prop._2.getExtensions == null || prop._2.getExtensions.asScala.toList.map(_._1).contains("writeonly") ||
        prop._2.getExtensions.asScala.toList.map(_._1).contains("id")
    }
  }

  def getOperationTypeFromOperationId(operationId: String): String = {
    val operationTypeOpt = crud.filter(operationId.toLowerCase.contains(_)).headOption
    operationTypeOpt match {
      case Some(op) => op
      case _ => throw new RuntimeException(s"Unsupported Operation Type ($operationId), not matching any of: $crud")
    }
  }

  // TODO we need a query param version of this as well
  def pathParamsToPrintfFormat(path: String, pathParamList: List[SumoSwaggerParameter]): (String, String) = {
    // maybePrint(s" pathParamsToPrintfFormat called with $path and $pathParamList")
    if (pathParamList.isEmpty) {
      (path, "")
    } else {
      val (firstList, restOfParamList) = pathParamList.splitAt(1)
      val firstParam = firstList.head.param
      val formattedPath = path.replaceFirst(s"\\{${firstParam.getName}\\}",
        SumoTerraformSupportedPrintfTypes.toPrintfType(firstParam.getType().name))
      val (newPath, newArgs) = pathParamsToPrintfFormat(formattedPath, restOfParamList)
      (newPath, s", ${firstParam.getName}" + newArgs)
    }
  }


  def bodyParamToArgList(bodyParamOption: Option[SumoSwaggerParameter]): List[String] = {
    bodyParamOption match {
      case Some(bodyParam) =>
        val requestBodyType = bodyParam.param.getType().name
        List[String](varNameFromTypeName(requestBodyType) + " " + requestBodyType)
      case None => List[String]()
    }
  }

  def paramListToArgList(params: List[SumoSwaggerParameter]): List[String] = {
    params.map(_.toTerraformFuncArg())
  }

  def makeArgsListForDecl(params: List[SumoSwaggerParameter]): String = {
    val allParams = getArgsListForDecl(params)
    allParams.mkString(", ")
  }

  def getArgsListForDecl(params: List[SumoSwaggerParameter]): List[String] = {
    // TODO val queryParamList = params.filter(_.paramType == SumoTerraformSupportedParameterTypes.QueryParameter)
    // TODO Need to consider required params
    val pathParamList = params.filter(
      _.paramType == SumoTerraformSupportedParameterTypes.PathParameter)

    val bodyParamOpt = params.find(_.paramType == SumoTerraformSupportedParameterTypes.BodyParameter)

    paramListToArgList(pathParamList) ++ bodyParamToArgList(bodyParamOpt)
  }

  def getArgsListForFuncCall(params: List[SumoSwaggerParameter]): List[String] = {
    // TODO add QueryParams too
    val pathParamList = params.filter(_.paramType == SumoTerraformSupportedParameterTypes.PathParameter)
    val bodyParamList = params.filter(_.paramType == SumoTerraformSupportedParameterTypes.BodyParameter)
    //.getOrElse(List[SumoSwaggerParameter]())
    val allParams = pathParamList ++ bodyParamList

    allParams.map {
      param: SumoSwaggerParameter =>
        if (param.param.getType().isCompositeType()) {
          varNameFromTypeName(param.param.getType().name)
        } else {
          param.param.getName()
        }
    }
  }

  def getArgsListAsVariableAssignmentsFromDataSourceSchema(params: List[SumoSwaggerParameter]): List[String] = {
    // TODO add QueryParams too
    val pathParamList = params.filter(_.paramType == SumoTerraformSupportedParameterTypes.PathParameter)
    val bodyParamList = params.filter(_.paramType == SumoTerraformSupportedParameterTypes.BodyParameter)
    //.getOrElse(List[SumoSwaggerParameter]())
    val allParams = pathParamList ++ bodyParamList
    allParams.map {
      param: SumoSwaggerParameter =>
        if (param.param.getType().isCompositeType()) {
          val callSite = getTerraformResourceDataToObjectConverterFuncCall(param.param.getType())
          val varName = varNameFromTypeName(param.param.getType().name)
          s"$varName = $callSite"
        } else {
          getTerraformDataSourceValueSetCheck(param.param.getName(), singleReturn = true)
        }
    }
  }

  def makeTerraformHttpClientRequestString(httpMethod: String, bodyParamOption: Option[SumoSwaggerParameter]): String = {
    val httpClientFunc = httpMethodToTerraformHttpClientFunctionName(httpMethod)
    bodyParamOption match {
      case Some(bodyParam) =>
        val requestBodyType = bodyParam.param.getType().name
        httpClientFunc + "(url, " + varNameFromTypeName(requestBodyType) + ")"
      case None =>
        httpClientFunc + "(url)"
    }
  }

  def resolvePropertyType(openApi: OpenAPI, property: Schema[_]): SumoSwaggerType = {
    if (property.get$ref() != null) {
      val model = getTaggedComponents(openApi).get(property.get$ref()).get
      processModel(openApi, property.get$ref(), model)
    } else {
      SumoSwaggerType(SumoTerraformSchemaTypes.swaggerTypeToGoType(property.getType))
    }
  }

  def makeTerraformSchema(sumoSwaggerObject: SumoSwaggerObject): String = {
    "//TODO"
  }

  def makeTerraformUrlFormatForSprintf(path: String, params: List[SumoSwaggerParameter]): String = {
    // TODO val queryParamList = params.filter(_.paramType == SumoTerraformSupportedParameterTypes.QueryParameter)
    // TODO Need to consider required params
    val pathParamList = params.filter(
      _.paramType == SumoTerraformSupportedParameterTypes.PathParameter)
    val (printfFormattedPath, printfArgs) = SumoTerraformUtils.pathParamsToPrintfFormat(
      path, pathParamList)

    s""""$printfFormattedPath"$printfArgs""".stripMargin
  }

  // TODO IMPORTANT TO DO LATER IS THAT WHEN REFERENCE OBJECTS ARE CALCULATED, WE SHOULD PUT THEM IN A MAP
  // TO BE USED AS A CACHE, BECAUSE THESE GET RECALCULATED WHEN RESPONSE OBJECTS ARE IDENTICAL AS IN GET/LIST
  def processResponseObjects(openApi: OpenAPI, responses: Map[String, ApiResponse]): List[SumoSwaggerResponse] = {

    responses.map {
      case (respName: String, respObj: ApiResponse) =>
        if (respObj.getContent.get(respName).getSchema != null) {
          Option(respObj.getContent.get(respName).getSchema.get$ref()) match {
            case Some(ref) =>
              if (getTaggedComponents(openApi).get(ref).isDefined) {
                val swaggerType = processModel(openApi, ref, getTaggedComponents(openApi).get(ref).get)
                SumoSwaggerResponse(respName, Some(swaggerType))
              } else {
                null
              }
            case _ =>
              freakOut(s"*** Response  => $respName WTF?????? Response Schema Type ")
              throw new RuntimeException("This should not happen in processResponseObjects ")
          }
        } else {
          // No response body
          SumoSwaggerResponse(respName, None)
        }
    }.toList
  }

  def processPathParameter(openApi: OpenAPI, pathParam: PathParameter): SumoSwaggerParameter = {
    // PathParameter is of type SerializableParameter
    maybePrint(s"Path Param => ${pathParam.getName} type(${pathParam.getSchema.getType})" +
      s" required(${pathParam.getRequired}) class(${pathParam.getClass}) " +
      s"default(${pathParam.getSchema.getDefault})")
    SumoSwaggerParameter(SumoTerraformSupportedParameterTypes.PathParameter,
      SumoSwaggerObjectSingle(pathParam.getName,
        SumoSwaggerType(pathParam.getSchema.getType, List[SumoSwaggerObject]()),
        pathParam.getRequired, Some(pathParam.getSchema.getDefault.asInstanceOf[AnyRef])))
  }

  def processQueryParameter(openApi: OpenAPI, queryParam: QueryParameter): SumoSwaggerParameter = {
    // QueryParameter is of type SerializableParameter
    maybePrint(s"Query Param => ${queryParam.getName} type(${queryParam.getSchema.getType})" +
      s" required(${queryParam.getRequired}) class(${queryParam.getClass}) " +
      s"default(${queryParam.getSchema.getDefault})")
    SumoSwaggerParameter(SumoTerraformSupportedParameterTypes.QueryParameter,
      SumoSwaggerObjectSingle(queryParam.getName,
        SumoSwaggerType(queryParam.getSchema.getType, List[SumoSwaggerObject]()),
        queryParam.getRequired, Some(queryParam.getSchema.getDefault.asInstanceOf[AnyRef])))
  }

  def processBodyParameter(openApi: OpenAPI, bodyParam: Parameter): SumoSwaggerParameter = {
    // Body is of Type AbstractParameter
    maybePrint(s"BodyParameter param => ${bodyParam.getName} ${bodyParam.getSchema.get$ref()}" +
      s" required(${bodyParam.getRequired}) class(${bodyParam.getClass}) ")

    val defName: String = bodyParam.getSchema.get$ref().split("#/components/schemas/").last
    val modelOpt = getTaggedComponents(openApi).get(defName)
    modelOpt match {
      case Some(model) =>
        //maybePrint(s" #### DEF ($defName)  => ${model.getProperties}")
        val swaggerType = processModel(openApi, defName, model)
        //swaggerType.terraformify()
        SumoSwaggerParameter(SumoTerraformSupportedParameterTypes.BodyParameter,
          SumoSwaggerObjectSingle(defName, swaggerType, bodyParam.getRequired(), None))
      case None =>
        freakOut("processBodyParameter " + bodyParam.getName)
        throw new RuntimeException("This should not happen in processBodyParameter ")
    }
  }

  def processComposedModel(openApi: OpenAPI, modelDefName: String, composedModel: ComposedSchema): SumoSwaggerType = {
    val parts: List[SumoSwaggerObject] = composedModel.getAllOf.asScala.flatMap {
      case model: Schema[_] =>
        if (model.get$ref() != null) {
          maybePrint("composedModel GETTING THE REFERENCE FROM COMPOSED PROP " + model)
          processModel(openApi, model.get$ref(), model).props
        } else {
          if (model.getProperties != null) {
            getTaggedProperties(openApi, model).map {
              propTuple =>
                processModelProperty(openApi, propTuple._1, propTuple._2, propTuple._2.getRequired.asScala.toList)
            }
          } else {
            List[SumoSwaggerObject]()
          }
        }
    }.toList
    //processModel(swagger, model.getProperties.values().asScala, model).props

    SumoSwaggerType(modelDefName, parts)
  }

  def processModel(openApi: OpenAPI, modelDefName: String, model: Schema[_]): SumoSwaggerType = {
    maybePrint("MODEL FOR " + modelDefName + " IS " + model)

    model match {
      case arrayModel: ArraySchema =>
        //freakOut("arrayModel => " + arrayModel)
        SumoSwaggerType(modelDefName, List[SumoSwaggerObject]())
      case composedModel: ComposedSchema =>
        //freakOut("composedModel => " + composedModel)
        processComposedModel(openApi, modelDefName, composedModel)
      case _ =>
        if (model.get$ref() != null) {
          //freakOut("REF MODEL FOR " + refModel + " IS " + refModel.getSimpleRef + " props " + refModel.getProperties)
          val resolvedRefModel = getTaggedComponents(openApi).get(model.get$ref()).get
          //freakOut("CHANGING IT UP TO RESOLVED REF " + resolvedRefModel)
          processModel(openApi, modelDefName, resolvedRefModel)
        } else if (model.get$ref() == null) {
          val props: List[SumoSwaggerObject] = getTaggedProperties(openApi, model).map {
            propTuple =>
              processModelProperty(openApi, propTuple._1, propTuple._2, propTuple._2.getRequired.asScala.toList)
          }
          SumoSwaggerType(modelDefName, props)
        } else {
          //freakOut("DONT KNOW WHAT THIS IS " + modelDefName)
          SumoSwaggerType(modelDefName, List[SumoSwaggerObject]())
        }
    }

  }

  def processModelProperty(openApi: OpenAPI, propName: String, prop: Schema[_], requiredProps: List[String]): SumoSwaggerObject = {
    //maybePrint(s" #### PROP ($propName)  => ${prop.getTitle} (${prop.getType})")

    prop match {
      case arrayProp: ArraySchema =>
        maybePrint(s" #### ARRAY PROP ($propName)  => ${prop.getTitle} (${prop.getType} ${arrayProp.getItems.getType})")
        SumoSwaggerObjectArray(propName, resolvePropertyType(openApi, arrayProp.getItems), requiredProps.contains(arrayProp.getName), None)
      case _ =>
        if (prop.get$ref() != null) {
          val refModel = getTaggedComponents(openApi).get(prop.get$ref()).get
          SumoSwaggerObjectSingle(propName, processModel(openApi, propName, refModel), requiredProps.contains(prop.getName), None)
        } else {
          maybePrint(s" #### PROP ($propName)  => ${prop.getTitle} (${prop.getType})")
          // TODO Get Default, will need to see what type it is
          SumoSwaggerObjectSingle(propName, resolvePropertyType(openApi, prop),requiredProps.contains(prop.getName), None)
        }
    }
  }

  def processOperation(openApi: OpenAPI, baseType: String, operation: Operation, pathName: String, method: HttpMethod): SumoSwaggerEndpoint = {
    val operationPath = s"#${method.toString} => $pathName"
    maybePrint(s"OPERATION $operationPath " + operation.getOperationId)

    val responses = processResponseObjects(openApi, operation.getResponses.asScala.toMap)

    maybePrint("OPERATION PARAMETERS: \n") // + operation.getParameters.asScala)
    val params: List[SumoSwaggerParameter] = operation.getParameters.asScala.map { param: Parameter =>
      param match {
        case pathParam: PathParameter =>
          processPathParameter(openApi, pathParam)
        case queryParam: QueryParameter =>
          processQueryParameter(openApi, queryParam)
        case _ =>
          if (param.getIn == null && param.getContent != null) {
            processBodyParameter(openApi, _)
          }
          freakOut("WTF??????WTF??????WTF??????WTF?????? => " + param)
          throw new RuntimeException("This should not happen in processOperation ")
      }
    }.toList

    SumoSwaggerEndpoint(baseType, operation.getOperationId, pathName, method.name(), params, responses)
  }

  def processPath(openApi: OpenAPI, baseType: String, path: PathItem, pathName: String):
  List[SumoSwaggerEndpoint] = {
    val operationMap = Map(
      HttpMethod.GET -> path.getGet,
      HttpMethod.POST -> path.getPost,
      HttpMethod.DELETE -> path.getDelete,
      HttpMethod.PUT -> path.getPut
    )
    val filteredOps = operationMap.filter {
      methodOperationTuple =>
        methodOperationTuple._2 != null && methodOperationTuple._2.getExtensions != null
    }

    filteredOps.map {
      case (method: HttpMethod, operation: Operation) =>
        processOperation(openApi, baseType, operation, pathName, method)
    }.toList
  }

  def processClass(openApi: OpenAPI, baseType: String): SumoSwaggerTemplate = {
    // SumoSwaggerTemplate(sumoSwaggerClassName: String, supportedEndpoints: List[SumoSwaggerEndpoint])

    val filteredPaths = openApi.getPaths.asScala.filter {
      case (pathName: String, path: PathItem) =>
        val postExtensions = if (path.getPost != null && path.getPost.getExtensions != null) {
          path.getPost.getExtensions.asScala.toList
        } else List.empty[(String, AnyRef)]
        val getExtensions = if (path.getGet != null && path.getGet.getExtensions != null) {
          path.getGet.getExtensions.asScala.toList
        } else List.empty[(String, AnyRef)]
        val putExtensions = if (path.getPut != null && path.getPut.getExtensions != null) {
            path.getPut.getExtensions.asScala.toList
        } else List.empty[(String, AnyRef)]
        val deleteExtensions = if (path.getDelete != null && path.getDelete.getExtensions != null) {
          path.getDelete.getExtensions.asScala.toList
        } else List.empty[(String, AnyRef)]
        val vendorExtensions = postExtensions ++ getExtensions ++ putExtensions ++ deleteExtensions

        (!vendorExtensions.isEmpty) &&
        (pathName.toLowerCase.contains(baseType.toLowerCase + "s")  ||
        pathName.replaceAll("Model", "").toLowerCase.contains(baseType.toLowerCase + "s"))
    }

    freakOut(s"THOMASKAO FILTEREDPATHS: ${filteredPaths.map(_._1).toList.toString()}")

    val endpoints = filteredPaths.flatMap {
      case (pathName: String, path: PathItem) => processPath(openApi, baseType, path, pathName)
    }.toList

    SumoSwaggerTemplate(baseType, endpoints)
  }

  def seperatorLine(title: String = ""): String = {
    s"\n\n// -----------------------------$title-----------------------------//\n\n"
  }

  def getTerraformResourceSetters(propName: String, objName: String): String = {
    val noCamelCaseName = removeCamelCase(propName)
    s"""resourceData.Set("${noCamelCaseName.toLowerCase}", $objName.${propName.capitalize})""".stripMargin
  }

  def getTerraformResourceGetters(prop: SumoSwaggerObject): String = {
    val propName = prop.getName()
    val noCamelCaseName = removeCamelCase(propName)
    val propType = prop.getType().name
    prop match {
      case arrayProp: SumoSwaggerObjectArray =>
        s"""${propName.capitalize}: ${propName.toLowerCase},""".stripMargin
      case singleProp: SumoSwaggerObject =>
        s"""${propName.capitalize}: resourceData.Get(\"${noCamelCaseName.toLowerCase}\").($propType),""".stripMargin
    }
  }

  def getTerraformResourceValidators(objClass: SumoSwaggerType): String = {
    objClass.props.map {
      prop: SumoSwaggerObject =>
        getTerraformResourceValidator(prop, objClass.name)
    }.mkString("")
  }

  def getTerraformDataSourceValueSetCheck(varName: String,
                                          varTypeOpt: Option[String] = None,
                                          singleReturn: Boolean = false): String = {
    val singleReturnTxt = singleReturn match {
      case false => "nil, "
      case true => ""
    }
    val typeInfo = varTypeOpt match {
      case Some(varType) => s" to construct $varType"
      case None => ""
    }
    val noCamelCaseName = removeCamelCase(varName)
    if (varName.toLowerCase.contains("id")) {
      /* s"""${varName}Exists, ok := resourceData.Id();
         |if !ok {
         |    return ${singleReturnTxt}fmt.Errorf("SumologicTerraformError: %q is required${typeInfo}.", ${varName}Exists)
         |  }\n""".stripMargin */
      ""
    } else {
      s"""${varName}Exists, ok := resourceData.GetOkExists("$noCamelCaseName");
         |if !ok {
         |    return ${singleReturnTxt}fmt.Errorf("SumologicTerraformError: %q is required${typeInfo}.", ${varName}Exists)
         |  }\n""".stripMargin
    }
  }

  def getTerraformResourceValidator(prop: SumoSwaggerObject, typeName: String): String = {
    prop.getRequired() match {
      case true => getTerraformDataSourceValueSetCheck(prop.getName(), Option(prop.getType().name))
      case false => ""
    }
  }

  def getTerraformObjectToResourceDataConverterFuncCall(objClass: SumoSwaggerType): String = {
    val objName = varNameFromTypeName(objClass.name)
    s"${objName}ToResourceData(resourceData, $objName)"
  }

  def getTerraformObjectToResourceDataConverter(objClass: SumoSwaggerType): String = {
    val className = objClass.name
    val objName = varNameFromTypeName(objClass.name)

    val setters = objClass.props.map {
      prop: SumoSwaggerObject =>
        getTerraformResourceSetters(prop.getName(), objName)
    }.mkString("\n")

    s"""func ${objName}ToResourceData(resourceData *schema.ResourceData, $objName *$className) {
       |   $setters
       | }""".stripMargin

  }

  def getTerraformResourceDataToObjectConverterFuncName(objClass: SumoSwaggerType): String = {
    s"resourceDataTo${objClass.name}"
  }

  def getTerraformResourceDataToObjectConverterFuncCall(objClass: SumoSwaggerType): String = {
    val funcName = getTerraformResourceDataToObjectConverterFuncName(objClass)
    s"$funcName(resourceData)"
  }

  def getTerraformResourceDataToObjectConverter(objClass: SumoSwaggerType, skipValidators: Boolean): String = {
    val className = objClass.name

    val validators = if (skipValidators) {
      ""
    } else {
      getTerraformResourceValidators(objClass)
    }

    val arrayGetters = objClass.props.filter(_.isInstanceOf[SumoSwaggerObjectArray]).map {
      prop =>
        val propName = prop.getName.toLowerCase
        val noCamelCaseName = removeCamelCase(propName)
        val idVar = s"${propName}Ids"
        s"""${idVar} := resourceData.Get("${noCamelCaseName}").([]interface{})
           |${propName} := make([]${prop.asInstanceOf[SumoSwaggerObjectArray].objType.name}, len(${idVar}))
           |for i, v := range ${idVar} {
           |  ${propName}[i] = fmt.Sprint(v)
           |}""".stripMargin
    }.mkString("\n")

    val getters = objClass.props.map {
      prop: SumoSwaggerObject =>
        getTerraformResourceGetters(prop)
    }.mkString("\n")

    val funcName = getTerraformResourceDataToObjectConverterFuncName(objClass)
    s"""func $funcName(resourceData *schema.ResourceData) (*$className, error) {
       |$validators
       |$arrayGetters
       |   return &$className{
       |     $getters
       |   }, nil
       | }""".stripMargin
  }

  def getTerraformFullObjectToPartialRequestObjectConverter(objClass: SumoSwaggerType,
                                                            partialRequestClass: SumoSwaggerType): String = {
    val className = objClass.name

    val getters = objClass.props.map {
      prop: SumoSwaggerObject =>
        getTerraformResourceGetters(prop)
    }.mkString("\n")

    val funcName = getTerraformResourceDataToObjectConverterFuncName(objClass)
    s"""func $funcName(resourceData *schema.ResourceData) $className {
       |   return $className{
       |     $getters
       |   }
       | }""".stripMargin
  }
}