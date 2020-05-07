package com.sumologic.terraform_generator.objects

import com.sumologic.terraform_generator.StringHelper

trait SumoSwaggerEndpointHelper extends StringHelper {
  def bodyParamToArgList(bodyParamOption: Option[ScalaSwaggerParameter]): List[String] = {
    bodyParamOption match {
      case Some(bodyParam) =>
        val requestBodyType = bodyParam.param.getType().name
        List[String](lowerCaseFirstLetter(requestBodyType) + " " + requestBodyType)
      case None => List[String]()
    }
  }

  def paramListToArgList(params: List[ScalaSwaggerParameter]): List[String] = {
    params.map(_.toTerraformFuncArg())
  }

  def makeArgsListForDecl(params: List[ScalaSwaggerParameter]): String = {
    val allParams = getArgsListForDecl(params)
    allParams.mkString(", ")
  }

  def getArgsListForDecl(params: List[ScalaSwaggerParameter]): List[String] = {
    // TODO val queryParamList = params.filter(_.paramType == SumoTerraformSupportedParameterTypes.QueryParameter)

    val hasParams = params.map(_.paramType).exists { paramType =>
      paramType.equals(TerraformSupportedParameterTypes.QueryParameter) ||
          paramType.equals(TerraformSupportedParameterTypes.HeaderParameter)
    }
    val requestMap = if (hasParams) {
      List("paramMap map[string]string")
    } else {
      List.empty[String]
    }

    // TODO Need to consider required params
    val pathParamList = params.filter(
      _.paramType == TerraformSupportedParameterTypes.PathParameter)

    val bodyParamOpt = params.find(_.paramType == TerraformSupportedParameterTypes.BodyParameter)

    if (bodyParamOpt.isDefined) {
      bodyParamToArgList(bodyParamOpt) ++ requestMap
    } else {
      paramListToArgList(pathParamList) ++ requestMap
    }
  }
}
