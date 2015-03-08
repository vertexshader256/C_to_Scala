package com.c2scala

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
case class Parameter(name: String, paramType: String) {
  override def toString = name + ": " + paramType
}

class FunctionConverter(cTypes: HashMap[String, String]) extends ChainListener[List[String]](cTypes) {
  var returnType = ""
  var functionName = ""
  var isWithinParameters = false
  var latestParamTypeSpec: CParser.TypeSpecifierContext = null
  val parameters = ListBuffer[Parameter]()
  
  override def aggregateResult(aggregate: List[String], nextResult: List[String]): List[String] = {
    if (aggregate == null) {
        return nextResult
    }

    if (nextResult == null) {
        aggregate
    } else {
      aggregate ++ nextResult
    }
  }
  
  override def visitFunctionDefinition(ctx: CParser.FunctionDefinitionContext) = {
    println("FUNCTION ENTERED: " + ctx.getText)
    parameters.clear
    
    super.visitFunctionDefinition(ctx)
    
    println("FUNCTION EXITED")
    //println(ctx.getText)
    val result = "def " + functionName + "(" + parameters.map(_.toString).foldLeft("")(_ + ", " + _) + "): " + returnType + " = {}"
    println(result)
    results += result
    results.toList
  }
  
  override def visitTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
    if (!isWithinParameters) {
      returnType = translateTypeSpec(ctx)
    } else {
      latestParamTypeSpec = ctx
    }
    super.visitTypeSpecifier(ctx)
    Nil
  }
  
  override def visitParameterDeclaration(ctx: CParser.ParameterDeclarationContext) = {
    isWithinParameters = true
    super.visitParameterDeclaration(ctx)
      isWithinParameters = false
    parameters += Parameter(ctx.declarator().getText, translateTypeSpec(latestParamTypeSpec))
    Nil
  }
   
  override def visitDirectDeclarator(ctx: CParser.DirectDeclaratorContext) = {
    if (ctx.directDeclarator() == null) {
      functionName = ctx.getText
    }
    super.visitDirectDeclarator(ctx)
  }
}