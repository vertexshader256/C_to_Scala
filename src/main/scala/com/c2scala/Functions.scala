package com.c2scala

import scala.collection.mutable.ListBuffer

case class Parameter(name: String, paramType: String) {
  override def toString = name + ": " + paramType
}

class FunctionConverter extends ChainListener {
  var returnType = ""
  var functionName = ""
  var isWithinParameters = false
  val parameters = ListBuffer[Parameter]()
  
  
  override def enterFunctionDefinition(ctx: CParser.FunctionDefinitionContext) = {
    println("FUNCTION ENTERED: " + ctx.getText)
    parameters.clear
  }
  
  override def enterTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
    if (!isWithinParameters) {
      returnType = convertTypeSpecifier(ctx.getText)
    }
  }
  
  override def enterParameterDeclaration(ctx: CParser.ParameterDeclarationContext) = {
    isWithinParameters = true
  }
  
  override def exitParameterDeclaration(ctx: CParser.ParameterDeclarationContext) = {
    isWithinParameters = false
    parameters += Parameter(ctx.declarator().getText, convertTypeSpecifier(ctx.declarationSpecifiers().getText))
  }
  
  override def enterDirectDeclarator(ctx: CParser.DirectDeclaratorContext) = {
    if (ctx.directDeclarator() == null) {
      functionName = ctx.getText
    }
  }
  
  override def exitFunctionDefinition(ctx: CParser.FunctionDefinitionContext) = {
    
    println("FUNCTION EXITED")
    //println(ctx.getText)
    val result = "def " + functionName + "(" + parameters.map(_.toString).foldLeft("")(_ + ", " + _) + "): " + returnType + " = {}"
    println(result)
    results += result
  }
}