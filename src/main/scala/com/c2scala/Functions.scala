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
  val contents = ListBuffer[String]()
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
    parameters.clear
    
    super.visitFunctionDefinition(ctx)

    val params = if (!parameters.isEmpty) parameters.map(_.toString).reduce(_ + ", " + _) else ""
    var result = "def " + functionName + "(" + params + "): " + returnType + " = {"
    val contentStrings = contents.map{content => content.trim}
    if (contentStrings.size > 1)
      result += contentStrings.reduce(_ + "; " + _) // separate with semicolon for now
    else if (contentStrings.size == 1)
      result += contents(0).trim
    result += "}"
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
  
  override def visitStatement(ctx: CParser.StatementContext) = {
    contents += new StatementConverter(cTypes).visit(ctx).statement
    Nil
  }
  
  override def visitDeclaration(ctx: CParser.DeclarationContext) = {
    val blah = new DeclarationConverter(cTypes)
    blah.visitDeclaration(ctx)
    contents ++= blah.results.toList
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
    if (!isWithinParameters) {
      functionName = ctx.getText
    }
    super.visitDirectDeclarator(ctx)
  }
}