package com.c2scala

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.util.Try

case class Parameter(name: String, paramType: String) {
  override def toString = name + ": " + paramType
}

class FunctionConverter(cTypes: HashMap[String, String], outputFunctionContents: Boolean) extends ChainListener[List[String]](cTypes) {
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
    if (outputFunctionContents) {
      val contentStrings = contents.map{content => content.trim}
      if (contentStrings.size > 1)
        result += contentStrings.reduce(_ + "; " + _) // separate with semicolon for now
      else if (contentStrings.size == 1)
        result += contents(0).trim
    } else if (returnType != "" && returnType != "Unit") {
      if (cTypes.contains(returnType)) {
        var baseType = cTypes(returnType)
        var good = baseType
        while (cTypes.contains(baseType)) {
          baseType = cTypes(baseType)
        }
        result += getTypeDefault(baseType)
      } else {
        result += getTypeDefault(returnType)
      }
    }
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
    val blah = new DeclarationConverter(cTypes, outputFunctionContents)
    blah.visitDeclaration(ctx)
    contents ++= blah.results.toList
    Nil
  }
  
  override def visitParameterDeclaration(ctx: CParser.ParameterDeclarationContext) = {
    isWithinParameters = true
    super.visitParameterDeclaration(ctx)
    isWithinParameters = false
     
    val hasPointer = Try(ctx.declarator.pointer != null).getOrElse(false)
    if(ctx.declarator != null) { // is not 'void'
      parameters += Parameter(ctx.declarator.directDeclarator.getText, translateTypeSpec(latestParamTypeSpec, hasPointer))
    }

    Nil
  }
   
  override def visitDirectDeclarator(ctx: CParser.DirectDeclaratorContext) = {
    if (!isWithinParameters) {
      functionName = ctx.getText
    }
    super.visitDirectDeclarator(ctx)
  }
}