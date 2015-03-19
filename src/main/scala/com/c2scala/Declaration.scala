package com.c2scala

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.util.Try

class DeclarationConverter(cTypes: HashMap[String, String], outputFunctionContents: Boolean) extends ChainListener[Unit](cTypes) {

  val typedefNames = ListBuffer[String]()
  val directDeclarators = ListBuffer[String]()
  val explicitInitValues = ListBuffer[String]()
  var isFunctionPrototype = false
  var typeQualifier = ""
  var latestStorageSpecifier = ""
  var typeName = ""
  var varName = ""
  var islatestStructDecArray = false
  var latestStructDecName = ""
  var latestArraySize = ""
  var currentTypeSpec: CParser.TypeSpecifierContext = null
  var specifierQualifierLevel = 0
  
  def typedefLookahead(ctx: CParser.DeclarationContext): Boolean = {
    val checkTypedef = Try(ctx.declarationSpecifiers().declarationSpecifier().get(0).getText == "typedef")
    checkTypedef.getOrElse(false)
  }
  
  override def visitDeclaration(ctx: CParser.DeclarationContext) = {
    latestStorageSpecifier = ""
    typeQualifier = ""
    typedefNames.clear
    directDeclarators.clear
    explicitInitValues.clear
      
    val isTypedef = typedefLookahead(ctx)
    
    if (!isTypedef) {
      super.visitDeclaration(ctx)
    } else {
      val typedefConverter = new TypedefConverter(cTypes)
      typedefConverter.visit(ctx)
      results ++= typedefConverter.results
    }
    
    if (!isTypedef && (latestStorageSpecifier == "" || latestStorageSpecifier == "static")  && !isFunctionPrototype) {
      
      val scope = if (latestStorageSpecifier == "static") "private" else ""
      val qualifier = scope + " " + (if (typeQualifier == "const") "val" else "var")
      
      if (directDeclarators.size > 1) {
        val dirTypeName = if (!typedefNames.isEmpty) typedefNames(0) else typeName
        val decl = "(" + directDeclarators.map(_ + ": " + dirTypeName).reduce(_ + ", " + _) + ")"
        val baseTypeDefault = getDefault(cTypes, dirTypeName)
        val defaults: String = "(" + directDeclarators.zipWithIndex.map{ case (decl, index) =>
          postProcessValue(if (index < explicitInitValues.size) {
            explicitInitValues(index)
          } else {
            baseTypeDefault
          }, typeName)
        }.reduce(_ + ", " + _) + ")"
        results += qualifier + " " + decl + " = " + defaults + "\n"
      } else if ((typedefNames.size <= 2 || directDeclarators.size == 1) && typeName != "") {
        val baseTypeDefault = getDefault(cTypes, typeName)
        val default = if (!explicitInitValues.isEmpty) {
            explicitInitValues(0)
          } else {
            baseTypeDefault
          } 
        results += qualifier + " " + varName + ": " + typeName + " = " + postProcessValue(default, typeName) + "\n"
      } else {
        parseSimpleDecl()
      }
    } 
  }
  
  override def visitSpecifierQualifierList(ctx: CParser.SpecifierQualifierListContext) = {
    specifierQualifierLevel += 1
    super.visitSpecifierQualifierList(ctx)
    specifierQualifierLevel -= 1
  }
  
  override def visitPrimaryExpression(ctx: CParser.PrimaryExpressionContext) = {
    super.visitPrimaryExpression(ctx)
    if (ctx.expression() == null) { // is this the bottom of the tree?!
      latestArraySize = if (ctx.getText.contains("0x")) {
        Integer.getInteger(ctx.getText.drop(2), 16).toString
      } else {
        ctx.getText
      }
    }
  }
  
  def parseSimpleDecl() = {
    if (islatestStructDecArray && latestArraySize != "") {
        results += "var " + varName + ": Array[" + translateTypeSpec(currentTypeSpec) + "]" + " = Array.fill(" + latestArraySize + ")(" + getDefault(cTypes, currentTypeSpec.getText) + ")"
    } else if (islatestStructDecArray && latestArraySize == "") {
        results += "var " + varName + ": Array[" + translateTypeSpec(currentTypeSpec) + "]" + " = null"
    } else if (currentTypeSpec != null) {
        val baseTypeDefault = postProcessValue(getDefault(cTypes, typeName), typeName)
        results += "var " + convertTypeName(latestStructDecName, typeName) + ": " + typeName + " = " + baseTypeDefault
    }
  }
  
  
  override def visitStructDeclaration(ctx: CParser.StructDeclarationContext) = {
    latestStructDecName = ""
    islatestStructDecArray = false
    super.visitStructDeclaration(ctx)
    parseSimpleDecl()
  }
  
  override def visitTypeQualifier(ctx: CParser.TypeQualifierContext) = {
    typeQualifier =  ctx.getText
  }
  
  override def visitParameterTypeList(ctx: CParser.ParameterTypeListContext) = {
    isFunctionPrototype = true
  }
  
  override def visitInitDeclaratorList(ctx: CParser.InitDeclaratorListContext) = {
    directDeclarators.clear
    super.visitInitDeclaratorList(ctx)
  }
  
  override def visitInitializer(ctx: CParser.InitializerContext) = {
    explicitInitValues += ctx.getText
  }
  
  override def visitDirectDeclarator(ctx: CParser.DirectDeclaratorContext) = {
    islatestStructDecArray = true
    if (!ctx.getParent.isInstanceOf[CParser.DirectDeclaratorContext]) {
      directDeclarators += ctx.getText
    }
    varName = ctx.getText
    super.visitDirectDeclarator(ctx)
  }
     
  override def visitTypedefName(ctx: CParser.TypedefNameContext) = {
    latestStructDecName = ctx.Identifier().getText
    if (typedefNames.size == 1) {
      typeName = typedefNames(0)
    }
    typedefNames += ctx.Identifier().getText
    varName = ctx.Identifier().getText
  }
    
  override def visitTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
    if (specifierQualifierLevel <= 1) {
      currentTypeSpec = ctx
    } 
    
    if (ctx.typedefName() == null) {
      typeName = translateTypeSpec(ctx)
    } else {
      super.visitTypeSpecifier(ctx)
    }
  }
  
  override def visitFunctionDefinition(ctx: CParser.FunctionDefinitionContext) = {
    results ++= new FunctionConverter(cTypes, outputFunctionContents).visit(ctx)
  }
 
  override def visitStorageClassSpecifier(ctx: CParser.StorageClassSpecifierContext) = {
    latestStorageSpecifier = ctx.getText
  }

}