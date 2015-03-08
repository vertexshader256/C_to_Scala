package com.c2scala

import scala.collection.mutable.ListBuffer

import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.Token
import scala.collection.mutable.HashMap

class CConverter(cTypes: HashMap[String, String]) extends ChainListener[String](cTypes) {
  var struct: Struct = null
  var currentTypeSpec: CParser.TypeSpecifierContext = null

  var isTypeEnum = false
  var isWithinFunction = false
  var hasTypedefName = false
  
  val typedefNames = ListBuffer[String]()
  var latestStorageSpecifier = ""
  var latestTypeSpec: CParser.TypeSpecifierContext = null
  var latestDirectDeclarator = ""
  
  var latestArraySize = 0
  var isArray = false
  val enumerations = ListBuffer[enumerator]()
  
  case class enumerator(constant: String, expression: String)
    
  override def visitEnumerator(ctx: CParser.EnumeratorContext) = {
    if (ctx.enumerationConstant() != null && ctx.constantExpression() != null) {
      enumerations += enumerator(ctx.enumerationConstant().getText, ctx.constantExpression().getText)
    }
    super.visitEnumerator(ctx)
    ""
  }
  
  override def visitInitDeclaratorList(ctx: CParser.InitDeclaratorListContext) = {
    isWithinFunction = true
    super.visitInitDeclaratorList(ctx)
    ""
  }
  
  override def visitDirectDeclarator(ctx: CParser.DirectDeclaratorContext) = {
    isArray = true
    latestDirectDeclarator = ctx.getText
    super.visitDirectDeclarator(ctx)
    ""
  }
  
  override def visitPrimaryExpression(ctx: CParser.PrimaryExpressionContext) = {
    super.visitPrimaryExpression(ctx)
    if (ctx.expression() == null) { // is this the bottom of the tree?!
      latestArraySize = if (ctx.getText.contains("0x")) {
        Integer.getInteger(ctx.getText.drop(2), 16)
      } else if (ctx.getText forall Character.isDigit) {
          ctx.getText.toInt
      } else {
        0
      }
    }
    ""
  }

  override def visitEnumSpecifier(ctx: CParser.EnumSpecifierContext) = {
    isTypeEnum = true
    super.visitEnumSpecifier(ctx)
    ""
  }
   
  override def visitTypedefName(ctx: CParser.TypedefNameContext) = {
    typedefNames += ctx.Identifier().getText
    
    hasTypedefName = true
    ""
  }
    
  override def visitTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
    hasTypedefName = false
    super.visitTypeSpecifier(ctx)
    if (!hasTypedefName) {
      latestTypeSpec = ctx
    }
    
    ""
  }
   
  
  override def visitFunctionDefinition(ctx: CParser.FunctionDefinitionContext) = {
    results ++= new FunctionConverter(cTypes).visitFunctionDefinition(ctx)
    super.visitFunctionDefinition(ctx)
  }

  override def visitDeclaration(ctx: CParser.DeclarationContext) = {
    latestStorageSpecifier = ""
    latestTypeSpec = null
    currentTypeSpec = null
    isTypeEnum = false
    isWithinFunction = false
    isArray = false
    typedefNames.clear
    enumerations.clear
    
    super.visitDeclaration(ctx)
    
     if (struct != null && !typedefNames.isEmpty) {
      var result = "class " + typedefNames(0) + " {\n"
      //structDeclarations.foreach(println)
      if (!struct.structDecl.isEmpty) {
        result += struct.structDecl.map("  " + _).reduce{_ + "\n" + _}
      }
      result += "\n}"
      results += result
    } else if (isArray && typedefNames.size == 1) {
      results += "type " + latestDirectDeclarator + " = Array[" + typedefNames(0) + "]\n"
    } else if (!isTypeEnum && !isWithinFunction && latestStorageSpecifier == "typedef") {
      if (typedefNames.size == 1) {
        results += "type " + typedefNames(0) + " = " + translateTypeSpec(latestTypeSpec) + "\n"
        cTypes += typedefNames(0) -> latestTypeSpec.getText
      } else if (typedefNames.size == 2) {
        results += "type " + typedefNames(1) + " = " + typedefNames(0) + "\n"
        cTypes += typedefNames(1) -> typedefNames(0)
      }
    } else if (!isTypeEnum && !isWithinFunction && latestStorageSpecifier == "") {
      if (typedefNames.size == 1) {
        val baseTypeDefault = getTypeDefault(cTypes.withDefaultValue(latestTypeSpec.getText)(latestTypeSpec.getText))
        results += "var " + typedefNames(0) + ": " + translateTypeSpec(latestTypeSpec) + " = " + baseTypeDefault + "\n"
      } else if (typedefNames.size == 2) {
        val baseTypeDefault = getTypeDefault(cTypes.withDefaultValue(typedefNames(1))(typedefNames(1)))
        results += "var " + typedefNames(1) + ": " + typedefNames(0) + " = " + baseTypeDefault + "\n"
      }
    } else if (isTypeEnum && !enumerations.isEmpty) {
      results += "type " + typedefNames(0) + " = Int"
      enumerations.foreach{enum =>
        results += ("val " + enum.constant + ": " + typedefNames(0) + " = " + enum.expression)
      }
    }
    
    ""
  }
  
  override def visitStructOrUnionSpecifier(ctx: CParser.StructOrUnionSpecifierContext) = {
    struct = new StructConverter(cTypes).visitStructOrUnionSpecifier(ctx)
    ""
  }
 
  override def visitStorageClassSpecifier(ctx: CParser.StorageClassSpecifierContext) = {
    //println("ENTERING TYPEDEF: " +ctx.getText)
    latestStorageSpecifier = ctx.getText
    ""
    //super.visitStorageClassSpecifier(ctx)
  }

}