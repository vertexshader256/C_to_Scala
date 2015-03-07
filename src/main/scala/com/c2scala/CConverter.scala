package com.c2scala

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.Token

class CConverter extends ChainListener {
  var isWithinStruct = false
  var declarationHasStruct = false
  var declarationHasTypedefStruct = false
  val structDeclarations = ListBuffer[String]()
  var specifierQualifierLevel = 0
  var currentTypeSpec: CParser.TypeSpecifierContext = null

  var isTypeEnum = false
  var isWithinFunction = false
  var hasTypedefName = false
  
  val cTypes = HashMap[String, String]()
  
  val typedefNames = ListBuffer[String]()
  var latestStorageSpecifier = ""
  var latestTypeSpec: CParser.TypeSpecifierContext = null
  var latestDirectDeclarator = ""
  
  var latestArraySize = 0
  var isArray = false
  val enumerations = ListBuffer[enumerator]()
  
  var latestStructDecName = ""
  var islatestStructDecArray = false
  
  case class enumerator(constant: String, expression: String)
  
  
  
  override def enterSpecifierQualifierList(ctx: CParser.SpecifierQualifierListContext) = {
    specifierQualifierLevel += 1
  }
  
  override def exitSpecifierQualifierList(ctx: CParser.SpecifierQualifierListContext) = {
    specifierQualifierLevel -= 1
  }
  
  override def enterEnumerator(ctx: CParser.EnumeratorContext) = {
    if (ctx.enumerationConstant() != null && ctx.constantExpression() != null) {
      enumerations += enumerator(ctx.enumerationConstant().getText, ctx.constantExpression().getText)
    }
  }
  
  override def enterInitDeclaratorList(ctx: CParser.InitDeclaratorListContext) = {
    isWithinFunction = true
  }
  
  override def enterDirectDeclarator(ctx: CParser.DirectDeclaratorContext) = {
    isArray = true
    islatestStructDecArray = true
    latestDirectDeclarator = ctx.getText
  }
  
  override def exitPrimaryExpression(ctx: CParser.PrimaryExpressionContext) = {
    if (ctx.expression() == null) { // is this the bottom of the tree?!
      latestArraySize = if (ctx.getText.contains("0x")) {
        Integer.getInteger(ctx.getText.drop(2), 16)
      } else if (ctx.getText forall Character.isDigit) {
          ctx.getText.toInt
      } else {
        0
      }
    }
  }

  override def enterEnumSpecifier(ctx: CParser.EnumSpecifierContext) = {
    isTypeEnum = true
  }
   
  override def exitTypedefName(ctx: CParser.TypedefNameContext) = {
    if (!isWithinStruct) {
      typedefNames += ctx.Identifier().getText
    }
    
    hasTypedefName = true
    latestStructDecName = ctx.Identifier().getText
  }
  
  override def enterStructDeclaration(ctx: CParser.StructDeclarationContext) = {
    latestStructDecName = ""
    islatestStructDecArray = false
  }
  
  override def exitStructDeclaration(ctx: CParser.StructDeclarationContext) = {
    
    
    
      if (islatestStructDecArray && latestArraySize != 0) {
        structDeclarations += "var " + latestDirectDeclarator + ": Array[" + translateTypeSpec(currentTypeSpec) + "]" + " = Array.fill(" + latestArraySize + ")(" + getTypeDefault(currentTypeSpec.getText) + ")"//type " + latestDirectDeclarator + " = Array[" + typedefNames(0) + "]\n"
      } else if (islatestStructDecArray && latestArraySize == 0) {
        structDeclarations += "var " + latestDirectDeclarator + ": Array[" + translateTypeSpec(currentTypeSpec) + "]" + " = null"//type " + latestDirectDeclarator + " = Array[" + typedefNames(0) + "]\n"
      } else if (currentTypeSpec != "") {
        println(getTypeDefault(cTypes.withDefaultValue("couldnt find")(currentTypeSpec.getText)))
        val baseTypeDefault = getTypeDefault(cTypes.withDefaultValue(currentTypeSpec.getText)(currentTypeSpec.getText))
        structDeclarations += "var " + convertTypeName(latestStructDecName, currentTypeSpec.getText) + ": " + translateTypeSpec(currentTypeSpec) + " = " + baseTypeDefault
      }
  }
  
  override def enterTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
    hasTypedefName = false
  }
  
  override def exitTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
    if (!hasTypedefName)
      latestTypeSpec = ctx
      
      if (specifierQualifierLevel == 1) {
        currentTypeSpec = ctx
      } 
  }
  
 
  
  override def enterFunctionDefinition(ctx: CParser.FunctionDefinitionContext) = {

    results ++= new FunctionConverter().translate(ctx)
  }
  
  override def exitFunctionDefinition(ctx: CParser.FunctionDefinitionContext) = {
  }
  
  override def enterDeclaration(ctx: CParser.DeclarationContext) = {
    latestStorageSpecifier = ""
    latestTypeSpec = null
    currentTypeSpec = null
    declarationHasStruct = false
    isTypeEnum = false
    isWithinFunction = false
    isArray = false
    typedefNames.clear
    enumerations.clear
  }
  
  override def exitDeclaration(ctx: CParser.DeclarationContext) = {
    if (declarationHasStruct && !isWithinStruct) {
      var result = "class " + typedefNames(0) + " {\n"
      //structDeclarations.foreach(println)
      if (!structDeclarations.isEmpty) {
        result += structDeclarations.map("  " + _).reduce{_ + "\n" + _}
      }
      result += "\n}"
      results += result
    } else if (isArray && typedefNames.size == 1) {
      results += "type " + latestDirectDeclarator + " = Array[" + typedefNames(0) + "]\n"
    } else if (!isTypeEnum && !isWithinFunction && latestStorageSpecifier != "extern") {
      if (typedefNames.size == 1) {
        results += "type " + typedefNames(0) + " = " + translateTypeSpec(latestTypeSpec) + "\n"
        cTypes += typedefNames(0) -> latestTypeSpec.getText
      } else if (typedefNames.size == 2) {
        results += "type " + typedefNames(1) + " = " + typedefNames(0) + "\n"
        cTypes += typedefNames(1) -> typedefNames(0)
      }
    } else if (isTypeEnum && !enumerations.isEmpty) {
      results += "type " + typedefNames(0) + " = Int"
      enumerations.foreach{enum =>
        results += ("val " + enum.constant + ": " + typedefNames(0) + " = " + enum.expression)
      }
    }
    
    declarationHasStruct = false
  }
  
  override def enterStructOrUnionSpecifier(ctx: CParser.StructOrUnionSpecifierContext) = {
    isWithinStruct = true
    declarationHasStruct = true
    structDeclarations.clear
  }
  
  override def exitStructOrUnionSpecifier(ctx: CParser.StructOrUnionSpecifierContext) = {
    isWithinStruct = false
  }
  
  override def enterStorageClassSpecifier(ctx: CParser.StorageClassSpecifierContext) = {
    //println("ENTERING TYPEDEF: " +ctx.getText)
    latestStorageSpecifier = ctx.getText
  }
  
  override def exitStorageClassSpecifier(ctx: CParser.StorageClassSpecifierContext) = {
    //println("LEAVING TYPEDEF ")
  }
}