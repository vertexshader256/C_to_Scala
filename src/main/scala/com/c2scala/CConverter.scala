package com.c2scala

import scala.collection.mutable.ListBuffer

class CConverter extends CBaseListener {
  var isWithinStruct = false
  var declarationHasStruct = false
  var declarationHasTypedefStruct = false
  val structDeclarations = ListBuffer[String]()
  var specifierQualifierLevel = 0
  var currentTypeName = ""
  val results = ListBuffer[String]()
  var isTypeEnum = false
  var isWithinFunction = false
  var hasTypedefName = false
  
  val typedefNames = ListBuffer[String]()
  var latestStorageSpecifier = ""
  var latestTypeSpecifier = ""
  
  def convertTypeSpecifier(typeSpecifier: String) = typeSpecifier match {
    case "char" => "Char"
    case "float" => "Float"
    case "long" => "Long"
    case "short" => "Short"
    case "int" => "Integer"
    case _ => typeSpecifier
  }
  
  override def enterSpecifierQualifierList(ctx: CParser.SpecifierQualifierListContext) = {
    specifierQualifierLevel += 1
  }
  
  override def exitSpecifierQualifierList(ctx: CParser.SpecifierQualifierListContext) = {
    specifierQualifierLevel -= 1
  }
  
  override def enterStructDeclarationList(ctx: CParser.StructDeclarationListContext) = {
    
  }
  
  override def exitStructDeclarationList(ctx: CParser.StructDeclarationListContext) = {
    //isWithinStruct = false
  }
  
  override def enterInitDeclaratorList(ctx: CParser.InitDeclaratorListContext) = {
    isWithinFunction = true
  }
  
  override def exitInitDeclaratorList(ctx: CParser.InitDeclaratorListContext) = {
    //isWithinStruct = false
  }
  
  override def enterEnumSpecifier(ctx: CParser.EnumSpecifierContext) = {
    isTypeEnum = true
  }
  
  override def enterDeclarationSpecifiers(ctx: CParser.DeclarationSpecifiersContext) = {
    
  }
  
  override def exitDeclarationSpecifiers(ctx: CParser.DeclarationSpecifiersContext) = {
    if (declarationHasStruct && !isWithinStruct) {
      var result = "case class " + typedefNames(0) + "(\n"
      //structDeclarations.foreach(println)
      if (!structDeclarations.isEmpty) {
        result += structDeclarations.map("  " + _).reduce{_ + ",\n" + _}
      }
      result += "\n)"
      results += result
    } else if (!isTypeEnum && !isWithinFunction && latestStorageSpecifier != "extern") {
      if (typedefNames.size == 1) {
        results += "type " + typedefNames(0) + " = " + convertTypeSpecifier(latestTypeSpecifier) + "\n"
      } else if (typedefNames.size == 2) {
        results += "type " + typedefNames(1) + " = " + typedefNames(0) + "\n"
      }
    }
  }
  
  override def enterTypedefName(ctx: CParser.TypedefNameContext) = {
    
  }
  
  override def exitTypedefName(ctx: CParser.TypedefNameContext) = {
    if (isWithinStruct) {
      if (specifierQualifierLevel == 1) {
        currentTypeName = ctx.Identifier().getText
      } else if (specifierQualifierLevel == 2) {
        structDeclarations += "var " + ctx.Identifier().getText + ": " + currentTypeName
      }
    } else {
      typedefNames += ctx.Identifier().getText
    }
    
    hasTypedefName = true
  }
  
  override def enterTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
   // println("ENTER TYPE ")
    hasTypedefName = false
  }
  
  override def exitTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
   // println("EXIT TYPE ")
    if (!hasTypedefName)
      latestTypeSpecifier = ctx.getText
  }
  
  override def enterDeclaration(ctx: CParser.DeclarationContext) = {
    latestStorageSpecifier = ""
    declarationHasStruct = false
    isTypeEnum = false
    isWithinFunction = false
    typedefNames.clear
  }
  
  override def exitDeclaration(ctx: CParser.DeclarationContext) = {
    declarationHasStruct = false
  }
  
  override def enterStructOrUnionSpecifier(ctx: CParser.StructOrUnionSpecifierContext) = {
    //println("ENTERING STRUCT ")
    isWithinStruct = true
    declarationHasStruct = true
    structDeclarations.clear
  }
  
  override def exitStructOrUnionSpecifier(ctx: CParser.StructOrUnionSpecifierContext) = {
   // println("LEAVING STRUCT ")
    isWithinStruct = false
    var structResult = ""
    
  }
  
  override def enterStorageClassSpecifier(ctx: CParser.StorageClassSpecifierContext) = {
    //println("ENTERING TYPEDEF: " +ctx.getText)
    latestStorageSpecifier = ctx.getText
  }
  
  override def exitStorageClassSpecifier(ctx: CParser.StorageClassSpecifierContext) = {
    //println("LEAVING TYPEDEF ")
  }
}