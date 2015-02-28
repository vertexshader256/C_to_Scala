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
  var latestDirectDeclarator = ""
  var isArrayTypedef = false
  val enumerations = ListBuffer[enumerator]()
  
  case class enumerator(constant: String, expression: String)
  
  def convertTypeName(varName: String, typeName: String) = {
    if (varName == "type") {
      typeName.toLowerCase()
    } else {
      varName
    }
  }
  
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
  
  override def enterEnumerator(ctx: CParser.EnumeratorContext) = {
    if (ctx.enumerationConstant() != null && ctx.constantExpression() != null) {
      enumerations += enumerator(ctx.enumerationConstant().getText, ctx.constantExpression().getText)
    }
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
    
  }
  
  override def enterDirectDeclarator(ctx: CParser.DirectDeclaratorContext) = {
    if (latestStorageSpecifier == "typedef") {
      isArrayTypedef = true
      latestDirectDeclarator = ctx.getText
      println(latestDirectDeclarator)
    }
    
  }
  
  override def exitDirectDeclarator(ctx: CParser.DirectDeclaratorContext) = {
  }
  
  override def enterEnumSpecifier(ctx: CParser.EnumSpecifierContext) = {
    isTypeEnum = true
  }
  
  override def enterDeclarationSpecifiers(ctx: CParser.DeclarationSpecifiersContext) = {
    
  }
  
  override def exitDeclarationSpecifiers(ctx: CParser.DeclarationSpecifiersContext) = {
    
  }
  
  override def enterTypedefName(ctx: CParser.TypedefNameContext) = {
    
  }
  
  override def exitTypedefName(ctx: CParser.TypedefNameContext) = {
    if (isWithinStruct) {
      if (specifierQualifierLevel == 2 && currentTypeName != "") {
        structDeclarations += "var " + convertTypeName(ctx.Identifier().getText, currentTypeName) + ": " + convertTypeSpecifier(currentTypeName)
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
      
      if (specifierQualifierLevel == 1) {
        currentTypeName = ctx.getText
      } 
  }
  
  override def enterDeclaration(ctx: CParser.DeclarationContext) = {
    latestStorageSpecifier = ""
    latestTypeSpecifier = ""
    currentTypeName = ""
    declarationHasStruct = false
    isTypeEnum = false
    isWithinFunction = false
    isArrayTypedef = false
    typedefNames.clear
    enumerations.clear
  }
  
  override def exitDeclaration(ctx: CParser.DeclarationContext) = {
    if (declarationHasStruct && !isWithinStruct) {
      var result = "case class " + typedefNames(0) + "(\n"
      //structDeclarations.foreach(println)
      if (!structDeclarations.isEmpty) {
        result += structDeclarations.map("  " + _).reduce{_ + ",\n" + _}
      }
      result += "\n)"
      results += result
    } else if (isArrayTypedef && typedefNames.size == 1) {
      results += "type " + latestDirectDeclarator + " = Array[" + typedefNames(0) + "]\n"
    } else if (!isTypeEnum && !isWithinFunction && latestStorageSpecifier != "extern") {
      if (typedefNames.size == 1) {
        results += "type " + typedefNames(0) + " = " + convertTypeSpecifier(latestTypeSpecifier) + "\n"
      } else if (typedefNames.size == 2) {
        results += "type " + typedefNames(1) + " = " + typedefNames(0) + "\n"
      }
    } else if (isTypeEnum && !enumerations.isEmpty) {
      results += "case class " + typedefNames(0) + "(value: Integer)"
      enumerations.foreach{enum =>
        results += ("case object " + enum.constant + " extends " + typedefNames(0) + "(" + enum.expression + ")")
      }
      //results += "\n"
    }
    
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