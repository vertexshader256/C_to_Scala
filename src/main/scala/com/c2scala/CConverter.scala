package com.c2scala

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.Token

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
  
  val cTypes = HashMap[String, String]()
  
  val typedefNames = ListBuffer[String]()
  var latestStorageSpecifier = ""
  var latestTypeSpecifier = ""
  var latestDirectDeclarator = ""
  
  var latestArraySize = 0
  var isArray = false
  val enumerations = ListBuffer[enumerator]()
  
  var latestStructDecName = ""
  var islatestStructDecArray = false
  
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
  
  def getTypeDefault(typeSpecifier: String) = typeSpecifier match {
    case "char" | "long" | "short" | "int" => "0"
    case "float" | "double" => "0.0"
    case _ => "null"
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
        structDeclarations += "var " + latestDirectDeclarator + ": Array[" + convertTypeSpecifier(currentTypeName) + "]" + " = Array.fill(" + latestArraySize + ")(" + getTypeDefault(currentTypeName) + ")"//type " + latestDirectDeclarator + " = Array[" + typedefNames(0) + "]\n"
      } else if (islatestStructDecArray && latestArraySize == 0) {
        structDeclarations += "var " + latestDirectDeclarator + ": Array[" + convertTypeSpecifier(currentTypeName) + "]" + " = null"//type " + latestDirectDeclarator + " = Array[" + typedefNames(0) + "]\n"
      } else if (currentTypeName != "") {
        println(getTypeDefault(cTypes.withDefaultValue("couldnt find")(currentTypeName)))
        val baseTypeDefault = getTypeDefault(cTypes.withDefaultValue(currentTypeName)(currentTypeName))
        structDeclarations += "var " + convertTypeName(latestStructDecName, currentTypeName) + ": " + convertTypeSpecifier(currentTypeName) + " = " + baseTypeDefault
      }
  }
  
  override def enterTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
    hasTypedefName = false
  }
  
  override def exitTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
    if (!hasTypedefName)
      latestTypeSpecifier = ctx.getText
      
      if (specifierQualifierLevel == 1) {
        currentTypeName = ctx.getText
      } 
  }
  
  def copyTreeRecursive(original: ParseTree ): List[Token] = {
    if (original.getChildCount == 0) {
      List(original.getPayload.asInstanceOf[Token])
    } else {    
      (0 until original.getChildCount).toList.flatMap{x => copyTreeRecursive(original.getChild(x))}
    }
  }
  
  override def enterFunctionDefinition(ctx: CParser.FunctionDefinitionContext) = {
    
    import org.antlr.v4.runtime.CommonTokenStream;
    import org.antlr.v4.runtime.ListTokenSource;
    import org.antlr.v4.runtime.tree.ParseTreeWalker;
    import org.antlr.runtime.tree.CommonTree;
    import org.antlr.runtime.tree.CommonTreeAdaptor;
    import org.antlr.runtime.tree.TreeAdaptor;
    import org.antlr.runtime.Token;
    import scala.collection.JavaConversions._

    val listener = new FunctionConverter()

    val parser = new CParser(
            new CommonTokenStream(new ListTokenSource(copyTreeRecursive(ctx))))
    
        parser.setBuildParseTree(true);
  
        // This line prints the error
        val compilationUnit = parser.compilationUnit();
        
        ParseTreeWalker.DEFAULT.walk(listener, compilationUnit); 
  }
  
  override def exitFunctionDefinition(ctx: CParser.FunctionDefinitionContext) = {
  }
  
  override def enterDeclaration(ctx: CParser.DeclarationContext) = {
    latestStorageSpecifier = ""
    latestTypeSpecifier = ""
    currentTypeName = ""
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
        results += "type " + typedefNames(0) + " = " + convertTypeSpecifier(latestTypeSpecifier) + "\n"
        cTypes += typedefNames(0) -> latestTypeSpecifier
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