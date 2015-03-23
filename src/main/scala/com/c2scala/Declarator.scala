package com.c2scala

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.Try


/******************************************
 * declarator
    :   pointer? directDeclarator gccDeclaratorExtension*
 * 
 * Takes care of arrays
 *****************************************/

class InitializerConverter(cTypes: HashMap[String, String], typeName: String) extends ChainListener[String](cTypes) {
  override def visitInitializer(ctx: CParser.InitializerContext) = {
    if (ctx.assignmentExpression != null)
      ctx.getText
    else if (ctx.initializerList != null) {
      println(visit(ctx.initializerList))
      "Array(" + visit(ctx.initializerList) + ")"
    } else {
      ""
    }
  }
  
  override def visitInitializerList(ctx: CParser.InitializerListContext) = {
    if (ctx.initializerList != null && ctx.initializer != null) {
      visit(ctx.initializerList) + "," + visit(ctx.initializer)
    } else if (ctx.initializerList == null && ctx.initializer != null && ctx.initializer.assignmentExpression != null) {
      ctx.initializer.getText
    } else {
      visit(ctx.initializer)
    }
  }
}
//latestStorageSpecifier: String,
      //   typedefNames: List[String], isFunctionPrototype: Boolean, directDeclarators: List[String], explicitInitValues: List[String]

class DeclaratorConverter(cTypes: HashMap[String, String], typeName: String, latestStorageSpecifier: String, qualifier: String) extends ChainListener[Unit](cTypes) {
  var initializer = ""
  var latestStructDecName: String = ""
  var currentTypeSpec: CParser.TypeSpecifierContext = null
  //var islatestStructDecArray: Boolean = false
  var varNames = ListBuffer[String]()
  val myExplicitInitValues = ListBuffer[String]()
  var isArray = false
  var level = -1
  val directDeclarators = ListBuffer[String]()
  var latestArraySize = ""
  var latestDeclaratorValue = ""
  
  def showDec(declList: List[String]): String = {
    if (!declList.isEmpty) {
      if (declList.size > 1) {
        "Array.fill(" + declList.head + ")(" + showDec(declList.tail) + ")"
      } else {
        "Array.fill(" + declList.head + ")(null)"
      }
    } else {
      ""
    }
  }
  
  override def visitInitDeclaratorList(ctx: CParser.InitDeclaratorListContext) = {

    level += 1
    super.visitInitDeclaratorList(ctx)
    level -= 1
  }
  
  override def visitInitDeclarator(ctx: CParser.InitDeclaratorContext) = {
    super.visitInitDeclarator(ctx)
    val isFunctionProto = Try(ctx.declarator.directDeclarator.parameterTypeList != null)
    
    if (level == 0 && !isFunctionProto.getOrElse(false)) {

      val init = if (isArray) {
        if (ctx.initializer() != null) {
          initializer
        } else if (!directDeclarators.isEmpty) {
          showDec(directDeclarators.toList)
        }
      } else {
  
        // e.g "float x,y,z;"
        if (varNames.size > 1) {
          "(" + varNames.zipWithIndex.map{ case (decl, index) =>
            postProcessValue(if (index < myExplicitInitValues.size) {
              myExplicitInitValues(index)
            } else {
              getDefault(cTypes, typeName)
            }, typeName)
          }.reduce(_ + ", " + _) + ")"
        } else if ((varNames.size == 1) && typeName != "") {
          val default = if (!myExplicitInitValues.isEmpty) {
              myExplicitInitValues(0)
            } else {
              getDefault(cTypes, typeName)
            } 
         postProcessValue(default, typeName)
        } else {
          if (currentTypeSpec != null) {
              postProcessValue(getDefault(cTypes, typeName), typeName)
          } else {
            "null"
          }
        }
      }
      
      results += latestDeclaratorValue + " = " + init
    }
  }
  
  override def visitDeclarator(ctx: CParser.DeclaratorContext) = {
    super.visitDeclarator(ctx)

      latestDeclaratorValue = if (isArray) {
          val arrayType = directDeclarators.toList.map{x => "Array["}.reduce(_ ++ _) + typeName + directDeclarators.toList.map{x => "]"}.reduce(_ ++ _)
          qualifier + " " + varNames(0) + ": " + arrayType
      } else if ((latestStorageSpecifier == "" || latestStorageSpecifier == "static")) {
  
      // e.g "float x,y,z;"
        if (varNames.size > 1) {
          val decl = "(" + varNames.map(_ + ": " + typeName).reduce(_ + ", " + _) + ")"
          qualifier + " " + decl
        } else if ((varNames.size == 1) && typeName != "") {
          qualifier + " " + varNames(0) + ": " + typeName
        } else {
          if (currentTypeSpec != null) {
              qualifier + " " + convertTypeName(latestStructDecName, typeName) + ": " + typeName
          } else {
            ""
          }
        }
      } else {
        ""
      }
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
    if (currentTypeSpec != null) {
        val baseTypeDefault = postProcessValue(getDefault(cTypes, typeName), typeName)
        results += "var " + convertTypeName(latestStructDecName, typeName) + ": " + typeName + " = " + baseTypeDefault
    }
  }
  
  def outputOneDec(qualifier: String) = {
   val baseTypeDefault = getDefault(cTypes, typeName)
          val default = if (!myExplicitInitValues.isEmpty) {
              myExplicitInitValues(0)
            } else {
              baseTypeDefault
            } 
     results += qualifier + " " + varNames(0) + ": " + typeName + " = " + postProcessValue(default, typeName) + "\n"
  }
  
  override def visitInitializer(ctx: CParser.InitializerContext) = {
    initializer = new InitializerConverter(cTypes, typeName).visit(ctx)
    myExplicitInitValues += ctx.getText
  }
  
  override def visitDirectDeclarator(ctx: CParser.DirectDeclaratorContext) = {
    super.visitDirectDeclarator(ctx)
    
      if (ctx.assignmentExpression() == null) {
          varNames += ctx.getText
      } else if (ctx.assignmentExpression() != null) {
        isArray = true
        directDeclarators += ctx.assignmentExpression().getText
      }// else
//        directDeclarators += ctx.getText
    
  }
  
}