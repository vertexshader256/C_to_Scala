package com.c2scala

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

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
  val myDirectDeclarators = ListBuffer[String]()
  var initializer = ""
  var varName = ""
  val myExplicitInitValues = ListBuffer[String]()
  var isArray = false
  var level = -1
  val directDeclarators = ListBuffer[String]()
  
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

    if (level == 0) {
      if (isArray) {
        if (ctx.initializer() != null && !myDirectDeclarators.isEmpty) {
          val arrayType = myDirectDeclarators.toList.map{x => "Array["}.reduce(_ ++ _) + typeName + myDirectDeclarators.toList.map{x => "]"}.reduce(_ ++ _)
          results += "var " + varName + ": " + arrayType + " = " + initializer
        } else if (!myDirectDeclarators.isEmpty) {
          val arrayType = myDirectDeclarators.toList.map{x => "Array["}.reduce(_ ++ _) + typeName + myDirectDeclarators.toList.map{x => "]"}.reduce(_ ++ _)
          val value = showDec(myDirectDeclarators.toList)
          results += "var " + varName + ": " + arrayType + " = " + value
        }
      } else if ((latestStorageSpecifier == "" || latestStorageSpecifier == "static")) {
  
          // e.g "float x,y,z;"
            if (directDeclarators.size > 1) {
              val decl = "(" + directDeclarators.map(_ + ": " + typeName).reduce(_ + ", " + _) + ")"
              val baseTypeDefault = getDefault(cTypes, typeName)
              val defaults: String = "(" + directDeclarators.zipWithIndex.map{ case (decl, index) =>
                postProcessValue(if (index < myExplicitInitValues.size) {
                  myExplicitInitValues(index)
                } else {
                  baseTypeDefault
                }, typeName)
              }.reduce(_ + ", " + _) + ")"
              results += qualifier + " " + decl + " = " + defaults + "\n"
              println("HERE!")
            }// else if ((typedefNames.size <= 2 || directDeclarators.size == 1) && typeName != "") {
             // outputOneDec(qualifier)
            //} else {
            //  parseSimpleDecl()
            //}
      }
    }
  }
  
  override def visitInitializer(ctx: CParser.InitializerContext) = {
    initializer = new InitializerConverter(cTypes, typeName).visit(ctx)
    myExplicitInitValues += ctx.getText
  }
  
  override def visitDirectDeclarator(ctx: CParser.DirectDeclaratorContext) = {
    if (!ctx.getParent.isInstanceOf[CParser.DirectDeclaratorContext]) {
      directDeclarators += ctx.getText
    }
    
    super.visitDirectDeclarator(ctx)
    
    if (ctx.assignmentExpression() == null) {
      varName = ctx.getText
    } else if (ctx.assignmentExpression() != null) {
      myDirectDeclarators += ctx.assignmentExpression().getText
      isArray = true
    }
    
    
  }
  
}