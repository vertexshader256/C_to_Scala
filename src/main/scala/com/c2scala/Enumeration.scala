package com.c2scala

import scala.collection.mutable.ListBuffer

import scala.collection.mutable.HashMap
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.Token

case class Enumeration(name: String, enumerators: Seq[Enumerator])
case class Enumerator(name: String, expression: String)

class EnumConverter(cTypes: HashMap[String, String]) extends ChainListener[Enumeration](cTypes) {
  val enumerations = ListBuffer[Enumerator]()
  var lastEnumConstant = "0"
  
  override def aggregateResult(aggregate: Enumeration, nextResult: Enumeration): Enumeration = {
    if (aggregate == null) {
        nextResult
    } else if (nextResult == null) {
        aggregate
    } else {
      null
    }
  }
  
  override def visitEnumSpecifier(ctx: CParser.EnumSpecifierContext) = {
    import scala.collection.JavaConverters._
    
    super.visitEnumSpecifier(ctx)
    val index = ctx.getParent.getParent.getRuleIndex
    // assume the typedef name is the last child
    val typeDefName = ctx.getParent.getParent.getParent.children.asScala.last.getText
    Enumeration(typeDefName, enumerations)
  }
  
  override def visitEnumerator(ctx: CParser.EnumeratorContext) = {
    if (ctx.enumerationConstant() != null && ctx.constantExpression() != null) {
      enumerations += Enumerator(ctx.enumerationConstant().getText, ctx.constantExpression().getText)
      if (ctx.constantExpression().getText forall Character.isDigit)
        lastEnumConstant = ctx.constantExpression().getText
    } else if (ctx.enumerationConstant() != null) {
      val newEnumConstant = lastEnumConstant.toInt + 1
      enumerations += Enumerator(ctx.enumerationConstant().getText, newEnumConstant.toString)
      lastEnumConstant = newEnumConstant.toString
    }
    null
  }

}