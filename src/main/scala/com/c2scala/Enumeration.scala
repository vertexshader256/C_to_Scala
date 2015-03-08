package com.c2scala

import scala.collection.mutable.ListBuffer

import scala.collection.mutable.HashMap
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.Token

case class Enumeration(enumerators: Seq[Enumerator])
case class Enumerator(name: String, expression: String)

class EnumConverter(cTypes: HashMap[String, String]) extends ChainListener[Enumeration](cTypes) {
  val enumerations = ListBuffer[Enumerator]()
  
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
    super.visitEnumSpecifier(ctx)
    Enumeration(enumerations)
  }
  
  override def visitEnumerator(ctx: CParser.EnumeratorContext) = {
    if (ctx.enumerationConstant() != null && ctx.constantExpression() != null) {
      enumerations += Enumerator(ctx.enumerationConstant().getText, ctx.constantExpression().getText)
    }
    null
  }

}