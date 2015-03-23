package com.c2scala

import scala.collection.mutable.ListBuffer

import scala.collection.mutable.HashMap
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.TerminalNode

/*******************************************************
 * expression
    :   assignmentExpression
    |   expression ',' assignmentExpression
    ;
    
    Some math expression stuff
 *********************************************************/

class ExpressionConverter(cTypes: HashMap[String, String]) extends ChainListener[String](cTypes) {
  var assignmentExpression = ""
  var assignmentOperator = ""
  var unaryExpression = ""
  
  override def aggregateResult(aggregate: String, nextResult: String): String = {
    if (aggregate != null && nextResult == null) {
      aggregate
    } else if (aggregate == null && nextResult != null) {
      nextResult
    } else if (aggregate != null && nextResult != null) {
      aggregate + " " + nextResult
    } else {
      null
    }
  }
  
  override def visitExpression(ctx: CParser.ExpressionContext) = {
    super.visitExpression(ctx)
  }
  
  override def visitPrimaryExpression(ctx: CParser.PrimaryExpressionContext) = {
    if (ctx.expression() != null) "(" + super.visitPrimaryExpression(ctx) + ")" else super.visitPrimaryExpression(ctx)
  }
  
  override def visitAdditiveExpression(ctx: CParser.AdditiveExpressionContext) = {
    val left = if (ctx.additiveExpression() != null ) {
      visit(ctx.additiveExpression()) + " " + ctx.children.get(1).getText + " "
    } else {
      ""
    }
    left + visit(ctx.multiplicativeExpression())
  }
    
  override def visitMultiplicativeExpression(ctx: CParser.MultiplicativeExpressionContext) = {
    val left = if (ctx.multiplicativeExpression() != null ) visit(ctx.multiplicativeExpression()) + " " + ctx.children.get(1).getText + " " else ""
    left + visit(ctx.castExpression())
  }
  
  override def visitRelationalExpression(ctx: CParser.RelationalExpressionContext) = {
    val left = if (ctx.relationalExpression() != null ) visit(ctx.relationalExpression()) + " " + ctx.children.get(1).getText + " " else ""
    left + visit(ctx.shiftExpression())
  }
  
  override def visitEqualityExpression(ctx: CParser.EqualityExpressionContext) = {
    val left = if (ctx.equalityExpression() != null ) visit(ctx.equalityExpression()) + " " + ctx.children.get(1).getText + " " else ""
    left + visit(ctx.relationalExpression())
  }
  
  override def visitUnaryExpression(ctx: CParser.UnaryExpressionContext) = {
    if (ctx.unaryOperator() != null && ctx.castExpression() != null)  ctx.unaryOperator().getText + visit(ctx.castExpression()) else super.visitUnaryExpression(ctx)
  }
  
  override def visitAssignmentOperator(ctx: CParser.AssignmentOperatorContext) = {
    ctx.getText()
  }
  
  override def visitPostfixExpression(ctx: CParser.PostfixExpressionContext) = {
    val result = if (ctx.postfixExpression() != null) {
      ctx.getText
    } else if (super.visitPostfixExpression(ctx) != null) {
      super.visitPostfixExpression(ctx)
    } else {
      ctx.getText
    }
    result.replace("->", ".")
  }

}