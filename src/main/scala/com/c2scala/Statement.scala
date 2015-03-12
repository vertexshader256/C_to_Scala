package com.c2scala

import scala.collection.mutable.ListBuffer

import scala.collection.mutable.HashMap
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.TerminalNode

case class Statement(statement: String)

class StatementConverter(cTypes: HashMap[String, String]) extends ChainListener[Statement](cTypes) {
  var assignmentExpression = ""
  var assignmentOperator = ""
  var unaryExpression = ""
  
   override def aggregateResult(aggregate: Statement, nextResult: Statement): Statement = {
    if (aggregate == null) {
        nextResult
    } else if (nextResult == null) {
        aggregate
    } else {
      null
    }
  }
  
  override def visitStatement(ctx: CParser.StatementContext) = {
    super.visitStatement(ctx)
    println(assignmentExpression)
    Statement(assignmentExpression)
  }
  
  override def visitUnaryExpression(ctx: CParser.UnaryExpressionContext) = {
    unaryExpression = ctx.getText
    null
  }
  
  override def visitAssignmentOperator(ctx: CParser.AssignmentOperatorContext) = {
    assignmentOperator = ctx.getText
    null
  }
  
  override def visitAssignmentExpression(ctx: CParser.AssignmentExpressionContext) = {

    assignmentExpression = new AssignmentExpressionConverter(cTypes).visitAssignmentExpression(ctx)

    null
  }

}