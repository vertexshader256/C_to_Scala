package com.c2scala

import scala.collection.mutable.ListBuffer

import scala.collection.mutable.HashMap
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.TerminalNode

case class Statement(statement: String)

/*******************************************************************
 * statement
    :   labeledStatement
    |   compoundStatement
    |   expressionStatement
    |   selectionStatement
    |   iterationStatement
    |   jumpStatement
    |   ('__asm' | '__asm__') ('volatile' | '__volatile__') '(' (logicalOrExpression (',' logicalOrExpression)*)? (':' (logicalOrExpression (',' logicalOrExpression)*)?)* ')' ';'
    ;
    
    Very complex
 */

class StatementConverter(cTypes: HashMap[String, String]) extends ChainListener[Statement](cTypes) {
  var expression = ""
  var selectionStatement = ""
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
    if (expression != "")
      Statement(expression)
    else
      Statement(selectionStatement)
  }
  
  override def visitUnaryExpression(ctx: CParser.UnaryExpressionContext) = {
    unaryExpression = ctx.getText
    null
  }
  
  override def visitAssignmentOperator(ctx: CParser.AssignmentOperatorContext) = {
    assignmentOperator = ctx.getText
    null
  }
  
  override def visitSelectionStatement(ctx: CParser.SelectionStatementContext) = {
    selectionStatement = "if (" + new ExpressionConverter(cTypes).visitExpression(ctx.expression()) + ") " + new StatementConverter(cTypes).visit(ctx.statement.get(0)).statement
    if (ctx.statement.size > 1) {
      selectionStatement += " else " + new StatementConverter(cTypes).visit(ctx.statement.get(1)).statement
    }
    null
  }
  
  override def visitExpression(ctx: CParser.ExpressionContext) = {
    expression = new ExpressionConverter(cTypes).visitExpression(ctx)
    null
  }

}