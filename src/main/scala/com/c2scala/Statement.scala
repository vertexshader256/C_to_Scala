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
    import scala.collection.JavaConverters._
    
    def getAllChildren(acc: List[TerminalNode], ctx: ParseTree): List[TerminalNode] = {
      if (ctx.getChildCount == 0) {
        List(ctx.asInstanceOf[TerminalNode])
      } else {
        val children = ListBuffer[ParseTree]()
        val all = ListBuffer[ParseTree]()
        var i = 0
        while (ctx.getChild(i) != null) {
          children += ctx.getChild(i)
          i += 1
        }
        children.flatMap{ child => getAllChildren(acc, child)}.toList
      }
    }

    assignmentExpression = getAllChildren(Nil, ctx).map(_.getText).reduce(_ + " " + _)

    null
  }

}