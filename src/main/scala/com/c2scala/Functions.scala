package com.c2scala

class FunctionConverter extends CBaseListener {
  override def enterFunctionDefinition(ctx: CParser.FunctionDefinitionContext) = {
    println("FUNCTION ENTERED: " + ctx.getText)
  }
  
  override def enterTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
    println("TYPE ENTERED")
  }
  
  override def exitFunctionDefinition(ctx: CParser.FunctionDefinitionContext) = {
    
    println("FUNCTION EXITED")
    println(ctx.getText)
    //println(ctx.declarationSpecifiers().g)
    //println(ctx.declarator().getText)
  }
}