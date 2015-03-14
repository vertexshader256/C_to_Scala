package com.c2scala

import scala.collection.mutable.ListBuffer

import scala.collection.mutable.HashMap
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.Token

case class Struct(name: String, structDecl: Seq[String])

class StructConverter(cTypes: HashMap[String, String]) extends ChainListener[Struct](cTypes) {
  var declarationHasStruct = false
  var declarationHasTypedefStruct = false
  val structDeclarations = ListBuffer[String]()
  


  var latestStorageSpecifier = ""

  
   override def visitStructOrUnionSpecifier(ctx: CParser.StructOrUnionSpecifierContext) = {
    import scala.collection.JavaConverters._
    
    declarationHasStruct = true
    structDeclarations.clear
    declarationHasStruct = false
    
    super.visitStructOrUnionSpecifier(ctx)
    
    val typeDefName = ctx.getParent.getParent.getParent.children.asScala.last.getText
    Struct(typeDefName, structDeclarations)
  }

   override def aggregateResult(aggregate: Struct, nextResult: Struct): Struct = {
    if (aggregate == null) {
        nextResult
    } else if (nextResult == null) {
        aggregate
    } else {
      null
    }
  }
  
  
  
  override def visitDirectDeclarator(ctx: CParser.DirectDeclaratorContext) = {
    super.visitDirectDeclarator(ctx)
    null
  }
 
   
  override def visitTypedefName(ctx: CParser.TypedefNameContext) = {   
    null
  }
  
  override def visitStructDeclaration(ctx: CParser.StructDeclarationContext) = {
    val declConverter = new DeclarationConverter(cTypes, true)
    declConverter.visitStructDeclaration(ctx)
    declConverter.results.foreach{ structDecl => structDeclarations += structDecl }
    null
  }
   
  override def visitTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
    super.visitTypeSpecifier(ctx)
      
    null
  }
 
  
 

}