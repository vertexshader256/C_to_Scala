package com.c2scala

import java.io.IOException;

import org.antlr.v4.runtime.ANTLRFileStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;
import org.antlr.runtime.Token;
import scala.collection.mutable.ListBuffer

class MyListener extends CBaseListener {
  var isWithinStruct = false
  var declarationHasStruct = false
  var declarationHasTypedefStruct = false
  val structDeclarations = ListBuffer[String]()
  var specifierQualifierLevel = 0
  var currentTypeName = ""
  val results = ListBuffer[String]()
  var latestStorageSpecifier = ""
  
  override def enterSpecifierQualifierList(ctx: CParser.SpecifierQualifierListContext) = {
    specifierQualifierLevel += 1
  }
  
  override def exitSpecifierQualifierList(ctx: CParser.SpecifierQualifierListContext) = {
    specifierQualifierLevel -= 1
  }
  
  override def enterStructDeclarationList(ctx: CParser.StructDeclarationListContext) = {
    
  }
  
  override def exitStructDeclarationList(ctx: CParser.StructDeclarationListContext) = {
    //isWithinStruct = false
  }
  
  override def enterTypedefName(ctx: CParser.TypedefNameContext) = {
    
  }
  
  override def exitTypedefName(ctx: CParser.TypedefNameContext) = {
    println("typedef read completed: " + ctx.Identifier())
    if (isWithinStruct) {
      if (specifierQualifierLevel == 1) {
        currentTypeName = ctx.Identifier().getText
      } else if (specifierQualifierLevel == 2) {
        println(ctx.Identifier().getText)
        structDeclarations += "var " + ctx.Identifier().getText + ": " + currentTypeName
        println(ctx.Identifier().getText + ": " + currentTypeName)
      }
    } else if (declarationHasStruct && !isWithinStruct) {
      var result = "case class " + ctx.Identifier() + "(\n"
      //structDeclarations.foreach(println)
      result += structDeclarations.map("  " + _).reduce{_ + ",\n" + _}
      result += "\n)"
      results += result
    }
  }
  
  override def enterTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
   // println("ENTER TYPE ")
  }
  
  override def exitTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
   // println("EXIT TYPE ")
  }
  
  override def enterDeclaration(ctx: CParser.DeclarationContext) = {
    //println("ENTER DECLARATION ")
    latestStorageSpecifier = ""
    declarationHasStruct = false
  }
  
  override def exitDeclaration(ctx: CParser.DeclarationContext) = {
    declarationHasStruct = false
    //println("EXIT DECLARATION " )
  }
  
  override def enterStructOrUnionSpecifier(ctx: CParser.StructOrUnionSpecifierContext) = {
    //println("ENTERING STRUCT ")
    isWithinStruct = true
    declarationHasStruct = true
    structDeclarations.clear
  }
  
  override def exitStructOrUnionSpecifier(ctx: CParser.StructOrUnionSpecifierContext) = {
   // println("LEAVING STRUCT ")
    isWithinStruct = false
    var structResult = ""
    
  }
  
  override def enterStorageClassSpecifier(ctx: CParser.StorageClassSpecifierContext) = {
    //println("ENTERING TYPEDEF: " +ctx.getText)
    latestStorageSpecifier = ctx.getText
  }
  
  override def exitStorageClassSpecifier(ctx: CParser.StorageClassSpecifierContext) = {
    //println("LEAVING TYPEDEF ")
  }
}

object main {
    def main(arg: Array[String]) = {        
            val parser = new CParser(
                new CommonTokenStream(
                        new CLexer(
                                new ANTLRFileStream("C:\\Scala\\sandel_baseline\\src\\pre_ac_data.h"))));

            parser.setBuildParseTree(true);

            // This line prints the error
            val ctx = parser.compilationUnit();
            val listener = new MyListener();
            ParseTreeWalker.DEFAULT.walk(listener, ctx);            
      
            println("RESULTS: ")
            listener.results.foreach(println)
    }   
}