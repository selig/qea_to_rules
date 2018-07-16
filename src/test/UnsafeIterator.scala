package test

import shared._
import Definitions._
import qea._
import QEA._
import translation.LabelledStates._
import translation.DomainExplicit._
import translation.FreshVariable._
import translation.ToRules._

object UnsafeIterator extends App {

  val one = State("1")
  val two = State("2")
  val three = State("3")
  val four = State("4")
  
  val c = Variable("c")
  val i = Variable("i")
  
  val create = Event("create",List(c,i))
  val update = Event("update",List(c))
  val use = Event("use",List(i))

val unsafe_iterator : QEA_abstract[State] = QEA_abstract[State](
      Set(c,i),
      Set(one,two,three,four),
      Set(create,update,use),
      Set(
    		  (one,create,TRUE,ID,two),
    		  (two,update,TRUE,ID,three),
    		  (three,use,TRUE,ID,four)
      ),
      Set(one,two,three),
      one,
      emptyValuation
  )  
  
  	println("ORIG")
    println(unsafe_iterator)
  
    var translated = concrete_to_labelled_qea(unsafe_iterator)
    println("=================================================")
    println("LABELLED")
    println(translated)
    
    translated = labelled_qea_to_domain_explicit(translated)
    println("=================================================")
    println("DOMAIN")
    println(translated)    
    
    translated = labelled_qea_to_fresh_variable(translated)
    println("=================================================")
    println("FRESH")
    println(translated)    
    
    val rules = labelled_qea_to_rule_system(translated)
    println("=================================================")
    println("RULE SYSTEM")
    println(rules)  
  
}