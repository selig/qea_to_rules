package test

import shared._
import Definitions._
import qea._
import QEA._
import translation.LabelledStates._
import translation.DomainExplicit._
import translation.FreshVariable._
import translation.ToRules._

object Broadcast extends App {

  val one = State("1")
  val two = State("2")
  val three = State("3")
  
  val s = Variable("s")
  val r = Variable("r")
  
  val send_s = Event("send",List(s))
  val ack_r_s = Event("ack",List(r,s))  
  
  val broadcast : QEA_abstract[State] = QEA_abstract[State](
      Set(s,r),
      Set(one,two,three),
      Set(send_s,ack_r_s),
      Set(
    		  (one,send_s,TRUE,ID,two),
    		  (two,send_s,TRUE,ID,three),
    		  (two,ack_r_s,TRUE,ID,one)
      ),
      Set(one),
      one,
      emptyValuation
  )
  
    println("ORIG")
    println(broadcast)
  
    var translated = concrete_to_labelled_qea(broadcast)
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
    println(rules.toLaTeX)
}