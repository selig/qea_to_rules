package test

import shared._
import Definitions._
import qea._
import QEA._
import translation.LabelledStates._
import translation.DomainExplicit._
import translation.FreshVariable._
import translation.ToRules._

object AuctionBidding extends App{

  val one = State("1")
  val two = State("2")
  val three = State("3")
  val four = State("4")
  
  val c = Variable("c")
  val i = Variable("i")
  val a = Variable("a")
  val min = Variable("min")
  
  val list = Event("list",List(i,min))
  val bid = Event("bid",List(i,a))
  val sell = Event("sell",List(i))
  
  val auction : QEA_abstract[State] = QEA_abstract[State](
      Set(i),
      Set(one,two,three,four),
      Set(list,bid,sell),
      Set(
    		  (one,list,TRUE,ID,two),
    		  (two,bid,GreaterInt(a,c),Assign(c,a),two),
    		  (two,sell,GreaterInt(a,min),ID,three),
    		  (two,list,TRUE,ID,four),
    		  (two,bid,Not(GreaterInt(a,c)),ID,four),
    		  (two,sell,Not(GreaterInt(c,min)),ID,four)
      ),
      Set(one,two,three),
      one,
      new Valuation(Map(c->Value(0)))
  )
  
    println("ORIG")
    println(auction)
  
    var translated = concrete_to_labelled_qea(auction)
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
    println(rules)}