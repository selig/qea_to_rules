package test

import shared._
import Definitions._
import qea._
import QEA._
import translation.LabelledStates._
import translation.DomainExplicit._
import translation.FreshVariable._
import translation.ToRules._
import rules.RuleSystem
import rules.RuleExpression
import rules.RuleDefinition
import rules.Fact
import rules.Rules
import rules.Fact
import rules.EventPremise
import rules.NegatedPremise
import rules.RuleTerm
import Rules._

object UnsafeIterator {

  val c = Variable("c")
  val i = Variable("i")
  
  val create = Event("create",List(c,i))
  val update = Event("update",List(c))
  val use = Event("use",List(i))  
  
def create(printing:Boolean) : RuleSystem = {
  val one = State("1")
  val two = State("2")
  val three = State("3")
  val four = State("4")
  


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
  
  	if(printing) println("ORIG")
    if(printing) println(unsafe_iterator)
  
    var translated = concrete_to_labelled_qea(unsafe_iterator)
    if(printing) println("=================================================")
    if(printing) println("LABELLED")
    if(printing) println(translated)
    
    translated = labelled_qea_to_domain_explicit(translated)
    if(printing) println("=================================================")
    if(printing) println("DOMAIN")
    if(printing) println(translated)    
    
    translated = labelled_qea_to_fresh_variable(translated)
    if(printing) println("=================================================")
    if(printing) println("FRESH")
    if(printing) println(translated)    
    
    val rules = labelled_qea_to_rule_system(translated)
    if(printing) println("=================================================")
    if(printing) println("RULE SYSTEM")
    if(printing) println(rules.toLaTeX)
    
    return rules
}


def original : RuleSystem = {
  
  val r1 = RuleDefinition("r1",List(), Set(
		  RuleTerm(List(EventPremise(create),NegatedPremise(RuleExpression("r2",List(c,i)))),
				   List(RuleExpression("r2",List(c,i)),RuleExpression("r1",List())))
  ))
  val r2 = RuleDefinition("r2",List(), Set(
		  RuleTerm(List(EventPremise(update)),
				   List(RuleExpression("r3",List(c,i))))
  ))
  val r3 = RuleDefinition("r3",List(), Set(
		  RuleTerm(List(EventPremise(use)),
				   List(RuleExpression("Fail",List(c,i))))
  ))
  
  val definitions : Set[RuleDefinition] = Set(r1,r2,r3)
  var bad : Set[RuleExpression] = Set(RuleExpression("Fail",List(c,i)))
  val initial = new Fact(Set(("r1",emptyValuation)))
  
  RuleSystem(definitions,bad,initial)
}
  
def optimised : RuleSystem = {
  
  val r1 = RuleDefinition("r1",List(), Set(
		  RuleTerm(List(EventPremise(create),
		                NegatedPremise(RuleExpression("r2",List(c,i))),
		                NegatedPremise(RuleExpression("r3",List(c,i))),
		                NegatedPremise(RuleExpression("r4",List(c,i)))
		                ),
				   List(RuleExpression("r2",List(c,i)),RuleExpression("r1",List())))
  ))
  val r2 = RuleDefinition("r2",List(), Set(
		  RuleTerm(List(EventPremise(update)),
				   List(RuleExpression("r3",List(c,i))))
  ))
  val r3 = RuleDefinition("r3",List(), Set(
		  RuleTerm(List(EventPremise(use)),
				   List(RuleExpression("r4",List(c,i))))
  ))
  
  val definitions : Set[RuleDefinition] = Set(r1,r2,r3)
  var bad : Set[RuleExpression] = Set(RuleExpression("r4",List(c,i)))
  val initial = new Fact(Set(("r1",emptyValuation)))
  
  RuleSystem(definitions,bad,initial)
}


}

class main extends App{
  UnsafeIterator.create(true)
}