package rules

import shared._
import Definitions._

object Rules{
  type RuleName = String 
  type RuleInstance = (RuleName,Valuation)
}
import Rules._

trait Premise{ def isNegated:Boolean; def wellFormed : Boolean; def vars:Set[Variable]}

case class EventPremise(event:Event) extends Premise {
  def isNegated = false
  def wellFormed = true
  def vars = event.parameters.filterNot(_ isValue).map(_.asInstanceOf[Variable]).toSet
  override def toString : String = event.toString
}
case class GuardPremise(guard:Predicate) extends Premise{
  def isNegated = false
  def wellFormed = true
  def vars = Set()
  override def toString : String = guard.prettyprint
}

case class RuleExpression(name:RuleName,parameters:List[Term]) extends Premise{
  def isPure : Boolean = (parameters forall (_.isParameter))
  def isNegated = false
  def wellFormed = true  
  def vars = parameters.filterNot(_ isValue).map(_.asInstanceOf[Variable]).toSet
  override def toString = name+(if(parameters.size>0){"("+parameters.mkString(",")+")"}else{""})
}

case class NegatedPremise(val premise:Premise) extends Premise{
  def isNegated = true
  def wellFormed = !(premise isNegated)
  def vars = premise.vars
  override def toString = "!"+premise.toString
}

case class RuleTerm(left:List[Premise],right:List[RuleExpression]){
  override def toString : String = left.mkString(",")+" --> "+right.mkString(",")
  def wellFormed : Boolean = {
    left.forall{
      case r: RuleExpression => r.isPure && r.wellFormed
      case other => other.wellFormed 
    } && 
    left.foldLeft((Set[Variable](),Set[Variable](),true))({
      case ((xs,ns,status),premise) =>  premise match {
        case NegatedPremise(p) => ((xs,ns++(p.vars--xs),status)) 
        case other => ((xs++other.vars,ns,(other.vars intersect ns).isEmpty))
      }
      })._3
  }
}

case class RuleDefinition(name:RuleName,parameters:List[Variable],body:Set[RuleTerm]){
  override def toString : String = {
    var res = name+(if(parameters.size>0){"("+parameters.mkString(",")+")"}else{""})
    if(body.isEmpty){ res+="{}"}
    else{
      res+="{\n"
      body.foreach(res += "\t"+_.toString+"\n")
      res+="}"
    }
    res
  }
}

class Fact(ruleInstances:Set[RuleInstance]){
  override def toString = "{"+ruleInstances.mkString(",")+"}"
}
class ExtendedFact(ruleInstances:Set[RuleInstance],events:Set[Event]) extends Fact(ruleInstances)

case class RuleSystem(
    val definitions : Set[RuleDefinition],
    val bad : Set[RuleExpression],
    val initial:Fact){
  
  override def toString : String = {
    var res = "Rule Definitions:\n"
    definitions.toList.sortBy{case rd: RuleDefinition => (rd.name,rd.parameters.length)
      }.foreach(res+=_.toString+"\n")
    res += "Bad: {"+ bad.mkString(",")+"}\n"
    res += "Initial: "+initial
    res
  }
  
}