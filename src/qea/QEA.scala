package qea

import shared._
import Definitions._


object QEA{
  type Alphabet = Set[Event]

  
}
import QEA._


case class State(name:String){
  override def toString : String = "s_"+name
  def <(other:State) : Boolean = name < other.name
  def compare : State => Int = {case that:State => name.compare(that.name)}
  def prettyprint: String = toString
}

case class QEA_abstract[S <% {def prettyprint : String; def compare:S=>Int}](
    val X:Set[Variable],
    val Q:Set[S],
    val A:Alphabet,
    val d:Set[(S,Event,Predicate,Assignment,S)],
    val F:Set[S],    
    val q0:S,
    val v0:Valuation
    ){
  
  def Y : Set[Variable] = A.map(_.vars).flatten -- X
  
  def is_well_formed : Boolean = 
    (Q contains q0) &&
    (F subsetOf Q) &&
    (v0.domain intersect X).isEmpty &&
    d.forall({case (s1,e,p,a,s2) => 
      (Q contains s1) && (Q contains s2) && (A contains e)
      })
  
  override def toString : String = {
    var res = "States: {"+Q.map(_.prettyprint).mkString(",")+"}, Final: {"+F.mkString(",")+"}\n"
    res += "X = {"+X.mkString(",")+"}, Y = {"+Y.mkString(",")+"}\n"
    
    d.toList.sortWith{
      case (t1,t2) => t1._1.compare(t2._1) > 0}.foreach{case (s1,e,p,a,s2) =>
        res += s1.prettyprint + " --"+e.toString+" if "
        res += p.prettyprint+" do "+a.prettyprint+"--> "+s2.prettyprint
        res+="\n"
      }
    
    res
  }
      
}

