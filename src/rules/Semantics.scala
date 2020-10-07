package rules

import shared._
import Definitions._
import Printing._
import Rules._
import test.UnsafeIterator
import Rewrite._
import shared.Valuation

object Semantics{
  
}

object Fire {
   def apply(G:ExtendedFact,theta:Valuation,premise:Premise) : Set[Valuation] = premise match {
     case EventPremise(event) => { 
    	   val b = theta(event)
           val others = G.events.filter({case a => matches(a,b)}) 
           val res = others.map{case a => theta.extend(matchE(a,b)) }  	   
    	   return res
          
     }
     //TODO currently this just works for Parameter Terms as we haven't implemented other kinds of terms yet
     case RuleExpression(name:RuleName,params:List[Term]) => {
    	   val others = G.ruleInstances.filter({    	    
    	     case (r,v) => r==name &&  params.forall{
    	       case p : Parameter => theta.applyP(p) == v.applyP(p); 
    	       case _ => false}
    	   })
    	   return others.map{case (r,v) => theta.extend(v)}
     }
     case GuardPremise(guard:Predicate) => {
       if(guard(theta)) return Set(theta)
       return Set()
     }
     case NegatedPremise(p:Premise) => {
       if(Fire(G,theta,p).isEmpty) return Set(theta)
       return Set()
     }
     return Set()
   }
   def apply(G:ExtendedFact,theta:Valuation,premises:List[Premise]) : Set[Valuation] = {
     if(premises.isEmpty) return Set(theta)
     Fire(G,theta,premises.head).map{ v => Fire(G,v,premises.tail)}.flatten
   }
   def apply(G:ExtendedFact,RS:RuleSystem, inst:RuleInstance) : Set[RuleInstance] = {  
     val (name,theta) = inst
     val definition = RS.definitions.find(
         {case RuleDefinition(n,params,body) => (n==name) && (params.toSet == theta.domain) })
     if(definition.isDefined){
       definition.get.body.map{
         case RuleTerm(left,right) => Fire(G,theta,left).map(v => right.map({
           //TODO again only dealing with Parameter terms
           case RuleExpression(n,ps) => {             
             val m = ps.map{
               case p : Variable => (p,v(p))
             }.toMap
             val newtheta = new Valuation(m)
             (n,newtheta)
           }               
       }))
       }.flatten.flatten
     }
     else Set()
   }
}

//TODO We don't have negated rule expressions yet so don't have to handle them here!
object Rewrite{
  implicit def lift_fact(fact:Fact) = new {
    def rewrite(event:Event,RS:RuleSystem) :Fact = {
      if(false){
    	  println("============")
    	  println("Rewriting")
    	  fact.ruleInstances.foreach(println)
    	  println("With "+event)
      }
      
      val extended = new ExtendedFact(fact.ruleInstances,Set(event))
      val updated = fact.ruleInstances.map{case ri =>
        val replace = Fire(extended,RS,ri)
        if(replace.isEmpty) Set(ri)
        else replace
      }.flatten
      new Fact(updated)
    }
  }
  implicit def lift_rs(RS:RuleSystem) = new {
    def apply(trace:List[Event]) : Boolean = {
      val initial = RS.initial
      val end : Fact = trace.foldLeft(initial)({case (f,a) => f.rewrite(a,RS)})
      if(false){
    	  println("============")
    	  println("End with ")
    	  end.ruleInstances.foreach(println)
      }
      end.ruleInstances.forall{case (r1,theta) =>
        !RS.bad.find({case RuleExpression(r2,params) => r1==r2 && theta.domain == params.toSet}).isDefined
      }
    }
  }
}

object main extends App {


  
  val rules = UnsafeIterator.create(false)
  //val rules = UnsafeIterator.original
  
  //print(rules)
  
  val col1 = Value("col1")
  val iter1 = Value("iter1")  
  
  val trace = List(
      Event("create",List(col1,iter1)),
      Event("use",List(iter1)),
      Event("update",List(col1)),
      Event("use",List(iter1))
  )
  
  val verdict = rules(trace)
  print(verdict)
}

object benchmark extends App {
  
def time[R](block: => R): Long = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    //println("Elapsed time: " + (t1 - t0)/1000000 + "ms")
    //result
    (t1 - t0)/1000000
}  
def run(rules:RuleSystem, cols:Int, iters_per_col:Int,uses:Int,label:String) {  
  
  val trace : List[Event] = (0 to cols).toList.map({c => 
     val iters : List[Event] = (0 to iters_per_col).toList.map{i => 
       val create = Event("create",List(Value("col"+c),Value("iter"+c+"_"+i)))
       val using = (0 to uses).toList.map{_ => Event("use",List(Value("iter"+c+"_"+i)))}   
       create +: using
     }.flatten 
     val updated = Event("update",List(Value("col"+c)))
     iters :+ updated
  }).flatten

  //trace.foreach(println)
  
  val t = time { 
  val verdict = rules(trace) 
  //println(verdict)  
  }
  println(label+" on "+trace.length+" took "+t+"ms")
} 

// Warmup

for(i <- 1 to 10){
  println("Iteration "+i)
  run(UnsafeIterator.original,10,10,100,"original")
  run(UnsafeIterator.optimised,10,10,100,"optimised")
  run(UnsafeIterator.create(false),10,10,100,"translated")
}


for(i <- 1 to 3){
  println("Iteration "+i)
  run(UnsafeIterator.original,20,10,100,"original")
  run(UnsafeIterator.optimised,20,10,100,"optimised")
  run(UnsafeIterator.create(false),20,10,100,"translated")
}

/*
for(i <- 1 to 3){
  println("Iteration "+i)
  run(UnsafeIterator.original,1,10,100,"original")
  run(UnsafeIterator.original,10,10,100,"original")
  run(UnsafeIterator.original,10,100,100,"original")
  run(UnsafeIterator.optimised,1,10,100,"optimised")
  run(UnsafeIterator.optimised,10,10,100,"optimised")
  run(UnsafeIterator.optimised,10,100,100,"optimised")
  run(UnsafeIterator.create(false),1,10,100,"translated")
  run(UnsafeIterator.create(false),10,10,100,"translated")
  run(UnsafeIterator.create(false),10,100,100,"translated")
}
*/

}