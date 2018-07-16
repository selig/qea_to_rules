package translation

import shared._
import Definitions._
import qea._
import QEA._
import LabelledStates._
import rules._

object ToRules {

def labelled_qea_to_rule_system(input: LabelledQEA) : RuleSystem = {  
 
  val Y = input.Y.toList.sorted
  
  val rd = input.Q.map{ 
    case (state,vars) => new RuleExpression("r"+state.name,vars.toList.sorted++Y)}
  
  //println("RD")
  //rd.foreach(println)
  //println("-------------")
  
  var definitions : Set[RuleDefinition] = input.Q.map{ case (state,vars) =>
    
    
    val params = (vars++Y).toList.sorted
    var body : Set[RuleTerm] = input.d.filter(_._1== ((state,vars))).map{
      case (_,e,p,a,(state2,vars2)) =>
        var left : List[Premise] = List()
        var right : List[RuleExpression] = List()
        
        // Start of left shared between (i) and (ii)
          if(p==TRUE){ left = List(EventPremise(e)) }
          else{ left = List(EventPremise(e),GuardPremise(p))}        
        
        // case (i) Transitions with the same label
        if(vars == vars2){          
          var pnew : List[Term] = vars2.toList.sorted 
          pnew ++= Y.map{v : Variable => a.termFor(v)}
          val re = RuleExpression("r"+state2.name,pnew)
          //println("re is "+re)
          right = List(re)
        }
        else{ // case (ii) Transitions extending the label
          assert(vars.subsetOf(vars2) && !vars.equals(vars2))
          var yp = 0
          val max_case = rd.filter{case RuleExpression(name,params) =>
          		val vp = params.map{case t : Term => t.asInstanceOf[Variable]}.toSet
          		!vars.equals(vp) && vars.subsetOf(vp)
          }.map{ case RuleExpression(name,params) =>
            val re = RuleExpression(name,params.map{case t : Term => t match {
              case Variable(n) => {
                if(Y.contains(Variable(n))){Variable(n+""+yp)} 
                else t
              }
              case _ => t 
            }})
            yp = yp+1;
            re
          }.toList.sortBy(_.name)
          left = left ++ max_case.map{case re : RuleExpression => NegatedPremise(re)}
          val re1 = RuleExpression("r_"+state.name,(vars++Y).toList.sorted)
          var pnew : List[Term] = vars2.toList.sorted 
          pnew ++= Y.map{v : Variable => a.termFor(v)}
          val re2 = RuleExpression("r"+state2.name,pnew)          
          right = List(re1,re2)
        }
        
        RuleTerm(left,right)
    }.filter{case rt:RuleTerm =>
    	!(rt.right.size == 1 && rt.right.head.equals(RuleExpression("r"+state.name,params))) 
    }
    
    new RuleDefinition("r"+state.name,params,body)
  }
  
  val initial : Fact = new Fact(Set(("r"+input.q0._1.name,input.v0)))
  var bad : Set[RuleExpression] = (input.Q -- input.F).filter{
    case (state,vars) => vars==input.X
  }.map{ 
    case (state,vars) => new RuleExpression("r"+state.name,vars.toList.sorted++Y)}
  
  return new RuleSystem(definitions,bad,initial)  
}

}