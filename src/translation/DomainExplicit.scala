package translation

import shared._
import Definitions._
import qea._
import QEA._
import LabelledStates._

object DomainExplicit {

  // Section 4.2
  
  def labelled_qea_to_domain_explicit(input: LabelledQEA) : LabelledQEA = {   
  
    val Y = input.Y    
    
    type Transitions = Set[(Label,Event,Predicate,Assignment,Label)]
    val new_delta : Transitions =  input.d ++ input.Q.map{case (state,varS) => 
      input.A.filter{case event => !event.vars.intersect(input.X--varS).isEmpty}.map{event =>
        (varS.intersect(event.vars--Y)).subsets.filter(x => !x.isEmpty).map{
          case replace =>
            val rmap = replace.map(v => (v,Variable(v.name+"_p"))).toMap[Parameter,Parameter]
            val new_params = event.parameters.map(p => rmap.getOrElse(p,p))

            val kept_vars = event.vars -- replace
            val guard = DistinctPred(rmap.toSet)

            ((state,varS),Event(event.name,new_params),guard,ID,(state,varS++kept_vars))
        }
      }.flatten  
    }.flatten

    // Don't extend F in paper as implicitly the set of states is the full set (we do not 
    // remove unreachable states)
    val orig_F = input.F.map(_._1)
    val new_F = input.F ++ new_delta.map(_._5).filter{
      case (state,vars) => orig_F.contains(state) && vars==input.X}
    
    return new LabelledQEA(input.X,input.Q,input.A,new_delta,new_F,input.q0,input.v0)
  }
}