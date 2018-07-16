package translation


import shared._
import Definitions._
import qea._
import QEA._
import LabelledStates._

object FreshVariable {

  def labelled_qea_to_fresh_variable(input: LabelledQEA) : LabelledQEA = {
    
    val Y = input.Y   
    
    val new_delta = input.d.map{case (s1,e,p,a,s2) => 
      val free = e.vars & Y
      val vmap = free.map(v => (v,Variable(v.name+"_p"))).toMap
      val rmap = vmap.toMap[Parameter,Parameter]
      val new_params = e.parameters.map(p => rmap.getOrElse(p,p))
      val new_p : Predicate = if(p==TRUE) TRUE else RenamedPredicate(p,vmap)
      val new_a : Assignment = Assignment(vmap.toList.map{case (a,b) => Assign(a,b)} ++ a.assigns)
      (s1,Event(e.name,new_params),new_p,new_a,s2)
    }
    
    return new LabelledQEA(input.X,input.Q,input.A,new_delta,input.F,input.q0,input.v0)
  }
  
}