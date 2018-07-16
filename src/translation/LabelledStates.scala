package translation

import scala.collection.mutable.Queue

import shared._
import Definitions._
import qea._
import QEA._

// Section 4.1

object LabelledStates {

  type Label = (State,Set[Variable])

  implicit def order_labels(l1:Label) = new {
    def prettyprint : String = l1._1 + "{"+l1._2.mkString(",")+"}"
    def compare : Label => Int = {case l2:Label =>
    		var dif = l1._2.size - l2._2.size
    		if(dif==0){
    		  dif = l1._1.compare(l2._1)
    		}
    		dif
    }
  }
  
  type ConcreteQEA = QEA_abstract[State]
  type LabelledQEA = QEA_abstract[Label]
  
  def concrete_to_labelled_qea(input: ConcreteQEA) : LabelledQEA = {   
    
    val Y = input.Y    
    val ls_init : Label = (input.q0,Set()) 
   
    type Transitions = Set[(Label,Event,Predicate,Assignment,Label)]
    var delta : Transitions = Set()
    
    var todo : scala.collection.mutable.Queue[Label] = scala.collection.mutable.Queue()
    todo.enqueue(ls_init)
    
    var queued : Set[Label] = Set(ls_init)    
    
    while(!todo.isEmpty){
      val (state,vars) = todo.dequeue()
      //println("PROCESS " + ((state,vars)))
      
      var new_transitions : Transitions = input.d.filter(_._1 == state).map{
        case (_,event,p,a,s2) => ((state,vars),event,p,a,(s2,vars++(event.vars--Y))) 
      }
      val skip_transitions : Transitions = input.A.map{event =>
      	 val other_guards : Set[Predicate] = 
      	   input.d.filter(_._1==state).filter(_._2==event).map(_._3)
      	 val guard : Predicate = (if(other_guards.isEmpty) TRUE 
      	 						  else (if(other_guards.contains(TRUE)) FALSE
      	 						  else NegDisjunct(other_guards)))
      	 ((state,vars),event,guard,ID,(state,vars++(event.vars--Y)))
      }.filter(_._3!=FALSE)
      
      new_transitions ++= skip_transitions
      //println("skip_transitions")
      //skip_transitions.map(prettyprint[Label]).foreach(println)
      
      val new_states = new_transitions.map(_._5).filter(s => !queued.contains(s)).toSet
      
      new_states.foreach{s => todo.enqueue(s)}
      queued ++= new_states
      
      delta ++= new_transitions
    }

       
    val LS = delta.map{case (s1,_,_,_,s2) => Set(s1,s2)}.flatten
    val F = LS.filter{case (s:State,vars:Set[Variable]) => 
      ((input.F contains s) && (vars==input.X))}
    
    return new LabelledQEA(input.X,LS,input.A,delta,F,ls_init,input.v0)
  }
  
  
}