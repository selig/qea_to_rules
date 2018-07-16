package qea

import shared._
import Definitions._
import QEA._

object qeaSemantics{
  
  type Configuration = (State,Valuation)
  type MonitoringState = Map[Valuation,Configuration]
 
  def relevant(theta:Valuation,groundEvent:Event,alphabet:Alphabet) : Boolean = {
    assert(groundEvent isGround)
    return alphabet.exists(otherEvent => matches(groundEvent,otherEvent))
  }
}