package shared

trait Term { 
  def isParameter:Boolean;
  def isValue:Boolean
  def evaluate(v:Valuation) : Parameter
}

trait Parameter extends Term { 
  def isParameter = true
  override def evaluate(v:Valuation) : Parameter = this
}
case class Variable(val name:String) extends Parameter with Ordered[Variable]{ 
  override def isValue = false
  override def toString : String = name//"var_"+name 
  def compare(that:Variable) = name.compare(that.name)
}
case class Value(val value:Any) extends Parameter{
  override def isValue = true
  override def toString : String = "val_"+value
}

case class Event(val name:String,val parameters:List[Parameter]){
  def isGround : Boolean = (parameters forall {_ isValue})
  def vars : Set[Variable] = parameters.filter(!_.isValue).map(_.asInstanceOf[Variable]).toSet
  override def toString() = { name+"("+parameters.mkString(",")+")"}
}

class Valuation(m:Map[Variable,Value]) {

    def contains : Parameter => Boolean = {
      case v : Variable => m.contains(v)
      case v : Value => false
    }
    def apply(p:Parameter) : Value = p match {
      case v : Variable => m.apply(v)
      case v : Value => v
    }
    def +(x:Parameter,y:Parameter) : Valuation = {
      (x,y) match {        
        case (u:Variable,v:Value) => new Valuation(m + (u->v))
        case _ => this
      }
    }
    def domain : Set[Variable] = m.keys.toSet
    
    override def toString = {
      val pairs = m.toList.sortBy(_._1.name)
      "["+pairs.map{case (x,v) => x+"->"+v}.mkString(",")+"]"      
    }
    
  }

trait Predicate { 
  def apply(v:Valuation) : Boolean; 
  def prettyprint:String = toString
}

case class Assign(x:Variable,t:Term)
//TODO - I am going to assume that a variable is assigned to at most once
case class Assignment(assigns:List[Assign]){
  def apply(v:Valuation) : Valuation = {
      var res = v
      assigns.foreach{case Assign(in,out) => res = res.+(in,out.evaluate(res))}
      res    
  }
  def prettyprint:String = toString
  override def toString : String = assigns.toList.map{
    case Assign(a,b) => a+":="+b}.mkString(";")
  
  def termFor(v:Variable) : Term = {
      val get = assigns.filter(_.x==v)
      if(get.isEmpty){ v}
      else{get.head.t}
    }
}


object Definitions{
  
  val emptyValuation : Valuation = new Valuation(Map())
  
  def matchParameterList(first:List[Parameter],second:List[Parameter]) : (Valuation,Boolean) =
    		(first zip second).foldLeft(
    		    (emptyValuation,true))(
    		{
    		  case ((theta:Valuation,status:Boolean),(value:Parameter,param:Parameter)) =>
    		    if(status){
    		      if (param isValue) {
    		    	  ((theta,value equals param))     		      
    		      }
    		      else {
    		         if(theta contains param){
    		    	  ((theta,theta(param) equals value)) 
    		         }
    		         else{
    		           (((theta + (param,value)),status))
    		         }
    		      }
    		    }
    		    else{ ((theta,status))}
    		})    
  
  def matches(groundEvent:Event,otherEvent:Event) : Boolean = {
    assert(groundEvent isGround)
    return groundEvent.name == otherEvent.name &&
    		matchParameterList(groundEvent.parameters,otherEvent.parameters)._2
  }
  def matchE(groundEvent:Event,otherEvent:Event) : Valuation = {
    assert(matches(groundEvent,otherEvent))
    return matchParameterList(groundEvent.parameters,otherEvent.parameters)._1
  }
  

  
  val TRUE : Predicate = new Predicate{
    def apply(v:Valuation) = true
    override def toString = "TRUE"
  }
  val FALSE : Predicate = new Predicate{
    def apply(v:Valuation) = false
    override def toString = "FALSE"
  }  
  val ID : Assignment = Assignment(List[Assign]())
  
  // if others is empty then this should return something equivalent to TRUE
  case class NegDisjunct(others:Set[Predicate]) extends Predicate {
	  def apply(v:Valuation) : Boolean =  others.forall(!_.apply(v))
	  override def toString : String = "not ("+others.mkString(" or ")+")"
  }
  
  case class DistinctPred(pairs:Set[(Parameter,Parameter)]) extends Predicate{
    
    def apply(v:Valuation) : Boolean = pairs.forall{case (a,b) =>
    	v(a) != v(b)
    }
    
    override def toString : String = 
      pairs.toList.map{case (a,b) => a+"!="+b}.mkString(" and ")
  }
  
  // ASSERT domain and range of vmap are disjoint
  case class RenamedPredicate(p:Predicate,vmap:Map[Variable,Variable]) extends Predicate{
    def apply(v:Valuation) : Boolean = {
      // first update the valuation
      var v_p = v
      vmap.foreach{case (in,out) => v_p = v_p.+(out,v(in))}
      // now check p
      p(v_p)
    }
    override def toString : String = {
      var res = p.toString
      vmap.foreach{case (in,out) => 
        res = res.replace(in.toString,out.toString)}
      res
    }
  }
  case class Not(p:Predicate) extends Predicate {
    def apply(v:Valuation) : Boolean = !p(v)
    override def toString : String = "!("+p.toString+")"
  }
  
  case class GreaterInt(v1:Variable,v2:Variable) extends Predicate{
    def apply(v:Valuation) : Boolean = 
      (v(v1).asInstanceOf[Int] > v(v2).asInstanceOf[Int] )
    override def toString : String = v1+">"+v2
  }
 
  implicit def lift_assign(a:Assign) : Assignment = Assignment(List(a))
  
}