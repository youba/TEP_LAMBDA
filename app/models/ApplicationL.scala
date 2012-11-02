package models
import java.util.ArrayList

class ApplicationL extends ILambdaTerme{

  
  var abs:LambdaAbs=null
  //a changer en IlambdaTerme
  var args:ArrayList[ILambdaTerme]=new ArrayList[ILambdaTerme]() // a changer en ILambdaTerme
  
  
  override def toString():String={
    var s : String = "(l"
    var i :Int=0
    while(i<abs.params.size())
    {
      s=s+abs.params.get(i).toString()
      i=i+1
    }
    i=0
    s=s+"."
    while(i<abs.corps.size())
    {
      s=s+abs.corps.get(i).toString()
      i=i+1
    } 
    i=0
    s=s+") <"
    while(i<args.size())
    {
      s=s+args.get(i).toString()
      i=i+1
    }
    s=s+">"
    return s
  }
  
  override def isAbstraction():Boolean={
    return false
  }	
  override def isApplication():Boolean={
    return true
  }
	
  override def equals(terme:ScalaObject):Boolean={
    return false	
  }

  def isDansArg(terme: LambdaTerme):Boolean={
    for(i<-0 until args.size()){
      if(args.get(i).equals(terme))return true
        
    }
    return false
  }
  
  override def cloneTerme():ILambdaTerme={
    var a :ApplicationL = new ApplicationL
    a.abs=this.abs.cloneTerme().asInstanceOf[LambdaAbs]
    for(i<-0 until args.size()){
      a.args.add(this.args.get(i).cloneTerme().asInstanceOf[ILambdaTerme])
    }
    return a
  }
  
  override def containsTerme(c:Char):Boolean={
    var trouve : Boolean = false
    if(this.abs.containsTerme(c)){
      trouve=true
    }
    for(i<-0 until this.args.size()){
      if(this.args.get(i).containsTerme(c)){
        trouve = true
      }
    }
    return trouve
  }
  
 
  
  
  
}