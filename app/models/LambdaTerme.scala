package models

class LambdaTerme extends ILambdaTerme{
	var nom:Char='a'
  
	def LambdaTerme(nom:Char){
	  rename(nom)
	}
	  
	def LambdaTerme()={
	  
	}  
	def rename(newname:Char){
	  Tools.Alphabet.set(newname-97,false)
	  nom=newname 
	}  
	  
   override def equals(terme:ScalaObject):Boolean={
    
    if(terme == null) return false
    if(!terme.isInstanceOf[LambdaTerme]){
     return false 
    }
    var other : LambdaTerme= terme.asInstanceOf[LambdaTerme] 
    return (nom==other.nom)
  }
  
  override def isAbstraction():Boolean ={
    return false
  }
  
  override def isApplication():Boolean={
    return false
  }
  
  override def toString():String={
    return " "+nom+" "
  }
  
  override def cloneTerme():ILambdaTerme={
    var c :LambdaTerme=new LambdaTerme
    c.rename(this.nom)
    return c
  }
  
  override def containsTerme(c:Char):Boolean={
    if(c.equals(this.nom)){
      return true
    }
    return false
  }
 
}