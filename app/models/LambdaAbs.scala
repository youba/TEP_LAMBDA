package models
import java.util.ArrayList

 class LambdaAbs extends ILambdaTerme{

  var params:ArrayList[LambdaTerme]=new ArrayList[LambdaTerme]()
  var corps :ArrayList[ILambdaTerme]=new ArrayList[ILambdaTerme]()
  
  override def equals(terme:ScalaObject):Boolean={
    if(terme.isInstanceOf[LambdaAbs]){
      var t:LambdaAbs=terme.asInstanceOf[LambdaAbs]
      return params.equals(t.params) & corps.equals(t.corps)
    }
    return false
  }
   
  override def isAbstraction():Boolean ={
    return true
  }
  
  override def isApplication():Boolean={
    return false
  }
  override def toString():String={
   var s:String="(l"
   var i:Int=0
   while(i<params.size()){
     s=s+params.get(i).toString()
     i=i+1
   }
   i=0
   s=s+"."
   
   while(i<corps.size()){
     s=s+corps.get(i).toString()
     i=i+1
   }
   s=s+")"
    return s
  }
  
  def afficheCorp():String={
    var s:String=""
    var i:Int=0
    while(i<corps.size()){
      s=s+corps.get(i)
      i=i+1
    }
    return s
  }
  
  
  def estDansCorp(terme : LambdaTerme):Boolean={
    var i:Int=0
    while(i<corps.size()){
      if(corps.get(i).equals(terme))
        return true
      i=i+1
    }
      return false
    
  }
  
   override def cloneTerme():ILambdaTerme={
    var ab :LambdaAbs=new LambdaAbs
    for(i<-0 until params.size()){
      ab.params.add(this.params.get(i).cloneTerme().asInstanceOf[LambdaTerme])
    }
    for(i<-0 until corps.size()){  
      ab.corps.add(this.corps.get(i).cloneTerme())
    }
    return ab
  }
   
   override def containsTerme(c:Char):Boolean={
     var trouve :Boolean=false
     for(i<-0 until params.size())
     {				
       if( params.get(i).containsTerme(c)){
         trouve =true
       }
     }
     for(i<-0 until corps.size())
     {				
         if(corps.get(i).containsTerme(c)){
           trouve=true
         }
     }
     return trouve
   }
  
   
   def corpsAbstractionToApplication()={
     
     if(!this.corps.isEmpty()){
       if(this.corps.get(0).isInstanceOf[LambdaAbs]){
         var premierAbs : LambdaAbs = this.corps.get(0).asInstanceOf[LambdaAbs] 
         var corpsPremierAbs : ArrayList[ILambdaTerme] = premierAbs.corps
         for(j<-0 until premierAbs.params.size()){
           this.params.add(premierAbs.params.get(j))
         }
         this.corps.remove(0)
         for(j<-0 until corpsPremierAbs.size()){ 
        	 this.corps.add(0,corpsPremierAbs.get(j))
         }
       }
     }
     
     if(this.corps.size()>=1){
     for(i<-1 until this.corps.size()){
       if(i<this.corps.size()-2 && this.corps.get(i).isAbstraction()){
         var newApp:ApplicationL = new ApplicationL
         newApp.abs=this.corps.get(i).cloneTerme().asInstanceOf[LambdaAbs]
         newApp.args=new ArrayList[ILambdaTerme]()
         newApp.args.add(this.corps.get(i+1))
        
         this.corps.add(i,newApp)
         this.corps.remove(i+1)
         this.corps.remove(i+1)
       }
     }
   }
   }
   

   
   
}