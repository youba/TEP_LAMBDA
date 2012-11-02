package models
import scala.collection.mutable.ArrayLike
import java.util.ArrayList
import com.sun.corba.se.spi.transport.CorbaAcceptor

abstract class ITerme {
  def isAbs() :Boolean ;
  def isTerme() : Boolean;
   def isApp() : Boolean;
  def cloneTerme() : ITerme;
  def equalsTo(terme: ScalaObject):Boolean ;
}



class  Terme extends ITerme {
  
   var nom : Char = ' '
   
   def Terme(name : Char ){
      nom = name
   }
   
   def  setName(name : Char ): Unit ={
     nom = name
     Tools.Alphabet.set(name-97,false)
   }
   
   override def cloneTerme():ITerme={
       var copy : Terme = new Terme()
        copy.setName(nom)
        return copy
   }
   
   override def equalsTo(terme: ScalaObject):Boolean = {
    if(terme == null) return false
    if(!terme.isInstanceOf[Terme]){
     return false 
    }
    var other = terme.asInstanceOf[Terme] 
    return (nom==other.nom)
   }
   
   def getName(): Char = {
     return nom 
   }
   override def toString():String ={
	   return nom.toString()
   }
   
   override def isAbs() :Boolean ={
      return false
   }
  override def isTerme() : Boolean = {
    return true 
  }
  override def isApp() : Boolean = {
    return false 
  }
}

class Abs extends ITerme {
   var params  = new ArrayList[Terme] 
   var corps  =  new ArrayList[ITerme]
   
   override def isAbs() :Boolean ={
      return true
   }
   
   override def equalsTo(terme:ScalaObject):Boolean={
    if(terme.isInstanceOf[Abs]){
      var t = terme.asInstanceOf[Abs]
       return  arrayListComparer.Equals(t.params,params) && arrayListComparer.isEquals(t.corps,corps)
    }
    return false
  }
   
   
   override def isTerme() : Boolean = {
    return false 
  }
  override def isApp() : Boolean = {
    return false 
  }
   override def toString():String ={
	   var sb   = new StringBuilder()
	   sb.append("(l")
	   var i = 0
	   while( i < params.size()){
	     sb.append(params.get(i))
	     i = i + 1
	   }
	   sb.append(".")
	   var j = 0
	   while( j < corps.size()){
	     sb.append(corps.get(j))
	     j =j +1
	   }
	   sb.append(")") 
	   return sb.toString()
   }
   override def cloneTerme():ITerme={
       var copy : Abs = new Abs()
        for( i <-0 until params.size())
	        copy.params.add(params.get(i).cloneTerme().asInstanceOf[Terme])
	   for( i <-0 until corps.size())
	         copy.corps.add(corps.get(i).cloneTerme())
       
        return copy
   }
   
   def containsTerme(name : Char):Boolean = {
     for( i <-0 until corps.size()){
       var terme  = corps.get(i)
       if(terme.isTerme()){
         if(terme.asInstanceOf[Terme].getName() == name)
           return true
       }
     }
	 for( i <-0 until params.size())
		 if(params.get(i).getName() == name)
		   return true
     return false
   }
   
}

class App extends ITerme {
   var  args = new ArrayList[ITerme]
   var   abs : Abs = null 
   override def isApp() : Boolean = {
      return true 
  }
   override def isAbs() :Boolean ={
      return false
   }
    override def equalsTo(terme:ScalaObject):Boolean={
    
      if(terme.isInstanceOf[App]){
      var t =terme.asInstanceOf[App]
      return arrayListComparer.isEquals(args,t.args) &&  abs.equalsTo(t.abs)
    }
    return false
    }
   
   
   override def isTerme() : Boolean = {
    return false 
  }
   override def toString():String ={
	   var sb   = new StringBuilder()
	   sb.append("[ ")
	     sb.append(abs.toString())
	   sb.append("<")
	    var i = 0
	   while( i < args.size()){
	     sb.append(args.get(i))
	     i =i + 1
	   }
	   sb.append("> ]") 
	   return sb.toString()
   }
   
    override def cloneTerme():ITerme={
       var copy  = new App
       copy.abs =  abs.cloneTerme().asInstanceOf[Abs]
	   for( i <-0 until  args.size())
	         copy.args.add(args.get(i).cloneTerme())
        return copy
   }
}


object Copyer{
   	def copyArrayList(src : ArrayList[ITerme]):ArrayList[ITerme]= {
    var res  =  new ArrayList[ITerme]
    for(i <- 0 until src.size ){
      res.add(src.get(i).cloneTerme)
    }
    
    return res
   }
}

object arrayListComparer{
  
  def  isEquals( ar1 : ArrayList[ITerme],ar2: ArrayList[ITerme]):Boolean ={
     if(ar1.size != ar2.size) return false
     for(i <-0 until ar1.size){
       if( !ar1.get(i).equalsTo(ar2.get(i)) )
            return false
     }
    return true
  }
  def  Equals( ar1 : ArrayList[Terme],ar2: ArrayList[Terme]):Boolean ={
     if(ar1.size != ar2.size) return false
     for(i <-0 until ar1.size){
       if( !ar1.get(i).equalsTo(ar2.get(i)) )
            return false
     }
    return true
  }
}

object LambdaEvaluator2 {

   //AJOUT
  def evalManager(action:Int,terme:ArrayList[ITerme],etapes:ArrayList[ArrayList[ITerme]]):ArrayList[ITerme]={
    action match{
			case 1 => {if(etapes.isEmpty())
			{
				etapes.add(Copyer.copyArrayList(terme))	
				var nextTerme :ArrayList[ITerme] = this.betaReduction(terme)
				etapes.add(nextTerme)	
				return nextTerme
			}
			else{
				var nextTerme :ArrayList[ITerme] = this.betaReduction(Copyer.copyArrayList(terme))
				if(! arrayListComparer.isEquals(nextTerme,terme)){
				    etapes.add(nextTerme)	
						return nextTerme
				}
			    throw new Exception("cette expression ne comporte aucun Redex !!")
			}
			}
			case 2 =>
			if(etapes.isEmpty()){

				return terme
			}
			if(etapes.size()==1)return etapes.get(0)
					etapes.remove(etapes.size()-1)
					return etapes.get(etapes.size()-1)

			case _=> println("Action doit etre 1,2 ")  
			return null
			} 
   }
  
  
   def alphaConversion( abs: Abs , terme : ITerme ): Unit = {
      if(terme.isTerme()){
        var  t  = terme.asInstanceOf[Terme]
        var param  =  abs.params.get(0)
        if(abs.containsTerme(t.getName())){
        for(i <-0 until abs.params.size()){
             var c   = abs.params.get(i)
               if(c.getName() == t.getName()){
                 var nchar = Tools.getFirstFree
                  for(j <-0 until abs.corps.size()){
                	  var co  : ITerme = abs.corps.get(j)
                	  if(co.isTerme()){
                		  var h = co.asInstanceOf[Terme]
                		  if(h.getName() == t.getName())
                			  h.setName(nchar)
            		}
                  }
                   c.setName(nchar)
               }
        }
          
        }
      }
     
  }
  
  def betaReduction(lambdaApp : ArrayList[ITerme] ): ArrayList[ITerme]={
    var  abs  : ITerme = null
    var size = lambdaApp.size()
    var  i  = 0
    var stop = false
    if(lambdaApp.isEmpty())
      return lambdaApp
      
    while(i < size && ! stop){
      var  terme =  lambdaApp.get(i)
      if(terme.isAbs() || terme.isApp()){
    	abs =  terme
    	stop = true 
      }        
      i = i + 1
    }
    if(i < size && abs != null){
     if(abs.isAbs() ){
    	 var ab = abs.asInstanceOf[Abs]
    	 var  terme =  lambdaApp.remove(i)
    	 
    	 replace(ab,terme)
    	 if(ab.params.size() == 0){
    		 lambdaApp.remove(i-1)
    		 lambdaApp.addAll(i-1,ab.corps)
    	 }
      }else if(abs.isApp()){
        var app = abs.asInstanceOf[App]
        var res =reduceApp(app)
         lambdaApp.set(i-1,res)
        }
    }else if(i == size && abs != null) {
     var  res= reduceApp(lambdaApp.get(i-1))	
    	 if(res.isAbs()){
    	  var ab = abs.asInstanceOf[Abs]
    	  if(ab.params.isEmpty()){
    	    lambdaApp.remove(i-1)
    	    lambdaApp.addAll(i-1,ab.corps)
    	  }
    	}else 
    	  lambdaApp.set(i-1,res)
    }
    
    return  lambdaApp 
  }
  
  def reduceApp(te : ITerme ):ITerme={
		   
      if(te.isAbs() ){
    	 var ab = te.asInstanceOf[Abs]
    	  betaReduction(ab.corps)
    	 return ab 
      }else if (te.isApp()){
        var app = te.asInstanceOf[App]
        if(app.args.isEmpty())
        	return reduceApp(app.abs)
        var  tt   =  app.args.remove(0) 
       	
        replace(app.abs,tt)
        if(app.args.isEmpty())
          return app.abs
        return app
      }
      return te 
  }
  
  
  def  replace(abs : Abs , terme : ITerme) : Unit={
    
     alphaConversion(abs,terme)
     var  param  = abs.params.remove(0)
     for(i <- 0  until abs.corps.size()){
       var t = abs.corps.get(i)
       if(t.isTerme() && t.asInstanceOf[Terme].getName() == param.getName()){
         abs.corps.set(i,terme.cloneTerme())
       }
     } 
  }
}

import scala.util.control.Breaks._

object LParser {

	def parseLambda(exp  : String): ArrayList[ITerme]= {
			var  expr : String = exp ;
	expr = expr.trim() 
	if( ! expr.startsWith("(") ){
		expr = "("+expr+ ")"
	}
	var   sb : StringBuilder  = new StringBuilder() 
	
    for( cpt <- 0 until expr.length()){
    		var  curr :Char = expr.charAt(cpt)
    	     if(curr != ' ')
    		   sb.append(curr)
    }
	  
	
	
	var app  = new ArrayList[ITerme]
	var pred : Char = ' '
	var isCorps : Boolean = false 
	var isArgs  : Boolean = false

	var chars: Array[Char] = sb.toArray[Char] ;
	var i : Int =0  
	while( i<chars.length){
		var current: Char =chars.apply(i)

				current match  {
				case ' ' => Unit
				case 'l' => Unit
				case '(' => {
					if(chars.apply(i+1) == 'l'){
							i = parseLambdaAbs(chars, i, app)	 
					}else if(chars.apply(i+1) == '('){
							i= parseApplication(chars, i,app)
					}

				}
				case ')' =>  Unit
				case '.' =>  Unit

				case _ =>{
					
					var lmd   = new Terme()
					lmd.setName(current)
					app.add(lmd)
					
				}
	}
	pred=current	
			i = i+ 1
	}
	
		return app
	}



	def parseApplication( array : Array[Char],pos : Int , parent: ArrayList[ITerme]): Int = {
	var app = new App()
	var prof : Int  = pos 
	while (array.apply(prof+1) == '(' ) {
		prof = prof +1 
	}

			var i : Int = parseLambdaAbs(array, prof,parent)
					app.abs  =  parent.remove(parent.size()-1).asInstanceOf[Abs]
					prof = prof - pos
					
		try{breakable {
			while( i < array.length  ) {
				var c : Char = array.apply(i)
						c match {
						case  '(' => {
							if(array.apply(i+1)=='l'){
								i=parseLambdaAbs(array, i,app.args)

							}else if(array.apply(i+1) =='('){
								i=parseApplication(array, i, app.args)

							}else prof= prof +1

						}
						case  ')' =>{
							if( prof ==0){
								//i = i+ 1
								break
							}else prof = prof -1  

						}
						case _=>{
							var  terme  = new Terme()
						terme.setName(c)
						app.args.add(terme)
						}

			} // C != ' ' 
			// Incr 
			i= i + 1;	
			} // While 
		}// breakable
		}catch{
		case _ => Unit //i=i+1 
		}
		
			parent.add(app)
		//	println("i at exit  : "+i)

		return i

	}

	def parseLambdaAbs(array :  Array[Char]  , pos :Int , parent: ArrayList[ITerme] ) : Int ={
    var  abs = new Abs() 
	var isCorp : Boolean = false
	var i : Int =pos+1 
	var cpt : Int  = 0
	var isBreak : Boolean  = false;
			//	println("i at enter  : "+i) 
			try {		
				breakable {
					while(  i < array.length ){
						var c  : Char = array.apply(i)

								c match {

								case '(' => cpt = cpt+1  			    
								case  'l' => Unit 
								case '.' =>  {if(array.apply(i+1) !='l')
												isCorp=true ;
											  }	
								case ')' => { 	    			
									if(cpt != 0){
										cpt = cpt -1	
									}else {
										break
									}
								}
								case _ =>{
									var  terme  = new Terme()
								terme.setName(c)
								if(isCorp){
									abs.corps.add(terme)
								}else{
									abs.params.add(terme)
								}
								}
					}// switch 
					i=i+1;
					}
				}
			}catch {
			case  _ => Unit 

			}
			parent.add(abs)
			//println("i at exit  : "+i) 
			return i 
	}
}