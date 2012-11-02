package models

/**
 * def main(args: Array[String]): Unit = {
    var lambda  : LambdaTerme = new LambdaTerme("hello scala")

    println(lambda)

  }

 */
import scala.util.control.Breaks._

object LambdaParser {

	def parseLambda(exp  : String): ILambdaTerme= {
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
	  
	
	
	var app :ApplicationL = new ApplicationL()
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
						if(app.abs == null){
							i = parseLambdaAbs(chars, i, app, false)	
									isArgs =true
						}else 
							i = parseLambdaAbs(chars, i, app, true)

					}else if(chars.apply(i+1) == '('){
						if(app.abs == null)
							i= parseApplication(chars, i,app,false)
							else 
								i= parseApplication(chars, i,app,true)

					}

				}
				case ')' =>  Unit
				case '.' =>  Unit

				case _ =>{
					if(isArgs){
						var lmd  : LambdaTerme  = new LambdaTerme()
					lmd.rename(current)
					app.args.add(lmd)
					}
				}
	}
	pred=current	
			i = i+ 1
	}
	if(app.args.isEmpty()){
		if(app.abs.corps.isEmpty())
			return app.abs.params.get(i)
					else return app.abs
	}else
		return app
	}



	def parseApplication( array : Array[Char],pos : Int , parent: ApplicationL,isArg:Boolean): Int = {
			var app :  ApplicationL = new ApplicationL()
	var prof : Int  = pos 
	while (array.apply(prof+1) == '(' ) {
		prof = prof +1 
	}

			var i : Int = parseLambdaAbs(array, prof, app, false)
					prof = prof - pos
					
		try{breakable {
			while( i < array.length  ) {
				var c : Char = array.apply(i)
						c match {
						case  '(' => {
							if(array.apply(i+1)=='l'){
								i=parseLambdaAbs(array, i, app, true)

							}else if(array.apply(i+1) =='('){
								i=parseApplication(array, i, app, true)

							}else prof= prof +1

						}
						case  ')' =>{
							if( prof ==0){
								//i = i+ 1
								break
							}else prof = prof -1  

						}
						case _=>{
							var  terme  : LambdaTerme = new LambdaTerme()
						terme.rename(c)
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
		if(isArg){
			parent.args.add(app)
		}else {
			parent.abs =  app.abs 
					parent.args = app.args
		}

		//	println("i at exit  : "+i)

		return i

	}

	def parseLambdaAbs(array :  Array[Char]  , pos :Int , parent: ApplicationL , isArg :Boolean) : Int ={
			var  abs : LambdaAbs = new LambdaAbs() 
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
									var  terme  : LambdaTerme = new LambdaTerme()
								terme.rename(c)
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
			if(isArg){
				parent.args.add(abs)
			}else parent.abs= abs
			//println("i at exit  : "+i) 
			return i 
	}



}