package models
import java.util.ArrayList

class LambdaEvaluator {

	

	// valeur de action:
	//1=next step
	//2=last step
	//3=final step
	def evalManager(action:Int, terme:ILambdaTerme,etapes:ArrayList[ILambdaTerme]):ILambdaTerme={

			action match{
			case 1 =>if(etapes.isEmpty())
			{
				etapes.add(terme)
				var nextTerme :ILambdaTerme = this.nextStep(terme)
				etapes.add(nextTerme)
				return nextTerme
			}
			if(!terme.equals(etapes.get(etapes.size()-1))){
				var nextTerme :ILambdaTerme = this.nextStep(terme)
						etapes.add(nextTerme)
						return nextTerme
			}
			
			var nextTerme :ILambdaTerme = this.nextStep(terme)
					etapes.add(nextTerme)
					return nextTerme
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

	
	def nextStep(terme:ILambdaTerme):ILambdaTerme={
			// DEBUG  
			println("nextStep in : "+terme)

			var alphaterme =  alphaConversion(terme)
			var  stepRes : ILambdaTerme  =  null ;
			var  cpt  : Int  =  0 ;
			var  stop : Boolean  = false;  
			println("Conversion "+alphaterme)
			if(alphaterme.isAbstraction()){
				println("Fin DU NIVEAU 1")
				var abs  :LambdaAbs = alphaterme.asInstanceOf[LambdaAbs]
						abs.corps = makeApplication(abs.corps)
				while(cpt <  abs.corps.size() &&  !stop){
					  var res : ILambdaTerme  = abs.corps.get(cpt) ;
					  if(res.isApplication()){
						  println("print result of makeApplication---------------")
						  println(res.toString())
						  stepRes = betaReduction(res) ;
						  stop = true ;
					  }
					  else 
					    cpt = cpt + 1 
					}
				if( stepRes != null){ // Replace 
					abs.corps.add(cpt,stepRes)
					abs.corps.remove(cpt+1);
				}
				return alphaterme 

			}else if(terme.isApplication()){
				return betaReduction(alphaterme) 
			}else 
				return alphaterme ;	

	}


	

	//il faut aussi checker le corps apres que la modif en fonction des parametre ai ete faite
	def alphaConversion(terme:ILambdaTerme):ILambdaTerme={
			if(terme.isApplication()){
				var result: ApplicationL = terme.cloneTerme().asInstanceOf[ApplicationL]
						for(i <- 0 until result.abs.params.size()){	 //on regarde pour chaque param s'il a apparait dans les args, si oui on le renomme (ainsi que ses occurence dans le corps) par le 1terme libre
							for(j<-0 until result.args.size()){
								if(result.args.get(j).containsTerme(result.abs.params.get(i).nom)){
									var oldChar : Char =result.abs.params.get(i).nom
											var newChar : Char  = Tools.getFirstFree
											result.abs.params.get(i).rename(newChar)
											for(k<-0 until result.abs.corps.size()){
												if(result.abs.corps.get(k).isInstanceOf[LambdaTerme]){ 
													var elCorps :LambdaTerme = result.abs.corps.get(k).asInstanceOf[LambdaTerme]
															if(elCorps.nom.equals(oldChar)){
																elCorps.rename(newChar)
															}
												}
											}
								}
							}
						}
			return result
			}
			return terme
	}

	//si le corps est une evaluation alors on fait beta reduction sur le corps?
	// a priori non, c'est gerer en dehors de cette fonction dans l'algo global
	def betaReduction (terme : ILambdaTerme):ILambdaTerme={
			if(terme.isApplication()){
				var te:ApplicationL = terme.asInstanceOf[ApplicationL]
						var result:ApplicationL = te.cloneTerme().asInstanceOf[ApplicationL]
								var t :ApplicationL =result.asInstanceOf[ApplicationL]// si c'est une application alors il faut caster
										var p1:LambdaTerme = t.abs.params.get(0)
										if(t.abs.estDansCorp(p1)){//si le premier param est dans le corp alors il faut substituer dans le corp, la valeur du param par l'argument i
											for(j<-0 until t.abs.corps.size())
											{
												if(t.abs.corps.get(j).isInstanceOf[LambdaTerme]){
													if(p1.nom.equals(t.abs.corps.get(j).asInstanceOf[LambdaTerme].nom)){// on substitue par l'argument i dans le corps
														t.abs.corps.set(j,t.args.get(0)) 
													}
												}
											} 
										}
								t.abs.params.remove(0)
								t.args.remove(0)
								if(t.args.isEmpty()){
									if(t.abs.params.isEmpty()){ 
										if(t.abs.corps.size()==1){
											return t.abs.corps.get(0)
										}
										var lt:LambdaAbs = new LambdaAbs
												lt.corps=t.abs.corps
												return lt     //ajouter un niveau type qui serait liste de terme?
									}else{
										//Checker ici si le premier element du corps 
										var l:LambdaAbs=new LambdaAbs
												l.corps=t.abs.corps
												l.params=t.abs.params
												// l.corpsAbstractionToApplication()
												return l 
									}
								}
								return t
			}else{
				// on va en profondeur
			}
			return terme 		// regarder en profondeur si il y a une application
	}

	def makeApplication(corps :ArrayList[ILambdaTerme]) : ArrayList[ILambdaTerme]={
			var res : ArrayList[ILambdaTerme] = new ArrayList[ILambdaTerme]() 
					var  max  : Int  = corps.size()
					var  app  : ApplicationL = new ApplicationL()
					var isAbsArgs  : Boolean  = false; 
					for( i <- 0  until  max){
						var   terme : ILambdaTerme = corps.get(i)
								if(terme.isApplication())
									res.add(terme)
									else if( terme.isAbstraction()){
										if(app.abs == null){
											app.abs = terme.asInstanceOf[LambdaAbs]
													isAbsArgs=true
										}else if(isAbsArgs){
											app.args.add(terme)
										} else 
											res.add(terme)
									}else{
										if( isAbsArgs  ){
											app.args.add(terme)
											if(app.abs.params.size() == app.args.size()){
												isAbsArgs = false
														res.add(app)
											}
										}else
											res.add(terme)
									} 
					}

					return res
	} 



	def getAppInsideAbs(abs:LambdaAbs):ApplicationL={
			

			return null
	}



}