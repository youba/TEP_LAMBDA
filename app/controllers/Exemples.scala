package controllers
import play.api.mvc.Controller
import play.api.mvc.Action

import play.api.templates.Html

import models.LambdaParser
import models.ILambdaTerme
import models.Tools
import java.util.ArrayList
import models.LambdaEvaluator
import models.ITerme
import models.LParser
import models.LambdaEvaluator2
import models.Copyer



object Exemples extends Controller  {

	var etapes : ArrayList[ArrayList[ITerme]]=null
	var terme : ArrayList[ITerme]=null
	var termeDebut :ArrayList[ITerme]=null
  
	def initEtapes()={
	  this.etapes = new ArrayList[ArrayList[ITerme]]()
	}
	
  
   
   def doNothing =Action{
     Ok(views.html.index("",null,null,null,null,false,false))
   }
   
   def beginReduction(ex : String)=Action{
     Tools.initAlphabet
     initEtapes()
     terme = LParser.parseLambda(ex)
     termeDebut= Copyer.copyArrayList(terme)
     etapes.add(termeDebut)
	 
     var h:Html = AfficheurCroco.stringToHTML(AfficheurCroco.termeToHTMLString(terme))
     Ok(views.html.index("",h,"Expression a reduire : "+this.afficheListITerme(termeDebut),"","",true,false))
   }
   
   def nextStep()=Action{
    try{
    LambdaEvaluator2.evalManager(1,etapes.get(etapes.size()-1),etapes)
	var h:Html = AfficheurCroco.stringToHTML(AfficheurCroco.termeToHTMLString(etapes.get(etapes.size()-1)))
      Ok(views.html.index("",h,"Expression a reduire : "+this.afficheListITerme(termeDebut),"Etape precedente : "+this.afficheListITerme(etapes.get(etapes.size()-2)),"Reduction : "+this.afficheListITerme(etapes.get(etapes.size()-1)),true,true))
	}catch{
	    case _ =>  Ok(views.html.index("",null,"Expression a reduire : "+this.afficheListITerme(termeDebut),"Etape precedente : "+this.afficheListITerme(etapes.get(etapes.size()-1)),"Reduction : "+this.afficheListITerme(etapes.get(etapes.size()-1)),false,true))
		  }
    
   }
   
   def lastStep()=Action{
	LambdaEvaluator2.evalManager(2,etapes.get(etapes.size()-1),etapes)
    if(etapes.size()==1){
      var h:Html = AfficheurCroco.stringToHTML(AfficheurCroco.termeToHTMLString(etapes.get(0)))
       Ok(views.html.index("",h,"Expression a reduire : "+this.afficheListITerme(termeDebut),"Etape precedente : "+this.afficheListITerme(terme),"Reduction : "+this.afficheListITerme(etapes.get(0)),true,false))
    }else{
    	var h:Html = AfficheurCroco.stringToHTML(AfficheurCroco.termeToHTMLString(etapes.get(etapes.size()-2)))
    	Ok(views.html.index("",h,"Expression a reduire : "+this.afficheListITerme(termeDebut),"Etape precedente : "+this.afficheListITerme(etapes.get(etapes.size()-2)),"Reduction : "+this.afficheListITerme(etapes.get(etapes.size()-1)),true,true))
    	}
    }
   
   // TODO World to Html appel dans un service
   
  def afficheListITerme(terme :ArrayList[ITerme]):String={
    var s:String =""
    for(i<-0 until terme.size()){
      s=s+terme.get(i).toString
    }
    return s
  }
   // controleur Expression to World[Family]
   
   
}