package controllers
import java.util.ArrayList
import models.ITerme
import play.api.templates.Html
import models.Terme
import models.Abs
import models.App

object AfficheurCroco {

  
  def termeToHTMLString(terme:ArrayList[ITerme]):String={
   var s:String=""
   for(i<-0 until terme.size()){
     s=s+"<td>"+this.unTermeToHTMLString(terme.get(i),false)+"</td>"
   }
   return s
    
  }
  
  //Affiche One terme
  // boolean indique si c'est un parametre
  //true = parametre -> donc croco
  //false = variable -> donc oeuf
  def unTermeToHTMLString(terme : ITerme,b:Boolean):String={
    var s:String=""
    if(terme.isInstanceOf[Terme])
    {
      if(b)
    	return s+imageParam(terme.asInstanceOf[Terme])
      else
        return s + imageVariable(terme.asInstanceOf[Terme])
    }
   if(terme.isAbs()){
     var abs:Abs =terme.asInstanceOf[Abs]
     for(i<-0 until abs.params.size()){
        s=s+"<br/>"+unTermeToHTMLString(abs.params.get(i),true)
     }
     for(i<-0 until abs.corps.size()){
       s=s+"<br/>"+unTermeToHTMLString(abs.corps.get(i),false)
     }
     return s
   }
   if(terme.isApp()){
     var ap:App=terme.asInstanceOf[App]
     s=s+unTermeToHTMLString(ap.abs,false)
     for(i<-0 until ap.args.size()){
       s=s+unTermeToHTMLString(ap.args.get(i),false)
     }
     return s
   }
    return s
  }
  
  //getImagefrom terme
  def imageParam(terme:Terme):String={
    var s="<img src=\"/assets/images/alligator/Alligator"+(terme.nom-97)+".jpg\"/>"
    		
    return s
  }
  
  def imageVariable(terme:Terme):String={
    var s="<img src=\"/assets/images/egg/Egg"+(terme.nom-97)+".jpg\"/>"
      
    return s 
  }
  
  def stringToHTML(s:String):Html={
    return new Html(s)
  }
}