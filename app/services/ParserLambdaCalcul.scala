package services
import models.ILambdaTerme
import scala.util.parsing.combinator.Parsers


class ParserLambdaCalcul(chaine :String) {

  var expression :String = chaine
  
  // ici on transforme expression en une vraie expression
  
  def parse(exp:String){
    
    var long:Int=exp.length()
    var i:Int=0
    while(i<long){
      if(exp.charAt(i).equals('l')){
        println("l")
      }
      
      i=i+1
    }
    
  }
  
}