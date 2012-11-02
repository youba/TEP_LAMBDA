package test
import models.Tools
import models.LambdaParser
import models.ILambdaTerme
import models.LambdaEvaluator
import java.util.ArrayList

object Main {
 
  def main(args: Array[String]): Unit = {
 
    Tools.initAlphabet
    
    println("Please enter lambda expression :")
     var expr : String  =  readLine()
     println("Parsing ......")
     var lambda  : ILambdaTerme = LambdaParser.parseLambda(expr)
     println("Display Result : ")
     println(lambda.toString())
     var evaluator : LambdaEvaluator = new LambdaEvaluator
   //  println("Test Alpha Conversion : "+evaluator.alphaConversion(lambda).toString())
    // println("Test Beta Reduction : "+evaluator.betaReduction(lambda).toString())
     val etapes:ArrayList[ILambdaTerme] = new ArrayList[ILambdaTerme]()
//     println("-------------------------")
     evaluator.evalManager(1,lambda,etapes)
     println(etapes.size())
//      println("-------------------------")
//     val trois:ILambdaTerme= evaluator.evalManager(1,deux,etapes)
     for(i<-0 until 20){
        println("-----------Etape "+(i+1)+"--------------")
       evaluator.evalManager(1,etapes.get(i+1),etapes)
     }
     
     
  }

}