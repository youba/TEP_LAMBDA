package tests
import models.ApplicationL
import models.LambdaTerme
import models.LambdaAbs
import models.LambdaEvaluator
import models.Tools
import models.ILambdaTerme
import java.util.ArrayList

object TestReduction {

  
  def main(args: Array[String]): Unit = {
   Tools.initAlphabet // A TOUJOURS FAIRE
   
   val app : ApplicationL=new ApplicationL()
    
   val arg1 : LambdaTerme = new LambdaTerme()
   arg1.rename('a')
   val arg2 : LambdaTerme = new LambdaTerme()
   arg2.rename('r')
   val p1 : LambdaTerme = new LambdaTerme()
   p1.rename('a')
   val p2 : LambdaTerme = new LambdaTerme()
   p2.rename('b')
   val c1 : LambdaTerme = new LambdaTerme()
   c1.rename('a')
   val c2 : LambdaTerme = new LambdaTerme()
   c2.rename('b')
   
   val abs : LambdaAbs = new LambdaAbs()
   
   abs.params.add(p1)
   abs.params.add(p2)
   abs.corps.add(c1)
   abs.corps.add(c2)
   
   app.abs=abs
   app.args.add(arg1)
   app.args.add(arg2)
   
   println(app.toString())
   
   val evaluator : LambdaEvaluator=new LambdaEvaluator()
//   val etape1:ILambdaTerme=evaluator.nextStep(app)
//   evaluator.nextStep(etape1)
   val etapes:ArrayList[ILambdaTerme] = new ArrayList[ILambdaTerme]()
   val etape1:ILambdaTerme=evaluator.evalManager(1,app,etapes)
   println("Etapes : "+etapes.toString())
   println(etape1.toString())
   val etape2:ILambdaTerme=evaluator.evalManager(2,etape1,etapes)
//   println("Etapes : "+etapes.toString())
   println(etape2.toString())
//   println(evaluator.evalManager(2,etape2,etapes).toString())
  }
}