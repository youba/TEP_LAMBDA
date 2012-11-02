package test
import services.ParserLambdaCalcul

object TestParser {

  def main(args: Array[String]): Unit = {
    
    val e:String="lx.x(lx.y)"
    val parser:ParserLambdaCalcul=new ParserLambdaCalcul(e)
    parser.parse(e)
      
  }
  
}