package models
import java.util.ArrayList

object Tools {

  //'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'
  //faire un alphabet
  // des qu'on cree un lambda terme on enleve la lettre correspondant a son nom
  // idem lors de l'alphaconversion
  
  val Alphabet : ArrayList[Boolean]=new ArrayList(26)
  
  def initAlphabet{
    Alphabet.clear()
    for(i<-0 until 26){
      Alphabet.add(true)
    }
  }
  
  
  
  def getFirstFree:Char={
    for(i<-0 until Tools.Alphabet.size){
      if(Tools.Alphabet.get(i)){
        return (i+97).asInstanceOf[Char]
      }
    }
    
    return 'a'
  }
}