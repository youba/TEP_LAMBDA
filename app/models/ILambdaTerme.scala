package models

abstract class ILambdaTerme {

	def isAbstraction():Boolean;	
	def isApplication():Boolean;
	
	def equals(terme:ScalaObject):Boolean;
	def toString():String;
    def  cloneTerme():ILambdaTerme;
    def containsTerme(c:Char):Boolean;
}