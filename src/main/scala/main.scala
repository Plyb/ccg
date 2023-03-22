import scala.collection.mutable._

object main extends App {
  implicit def stringToConstituent(s : String) = Primitive(s)

  val S = "S"
  val NP = "NP"
  val VP = "VP"

  def printExample(example: List[Constituent]): Unit = {
    println(example)
    println(Parser.parse(example))
    println()
  }

  def example1: List[Constituent] = {
    val who = S/(S/NP)
    val relativesOf = (S/(S\NP))/NP
    val thought = (S\NP)/S
    val Kim = S/(S\NP)
    val eliminated = (S\NP)/NP

    List(who, relativesOf, thought, Kim, eliminated)
  }

  def example2: List[Constituent] = {
    val everyone = S/(S\NP)
    val sees = (S\NP)/NP
    val herself = (S\NP)\((S\NP)/NP)

    List(everyone, sees, herself)
  }

  def example3Fail: List[Constituent] = {
    val everyone = S/(S\NP)
    val sees = (S\NP)/NP
    val sheself = S/(S\NP)

    List(everyone, sees, sheself)
  }

  def example4: List[Constituent] = {
    val everyone = S/VP
    val thinks = VP/S
    val he_bound = (VP\(VP/S))/(S\NP)
    val likesJohn = S\NP

    List(everyone, thinks, he_bound, likesJohn)
  }

  def example5: List[Constituent] = {
    val everyone = S/VP
    val thinks = VP/S
    val john = S/VP
    val likes = VP/NP
    val him_bound = (VP\(VP/S))\(S/NP)

    List(everyone, thinks, john, likes, him_bound)
  }

  def example6: List[Constituent] = {
    val left = S\NP
    val before = ((S\NP)\(S\NP))/S
    val maryDid = S/(S\NP)

    List(left, before, maryDid)
  }

  printExample(example1)
  printExample(example2)
  printExample(example3Fail)
  printExample(example4)
  printExample(example5)
  printExample(example6)
}