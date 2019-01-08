package com.fr.upem.partiel

import java.time.Instant
import java.time.temporal.ChronoUnit.YEARS

import cats.Monoid


// Part 1 (10pts)
object Part1 {

  // 1.1 Apply 'mul2' using pattern matching to the given integer (.5pts)
  def mul2(i: Int): Int = i * 2

  def applyMul2WithPatternMatching(i: Option[Int]): Option[Int] = i match {
    case Some(i) => Some(i * 2)
    case None => None
  }


  // 1.2 Apply 'mul2' WITHOUT using pattern matching to the given integer (.5pts)
  /*class MultipleOption(i:Option[Int]) {
    def *(x:Int) = i.map(_*x)
  }

    implicit def applyMul2WithoutPatternMatching(i: Option[Int]): Option[Int] =  {
     try {
        Some(i) = new MultipleOption(i)
     }catch
       {
         case e : Exception => None
       }
  }*/

  def applyMul2WithoutPatternMatching(i: Option[Int]): Option[Int] = {
    if (i.isDefined) Option(i.get * 2)
    else None
  }


  // 1.3 Refactor the following code using pattern matching (1pts)
  sealed trait Animal

  case object Cat extends Animal

  case object Bird extends Animal

  case class Dog(age: Int) extends Animal

  /* def formatAnimal(animal: Animal): String =
    if (animal == Cat)
      "It's a cat"
    else if (animal == Bird)
      "It's a bird"
    else if (animal.isInstanceOf[Dog])
      s"It's a ${animal.asInstanceOf[Dog].age} year old dog"
    else
      throw new RuntimeException("This should not happen but I'm a Java developer !")
*/

  def formatAnimal(animal: Animal): String = {
    animal match {
      case Cat =>
        s"it s a cat "
      case Bird =>
        s"i s a bird"
      case Dog(age) =>
        s"it s a $age year old dog "
      case _ => "nothing here ( not nessessary)"
    }
  }


  // 1.4 Find the index of the given element if any, use recursivity (1pts)
  def indexOf[A](l: List[A], a: A): Option[Int] = l match {
    case Nil => None
    case x :: xs if x == a => Option(0)
    case x :: xs => indexOf(xs, a) match {
      case Some(i) => Option(i + 1) // je ss pas sur Option(i-1)
      case None => None
    }

  }


  // 1.5 Throw away all errors (.5pts)
  case class Error(message: String)

  def keepValid[A](l: List[Either[Error, A]]): List[A] = l match {
    case Nil => List()
    case x :: xs => x match {
      case Right(value) => value :: keepValid(xs)
      case Left(_) => keepValid(xs)
    }
  }


  // 1.6 Aggregate values (.5pts)
  def aggregate[A](l: List[A], combine: (A, A) => A, empty: A): A = {
    if (l == Nil) empty
    else l.map(x => x).reduce(combine)
  }

  // 1.7 Aggregate valid values (.5pts)
  def aggregateValid[A](l: List[Either[Error, A]], combine: (A, A) => A, empty: A): A = {
    if (l == Nil) empty
    else l.collect {
      case Right(v) => v
    }.reduce(combine)
  }


  // 1.8 Create the Monoid typeclass and rewrite the above "aggregateValid" (.5pts)

  trait Operation[A]{
    def combine(x: A, y: A): A
    def empty
  }

  /*case class Foo(i:String , j: String)
  implicit val OperationMonoidStrings[Foo]{
    def  combine(x: Foo, y : Foo): Foo = (Foo ( x.i , y.j , s.y.concat(s.x)))

}*/

/*
  case class op(x: String, y: String)
  object Position {
    implicit val monoid: Monoid[op] = new Monoid[op] {
      override def empty = Position(0, 0)

      override def combine(p1: op, p2: op) = op(p1.x + p2.x, p1.y + p2.y)
    }
*/

  def aggregateValidM = ???



  // 1.9 Implement the Monoid typeclass for Strings and give an example usage with aggregateValidM (.5pts)

  // 1.10 Refactor the following object oriented hierarchy with an ADT (1.5pts)
  abstract class FinancialAsset {
    def computeEarnings: Double
  }

  abstract class FlatRateAsset extends FinancialAsset {
    protected val rate: Double
    protected val amount: Double

    override def computeEarnings: Double = amount + (amount * rate)
  }

  object LivretA {
    val Rate: Double = 0.75
  }

  class LivretA(override val amount: Double) extends FlatRateAsset {
    override protected val rate: Double = LivretA.Rate
  }

  object Pel {
    val Rate: Double = 1.5
    val GovernmentGrant: Int = 1525
  }

  class Pel(override val amount: Double, creation: Instant) extends FlatRateAsset {
    override protected val rate: Double = Pel.Rate
    override def computeEarnings: Double =
      if (Instant.now().minus(4, YEARS).isAfter(creation))
        super.computeEarnings + Pel.GovernmentGrant
      else
        super.computeEarnings
  }

  object CarSale {
    val StateHorsePowerTaxation: Int = 500
  }
  class CarSale(amount: Int, horsePower: Int) extends FinancialAsset {
    override def computeEarnings: Double = amount - (CarSale.StateHorsePowerTaxation * horsePower)
  }

  // 1.11 Extract the "computeEarnings" logic of the above hierarchy
  // into an "Earnings" typeclass and create the adequate instances (1.5pts)

  // => monode illustration

  // 1.12 Rewrite the following function with your typeclass (.5pts)
  def computeTotalEarnings(assets: List[FinancialAsset]): Double =
    assets.map(_.computeEarnings).sum



  // 1.13 Enrich the "String" type with an "atoi" extension method that parses the
  // given String to an Int IF possible (1pts)
  sealed trait simpletTrait
  final case class Integ(a: Integer) extends simpletTrait
  final case class Str(a: String) extends simpletTrait
  implicit class StrUpdate(val a: String) extends AnyVal {
    def atoi: simpletTrait =
      if (a.matches("[0-9]+$")) Integ(Integer.parseInt(a))
      else Str(a)
  }


}
