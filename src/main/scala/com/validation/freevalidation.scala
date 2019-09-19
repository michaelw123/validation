package com.validation
import scala.util.{Try, Success, Failure}

object freevalidation extends App {
  sealed trait Free[F[_], A]  {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
      case Return(a) => f(a)
      case FlatMap(sub, cont) => {
        FlatMap(sub, cont andThen (_ flatMap f))
      }
    }
    def  map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))
  }
  final case class Return[F[_], A](a: A) extends Free[F, A]
  case class FlatMap[F[_], I, A](sub: F[I], cont: I => Free[F, A]) extends Free[F, A]

  implicit def liftF[F[_], A](fa: F[A]): Free[F, A] = FlatMap(fa, Return.apply)

  case class NameAge(name:String, age:Int)
  sealed trait Validator[A] {
    def validate(arg:A):Either[Error, A]
    def validate:Either[Error, A]
  }
  case class NameValidator(name:String) extends Validator[String] {
    def validate (name:String) =  if (name.isEmpty) Left(NameError) else Right(name)
    def validate:Either[Error, String] = if (name.isEmpty) Left(NameError) else Right(name)
  }
  case class AgeValidator(age:Int) extends Validator[Int] {
    def validate(age: Int) = if (age >= 18) Right(age) else Left(AgeError)
    def validate:Either[Error, Int]= if (age >= 18) Right(age) else Left(AgeError)
  }
  case class NameAgeValidator(nameage:NameAge) extends Validator[NameAge] {
    def validate(nameage: NameAge) = Right(nameage)
    def validate:Either[Error, NameAge]= Right(nameage)
  }
  object Validator {
    def unliftV[Validator[A], A](v: Validator[A]) = {
      v match {
        case nv: NameValidator => nv.name
        case av: AgeValidator => av.age
        case nav: NameAgeValidator => nav.nameage
      }
    }
  }
  trait Error {
    def errorCode:Int
    def errorMsg:String
  }
  case object AgeError extends Error {
    val errorCode = 0
    val errorMsg = "Illegal Age"
  }
  case object NameError extends Error {
    val errorCode = 1
    val errorMsg = "Illegal Name"
  }
  sealed trait Executor[F[_]] {
    def exec[A](fa: F[A]): Either[Error, A]
  }
  val validators = new  Executor[Validator] {
    override def exec[A](fa: Validator[A]):Either[Error, A] = fa.validate
  }
  val person  = NameAge("Joe Doe",20)
  val validation = for {
    _ <- NameValidator(person.name)
    _  <- AgeValidator(person.age)
  } yield ()

  val x = validate(List.empty[Error], validation, validators)
  if (x._1.isEmpty) println(save(person)) else  x._1.foreach(println)

  def save(name:String, age:Int):Boolean = {
    println(s"save $name at age $age")
    true
  }
  def save(nameage:NameAge):Boolean = {
    println(s"save ${nameage.name} at age ${nameage.age}")
    true
  }

  def validate[F[_], A](errorList: List[Error], prg: Free[F, A], executor: Executor[F]): (List[Error], Option[A]) = {
    prg match {
      case Return(a) => (errorList, Some(a))
      case FlatMap(sub, cont) =>
        val z = executor.exec(sub)
        z match {
          case Right(a) => validate(errorList, cont(a), executor)
          case Left(e) => validate(errorList :+ e, cont(Validator.unliftV(sub)), executor)
        }
 //      executor.exec(sub).flatMap(x => validateAndRun(cont(x), executor))
    }
  }
}
