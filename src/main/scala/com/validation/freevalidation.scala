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
    def validate:Option[Error]

    def unbox:A
  }
  case class NameValidator(name:String) extends Validator[String] {
    def validate  =  if (name.isEmpty) Option(NameError) else None
    def unbox:String = name
  }
  case class AgeValidator(age:Int) extends Validator[Int] {
    def validate= if (age >= 18) None else Some(AgeError)
     def unbox:Int = age

  }
  case class NameAgeValidator(nameage:NameAge) extends Validator[NameAge] {
    def validate= None
    def unbox:NameAge = nameage
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
    def exec[A](fa: F[A]): Option[Error]
    def unbox[A](fa: F[A]):A
  }
  val validators = new  Executor[Validator] {
    override def unbox[A](fa: Validator[A]) = fa.unbox
    override def exec[A](fa: Validator[A]) = fa.validate
  }
  val person  = NameAge("John",10)
  val validation = for {
    _ <- NameValidator(person.name)
    _  <- AgeValidator(person.age)
  } yield ()

  val x = validate(List.empty[Option[Error]], validation, validators)
  if (x._1.isEmpty) println(save(person)) else  x._1.foreach(println)

  def save(name:String, age:Int):Boolean = {
    println(s"save $name at age $age")
    true
  }
  def save(nameage:NameAge):Boolean = {
    println(s"save ${nameage.name} at age ${nameage.age}")
    true
  }

  def validate[F[_], A](errorList: List[Option[Error]], prg: Free[F, A], executor: Executor[F]): (List[Option[Error]], Option[A]) = {
    prg match {
      case Return(a) => (errorList, Some(a))
      case FlatMap(sub, cont) => validate(executor.exec(sub) :: errorList, cont(executor.unbox(sub)), executor)
    }
  }
}
