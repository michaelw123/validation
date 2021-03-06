package com.validation
import scala.util.{Try, Success, Failure}

object freevalidation extends App {

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
      case Return(a) => f(a)
      case FlatMap(sub, cont) => FlatMap(sub, cont andThen (_ flatMap f))
    }

    def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))
  }
  final case class Return[F[_], A](a: A) extends Free[F, A]
  case class FlatMap[F[_], I, A](sub: F[I], cont: I => Free[F, A]) extends Free[F, A]

  implicit def liftF[F[_], A](fa: F[A]): Free[F, A] = FlatMap(fa, Return.apply)

  case class Person(name: String, age: Int)

  sealed trait Validator[A] {
    def validate: Option[Error]
    def unbox: A
  }

  case class NameValidator(name: String) extends Validator[String] {
    def validate = if (name.isEmpty) Some(NameError) else None
    def unbox: String = name
  }

  case class AgeValidator(age: Int) extends Validator[Int] {
    def validate = if (age >= 18) None else Some(AgeError)
    def unbox: Int = age
  }

  abstract class Error (errorCode:Int, errormsg:String)
  case object AgeError extends Error(errorCode=0, errormsg = "Illegal Age")
  case object NameError extends Error (errorCode=1, errormsg = "Illegal Name")

  sealed trait Executor[F[_]] {
    def exec[A](fa: F[A]): Option[Error]
    def unbox[A](fa: F[A]): A
  }

  val interpreter  = new Executor[Validator] {
    override def unbox[A](fa: Validator[A]) = fa.unbox
    override def exec[A](fa: Validator[A]) = fa.validate
  }
  val person = Person("John", 20)
  val validation = for {
    _ <- NameValidator(person.name)
    _ <- AgeValidator(person.age)
  } yield ()

  validate(validation, interpreter ) match {
    case Nil => save(person)
    case errors => errors foreach println
  }

  def save(nameage: Person): Boolean = {
    println(s"save ${nameage.name} at age ${nameage.age}")
    true
  }

  def validate[F[_], A](prg: Free[F, A], interpreter : Executor[F]): List[Error] = {
    def go(errorList: List[Option[Error]], prg: Free[F, A]): List[Option[Error]]= prg match {
      case Return(a) => errorList
      case FlatMap(sub, cont) => go(interpreter.exec(sub) :: errorList, cont(interpreter.unbox(sub)))
    }
    go(List.empty[Option[Error]], prg).flatten
  }
}
