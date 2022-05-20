package example

import magnolia1._

trait Monoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}

object Monoid {
  def combine[A](a1: A, a2: A)(implicit monoid: Monoid[A]): A =
    monoid.combine(a1, a2)

  def smoosh[A](as: List[A])(implicit monoid: Monoid[A]): A =
    as.foldLeft(monoid.empty)(monoid.combine)

  def foldMap[A, B](as: List[A])(f: A => B)(implicit monoid: Monoid[B]): B =
    as.foldLeft(monoid.empty) { (acc, a) =>
      monoid.combine(acc, f(a))
    }

  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    def combine(a1: Int, a2: Int): Int = a1 + a2
    def empty: Int                     = 0
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def combine(a1: String, a2: String): String = a1 + a2
    def empty: String                           = ""
  }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def combine(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def empty: List[A]                             = Nil
  }
}

object DeriveMonoid {
  type Typeclass[A] = Monoid[A]

  def join[A](ctx: CaseClass[Monoid, A]): Monoid[A] =
    new Monoid[A] {
      override def combine(a1: A, a2: A): A =
        // Person(name: String, age: Int)
        ctx.construct { param =>
          // name: String
          // p1.name
          val value1 = param.dereference(a1)
          // p2.name
          val value2 = param.dereference(a2)
          // Monoid[String].combine(p1.name, p2.name)
          param.typeclass.combine(value1, value2)
        }

      override def empty: A =
        ctx.construct { param =>
          param.typeclass.empty
        }
    }

  implicit def gen[A]: Monoid[A] =
    macro Magnolia.gen[A]
}

final case class Average(total: Int, count: Int) {
  def calculate: Int = total / count
}

object Average {
  implicit val monoid: Monoid[Average] = DeriveMonoid.gen[Average]
}

object MonoidExamples extends App {
  final case class NewPerson(name: String, age: Int, luckyNumbers: List[Int])
  implicit val personMonoid: Monoid[NewPerson] = DeriveMonoid.gen[NewPerson]

  val combined = Monoid.foldMap(
    List(
      NewPerson("John", 30, List(1, 3, 5)),
      NewPerson("Mary", 25, List(2, 4, 6)),
      NewPerson("Mike", 20, List(7, 8, 9)),
      NewPerson("John", 30, List(1, 3, 5)),
      NewPerson("Kit", 25, List(2, 4, 6))
    )
  )(identity)

  println(combined)
}

trait Show[A] {
  def show(a: A): String
}

object Show {
  def apply[A](implicit show: Show[A]): Show[A] = show

  def show[A: Show](value: A): String =
    Show[A].show(value)

  implicit val intShow: Show[Int] = _.toString

  implicit val stringShow: Show[String] =
    (string: String) => s"\"$string\""

  implicit val booleanShow: Show[Boolean] = _.toString

  implicit def listShow[A](implicit showA: Show[A]): Show[List[A]] =
    _.map(showA.show).mkString("List(", ", ", ")")
}

sealed trait SecureField extends Product with Serializable

object SecureField {

  final case class SocialSecurityNumber(int: Int)       extends SecureField
  final case class FavoriteIceCreamFlavor(name: String) extends SecureField

//  implicit val show: Show[SecureField] =
//    _ => s"<REDACTED>"
}

object DeriveShow {
  type Typeclass[A] = Show[A]

  def join[A](ctx: CaseClass[Show, A]): Show[A] =
    new Show[A] {
      override def show(a: A): String = {
        val labeledParameters = ctx.parameters.map { param =>
          val value      = param.dereference(a)
          val shownValue = param.typeclass.show(value)
          s"${param.label} = $shownValue"
        }
        s"${ctx.typeName.short}(${labeledParameters.mkString(", ")})"
      }
    }

  def split[A](ctx: SealedTrait[Show, A]): Show[A] =
    new Show[A] {
      def show(value: A): String =
        ctx.split(value) { sub =>
          sub.typeclass.show(sub.cast(value))
        }
    }

  implicit def gen[A]: Show[A] =
    macro Magnolia.gen[A]
}

final case class Person(name: String, age: Int, ssn: SecureField)

object Person {
  implicit val showPerson: Show[Person] = DeriveShow.gen[Person]
//  (person: Person) => {
//    val shownName = Show.show(person.name)
//    val shownAge  = Show.show(person.age)
//    val showSSN   = Show.show(person.ssn)
//    s"Person(name = $shownName, age = $shownAge, ssn = $showSSN)"
//  }
}

final case class Dog(name: String, hasBone: Boolean, dogSsn: SecureField)

object Dog {
  implicit val showDog: Show[Dog] = DeriveShow.gen[Dog]
//    (dog: Dog) => {
//    val shownName = Show.show(dog.name)
//    val shownBone = Show.show(dog.hasBone)
//    val showSSN   = Show.show(dog.dogSsn)
//    s"Dog(name = $shownName, hasBone = $shownBone, dogSsn = $showSSN)"
//  }
}

object ShowExamples extends App {
  val people = List(
    Person("John", 42, SecureField.SocialSecurityNumber(123456789)),
    Person("Mary", 23, SecureField.FavoriteIceCreamFlavor("Elderberry (Beef) Tatar"))
  )

  val dogs = List(
    Dog("Fido", true, SecureField.SocialSecurityNumber(123456789)),
    Dog("Spot", false, SecureField.SocialSecurityNumber(987654321))
  )

  println(Show.show(people))
  println(Show.show(dogs))
}
