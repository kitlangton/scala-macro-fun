package example

// Generating forms at compile-time
// type-class instances

object DebugTest extends App {
  // parse -> type -> optimize -> MACRO -> moreStuff -> generates Java
  final case class Person(name: String)
  val kit = Person("kit")
  Debug.debug(kit)
}

object Constants {
  val thing = "thing"
}
