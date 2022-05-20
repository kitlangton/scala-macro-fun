package example

import scala.reflect.macros.blackbox

object Debug {
  def debug[A](value: A): Unit =
    macro Macros.debugImpl[A]
}

class Macros(val c: blackbox.Context) {
  import c.universe._

  // context-bound
  def debugImpl[A: WeakTypeTag](value: Expr[A]): Tree = {
    val tpe = c.weakTypeOf[A]
    val result = value.tree match {
      case Select(This(_), TermName(name)) =>
        val separator = s": ${show(tpe)} = "
        q"println($name + $separator + $value)"
      case tree =>
        val separator = s": ${show(tpe)} = "
        q"println(${show(tree)} + $separator + $value)"
    }

//    panic(result)
    result
  }

  def panic(tree: Tree): Nothing = {
    val message =
      s"""
MACRO DEBUG
===========
show:    ${show(tree)}
showRaw: ${showRaw(tree)}
        """
    throw new Error(message)
  }

}
