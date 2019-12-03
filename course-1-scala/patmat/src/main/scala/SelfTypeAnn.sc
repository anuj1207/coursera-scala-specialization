trait B {
  val name: String
}

class A{
  this: B =>
  val name = "hurrr"
  def print(): Unit = println(name)
}

trait B1 extends B{
  val name: String = "Anuj"
  println("Instantiated successfully with anuj")
}

val a = new A with B1

a.print()
println(a.name)