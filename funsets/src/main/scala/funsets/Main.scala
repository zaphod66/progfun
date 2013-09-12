package funsets

object Main extends App {
  import FunSets._

  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  
  val u1 = union(s1, s2)
  val u2 = union(s2, s3)
  val u3 = union(u1, u2)
  
  val i1 = intersect(u1, u2)
  val i2 = intersect(u3, u2)
  val i3 = intersect(u3, i1)
  
  val d1 = diff(u2, s2)
  val d2 = diff(u3, u2)
  val d3 = diff(d2, s2)
  
  def p1(x: Int): Boolean = (x % 2 == 0)
  def p2(x: Int): Boolean = (x >= 2)
  def p3(x: Int): Boolean = (x >= 1)
  
  val f1 = filter(u3, p1)
  val f2 = filter(u3, p2)
  val f3 = filter(u3, p3)
  
  def mf1(x: Int): Int = x;
  def mf2(x: Int): Int = x * 2;
  
  val m1 = map(u3, mf1)
  val m2 = map(u3, mf2)
  
  print(FunSets.toString(s1) + " - ")
  print(FunSets.toString(s2) + " - ")
  println(FunSets.toString(s3))
  
  print(FunSets.toString(u1) + " - ")
  print(FunSets.toString(u2) + " - ")
  println(FunSets.toString(u3))
  
  print(FunSets.toString(i1) + " - ")
  print(FunSets.toString(i2) + " - ")
  println(FunSets.toString(i3))
  
  print(FunSets.toString(d1) + " - ")
  print(FunSets.toString(d2) + " - ")
  println(FunSets.toString(d3))

  print(FunSets.toString(f1) + " - ")
  print(FunSets.toString(f2) + " - ")
  println(FunSets.toString(f3))
  
  println("================")
  
  print(FunSets.toString(m1) + " - ")
  println(FunSets.toString(m2))
}
