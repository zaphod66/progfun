package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.8/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
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
    
    def p1(x: Int): Boolean = true
    def p2(x: Int): Boolean = false
    def p3(x: Int): Boolean = (x % 2 == 0)
    def p4(x: Int): Boolean = (x >= 2)
    def p5(x: Int): Boolean = (x >= 1)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains intersection elements") {
    new TestSets {
      assert(!contains(i1, 1), "Intersect 1.1")
      assert( contains(i1, 2), "Intersect 1.2")
      assert(!contains(i1, 3), "Intersect 1.3")

      assert(!contains(i2, 1), "Intersect 2.1")
      assert( contains(i2, 2), "Intersect 2.2")
      assert( contains(i2, 3), "Intersect 2.3")
    }
  }

  test("diff contains difference elements") {
    new TestSets {
      assert(!contains(d1, 1), "Difference 1.1")
      assert(!contains(d1, 2), "Difference 1.2")
      assert( contains(d1, 3), "Difference 1.3")

      assert( contains(d2, 1), "Difference 2.1")
      assert(!contains(d2, 2), "Difference 2.2")
      assert(!contains(d2, 3), "Difference 2.3")

      assert( contains(d3, 1), "Difference 3.1")
      assert(!contains(d3, 2), "Difference 3.2")
      assert(!contains(d3, 3), "Difference 3.3")
    }
  }

  test("filter contains all elements for which p holds") {
    new TestSets {
      val f1 = filter(u3, p1)
      val f2 = filter(u3, p2)
      val f3 = filter(u3, p3)
      val f4 = filter(u3, p4)
      val f5 = filter(u3, p5)

      assert( contains(f1, 1), "Filter 1.1")
      assert( contains(f1, 2), "Filter 1.2")
      assert( contains(f1, 3), "Filter 1.3")

      assert(!contains(f2, 1), "Filter 2.1")
      assert(!contains(f2, 2), "Filter 2.2")
      assert(!contains(f2, 3), "Filter 2.3")

      assert(!contains(f3, 1), "Filter 3.1")
      assert( contains(f3, 2), "Filter 3.2")
      assert(!contains(f3, 3), "Filter 3.3")

      assert(!contains(f4, 1), "Filter 4.1")
      assert( contains(f4, 2), "Filter 4.2")
      assert( contains(f4, 3), "Filter 4.3")

      assert( contains(f5, 1), "Filter 5.1")
      assert( contains(f5, 2), "Filter 5.2")
      assert( contains(f5, 3), "Filter 5.3")
    }
  }
  
  test("forall p") {
    new TestSets {

      assert( forall(u3, p1), "Forall 1")
      assert(!forall(u3, p2), "Forall 2")
      assert(!forall(u3, p3), "Forall 3")
      assert(!forall(u3, p4), "Forall 4")
      assert( forall(u3, p5), "Forall 5")
    }
  }
  
  test("exists p") {
    new TestSets {

      assert( exists(u3, p1), "Exists 1")
      assert(!exists(u3, p2), "Exists 2")
      assert( exists(u3, p3), "Exists 3")
      assert( exists(u3, p4), "Exists 4")
      assert( exists(u3, p5), "Exists 5")
    }
  }
  
  test("map f") {
    new TestSets {
      def f1(x: Int): Int = x
      def f2(x: Int): Int = x * 2
      
      val m1 = map(u3, f1)
      val m2 = map(u3, f2)

      assert( contains(m1, 1), "Map 1.1")
      assert( contains(m1, 2), "Map 1.2")
      assert( contains(m1, 3), "Map 1.3")
      assert(!contains(m1, 4), "Map 1.4")
      assert(!contains(m1, 5), "Map 1.5")
      assert(!contains(m1, 6), "Map 1.6")

      assert(!contains(m2, 1), "Map 2.1")
      assert( contains(m2, 2), "Map 2.2")
      assert(!contains(m2, 3), "Map 2.3")
      assert( contains(m2, 4), "Map 2.4")
      assert(!contains(m2, 5), "Map 2.5")
      assert( contains(m2, 6), "Map 2.6")
    }
  }
}
