package funsets

/**
 * 2. Purely Functional Sets.
 */
trait FunSets extends FunSetsInterface:
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  override type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   * Make usage of an anonymous function here.
   */
  def singletonSet(elem: Int): FunSet = (x: Int) => x == elem

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: FunSet, t: FunSet): FunSet = (x: Int) => s(x) || t(x)

  /**
   * Creates a set from a variable-length input sequence of ints
   */
  def setFrom(elements: Int*): FunSet = {
    require(elements.length > 1, "setFrom requires elements to have at least 2 elements")
    var s = singletonSet(elements(0))
    for (e <- elements.tail) {
      s = union(s, singletonSet(e))
    }
    s
  }

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet = (x: Int) => s(x) && t(x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   *
   * We check whether the element `x` is part of s but not of x
   */
  def diff(s: FunSet, t: FunSet): FunSet = (x: Int) => s(x) && !t(x)

  /**
   * Returns the subset of `s` for which `p` holds.
   *
   * We first check if x is a member of p , then check that p holds
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet = (x: Int) => s(x) && p(x)


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean =
    def iter(a: Int): Boolean =
      if a > bound then
        // We've seen all elements and the
        // predicate has held
        true
      else if s(a) && !p(a) then
        // The element is in the set
        // but the predicate does not hold
        false
      else
        iter(a + 1)
    iter(-bound)

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean =
    // We check for the predicate !(pa)
    // If forall(s, !p(a)) returns true, then the predicate is
    // not true for any element. Thus, !forall(s, !p(a)) will
    // return whether the predicate is true for at least 1 element
    !forall(s, (x: Int) => !p(x))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: FunSet, f: Int => Int): FunSet =
    // Use exist query with predicate such that f(q) == x, where `x` is an element
    // which exists in `s`, and `q` is an arbitrary element.
    (x: Int) => exists(s, (q: Int) => f(q) == x)

  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String =
    val xs = for i <- (-bound to bound) if contains(s, i) yield i
    xs.mkString("{", ",", "}")

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet): Unit =
    println(toString(s))

object FunSets extends FunSets
