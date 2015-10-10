package scala.compat.java8.collectionImpl

import java.util.Spliterator
import Stepper._

/** Abstracts all the generic operations of stepping over a collection with a fast tail operation.
  * The basic idea is that we drop breadcrumbs along when we need to split so we can jump around
  * when needed if we need to split again.  If we don't split, then we don't do any breadcrumb stuff.
  */
abstract class AbstractStepsWithTail[CC >: Null, Sub >: Null, Semi <: Sub](protected var underlying: CC, protected var maxN: Int) {
  // If breadCrumbs are nonempty, then we maintain the invariant
  //    maxN = breadCrumbs.length*breadInterval + breadLast
  protected final var breadCrumbs: List[CC] = Nil
  protected final var breadInterval: Int = 0
  protected final var breadLast: Int = 0
  protected def myTail(cc: CC): CC
  protected def myTailAfter(cc: CC, n: Int): CC   // Must return null if it doesn't take all n steps (empty end of collection is okay, one past is null)
  protected def myEmpty(cc: CC): Boolean
  def semiclone(half: Int): Semi
  def characteristics(): Int = Ordered
  def estimateSize(): Long = if (maxN == 0 || !hasNext) 0L else if (maxN < Int.MaxValue && maxN > 0) maxN else -1L
  def hasNext(): Boolean = if (myEmpty(underlying)) { underlying = null; maxN = 0; tailPoints = Nil; false } else true

  /** Returns the number of elements actually advanced, and sets `breadCrumbs`,
    * `breadInterval`, and `breadLast` to reflect the elements visited.  The
    * head of `breadCrumbs` will be exactly `n` after the current position of
    * `underlying` unless it was exhausted.  Thus, when using this to seek
    * ahead, if the return value is the requested value, pop one off of
    * `breadCrumbs` and go from there (using the remaining `breadCrumbs` for
    * the part you've passed).  If the return value is not what was requested,
    * you can search for a midpoint with `advanceUsingCrumbs` (for example).  If
    * the end was found, `maxN` will be set appropriately.
    */
  private def dropCrumbsUntil(n: Int, interval: Int = 0): Int = {
    breadInterval =
      if (interval > 0)   interval
      else if (n < 512)   32
      else if (n < 4096)  128
      else if (n < 65536) 1024
      else                8192
    breadCrumbs = Nil
    breadLast = 0
    var u = underlying
    var i = 0
    while (i + breadInterval < n && !myEmpty(u)) {
      myTailAfter(u, breadInterval) match {
        case null =>
          maxN = i
          while (!myEmpty(u)) { i += 1; breadLast += 1; u = myTail(u) }
        case ui =>
          if (!myEmpty(ui)) breadCrumbs = ui :: breadCrumbs
          u = ui
          i += breadInterval
      }
    }
    if (!myEmpty(u)) {
      while (!myEmpty(u) && i < n) { i += 1; breadLast += 1; u = myTail(u) }
      if (i == n && breadLast > 0) breadCrumbs = u :: breadCrumbs
      if (myEmpty(u)) maxN = i
    }
    i
  }

  /** Returns the number actually advanced, the collection at that point,
    * a list of breadcrumbs to use after the new point, and a list of crumbs
    * to use to get to that point (not including that point).
    */
  private def advanceUsingCrumbs(n: Int): (Int, CC, List[CC], List[CC]) = {
    if (breadCrumbs.isEmpty) {
      var u = underlying
      var i = 0
      while (i < n && !myEmpty(u)) {
        u = myTail(u)
        i -= 1
      }
      (n - i, u, Nil, Nil)
    }
    else {
      var later = List.newBuilder[CC]
      var u = breadCrumbs.head
      var crumbs = breadCrumbs.tail
      var m = if (breadLast > 0) breadLast else breadInterval
      while (crumbs.nonEmpty && math.abs(maxN - (m + breadInterval + n)) < math.abs(maxN - (m + n))) {
        later += u
        u = crumbs.head
        crumbs = crumbs.tail
        m += breadInterval
      }
      (maxN - m, u, later.result(), crumbs)
    }
  }

  def substep(): Sub = {
    if (maxN == Int.MaxValue || maxN < 0) {
      val n = if (maxN > -256) 128 else (-maxN) >>> 1
      val k = dropCrumbsUntil(n)
      if (k < n) subste()  // Will now hit other branch since maxN known
      else {
        val sub = semiclone(n)
        sub.breadCrumbs = breadCrumbs.tail
        sub.breadInterval = breadInterval
        sub.breadLast = breadLast
        underlying = breadCrumbs.head
        breadCrumbs = Nil
        maxN = if (n >= 0x8000000) -n else 4*maxN  // Exponential growth of how far to seek (really fast exponential so we have room to subdivide)
        sub
      }
    }
    else if (maxN > 1) {
      val n = maxN >>> 1
      // Need to write this part
      ???
    }
    else null
  }
}

/** Abstracts the operation of stepping over a generic indexable collection */
abstract class StepsWithTail[A, CC, STA >: Null <: StepsWithTail[A, CC, _]](_underlying: CC)
  extends AbstractStepsWithTail[CC, AnyStepper[A], STA](_underlying, Int.MaxValue, Nil)
  with AnyStepper[A]
{}

/** Abstracts the operation of stepping over an indexable collection of Doubles */
abstract class StepsDoubleWithTail[CC, STD >: Null <: StepsDoubleWithTail[CC, _]](_underlying: CC)
  extends AbstractStepsWithTail[CC, DoubleStepper, STD](_underlying, Int.MaxValue, Nil)
  with DoubleStepper
{}

/** Abstracts the operation of stepping over an indexable collection of Ints */
abstract class StepsIntWithTail[CC, STI >: Null <: StepsIntWithTail[CC, _]](_underlying: CC)
  extends AbstractStepsWithTail[CC, IntStepper, STI](_underlying, Int.MaxValue, Nil)
  with IntStepper
{}

/** Abstracts the operation of stepping over an indexable collection of Longs */
abstract class StepsLongWithTail[CC, STL >: Null <: StepsLongWithTail[CC, _]](_underlying: CC)
  extends AbstractStepsWithTail[CC, LongStepper, STL](_underlying, Int.MaxValue, Nil)
  with LongStepper
{}
