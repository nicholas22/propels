package propels.core.utils

import java.util.Comparator

object ComparatorUtils {

  /**
   * Wraps the given comparison function a new comparator
   *
   * @throws NullPointerException An argument is null
   */
  def toComparator[T](comparison: ((T, T) => Int)): Comparator[T] = {
    if (comparison == null) throw new NullPointerException("comparison")
    new ComparisonComparator(comparison)
  }

  /**
   * Wraps the given comparator returning a comparison function
   *
   * @throws NullPointerException An argument is null
   */
  def toComparison[T](comparator: Comparator[T]): ((T, T) => Int) = {
    if (comparator == null) throw new NullPointerException("comparator")
    return (t1: T, t2: T) => { comparator.compare(t1, t2) }
  }

  private class ComparisonComparator[T](val comparison: ((T, T) => Int)) extends Comparator[T] {

    override def compare(o1: T, o2: T): Int = comparison.apply(o1, o2)
  }
}