// /////////////////////////////////////////////////////////
// This file is part of Propel.
//
// Propel is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Propel is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with Propel. If not, see <http://www.gnu.org/licenses/>.
// /////////////////////////////////////////////////////////
// Authored by: Nikolaos Tountas -> salam.kaser-at-gmail.com
// /////////////////////////////////////////////////////////
package propels.core.utils

import java.util.Collection
import java.util.ArrayList
import java.util.Set
import java.util.TreeSet
import java.util.Collections
import java.util.Comparator
import java.util.TreeMap
import propel.core.collections.arrays.ReadOnlyArrayIterator
import propel.core.collections.lists.ReifiedList
import propel.core.collections.lists.ReifiedArrayList
import propel.core.collections.ReifiedIterable
import propel.core.collections.maps.ReifiedMap
import propel.core.counters.ModuloCounter
import propel.core.utils.InvalidCastBehaviour
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions
import scala.collection.JavaConverters
import scala.util.control.Breaks.breakable
import scala.util.control.Breaks.break
import scala.collection.mutable.Buffer
import scala.util.Sorting
import scala.reflect.ClassTag

object Linq {

  /**
   * The default list size to use for collecting results when the result size is unknown
   */
  val DEFAULT_LIST_SIZE = propel.core.utils.Linq.DEFAULT_LIST_SIZE

  /**
   * Applies an accumulator function over a Traversable. The specified seed value is used as the initial accumulator value.
   *
   * @throws NullPointerException An argument is null.
   */
  def aggregate[TSource, TAccumulate](values: Traversable[TSource], seed: TAccumulate, func: (TAccumulate, _ >: TSource) => TAccumulate) {
    if (values == null) throw new NullPointerException("values")
    if (func == null) throw new NullPointerException("func")

    var result = seed

    for (item <- values)
      result = func(result, item)

    result
  }

  /**
   * Applies an accumulator function over an array. The specified seed value is used as the initial accumulator value.
   *
   * @throws NullPointerException An argument is null.
   */
  def aggregate[TSource, TAccumulate](values: Array[TSource], seed: TAccumulate, func: (TAccumulate, _ >: TSource) => TAccumulate) {
    if (values == null) throw new NullPointerException("values")
    if (func == null) throw new NullPointerException("func")

    var result = seed

    for (item <- values)
      result = func(result, item)

    result
  }

  /**
   * Applies an accumulator function over a JDK iterable. The specified seed value is used as the initial accumulator value.
   *
   * @throws NullPointerException An argument is null.
   */
  def aggregate[TSource, TAccumulate](values: java.lang.Iterable[TSource],
    seed: TAccumulate, func: (TAccumulate, _ >: TSource) => TAccumulate) {
    if (values == null) throw new NullPointerException("values")
    if (func == null) throw new NullPointerException("func")

    val it = JavaConversions.iterableAsScalaIterable(values)

    aggregate(it, seed, func)
  }

  /**
   * Applies an accumulator function over a Traversable. The specified seed value is used as the initial accumulator value, and the specified
   * function is used to select the result value from the accumulator's type.
   *
   * @throws NullPointerException An argument is null.
   */
  def aggregate[TSource, TAccumulate, TResult](values: Traversable[TSource], seed: TAccumulate,
    func: (TAccumulate, TSource) => TAccumulate, resultSelector: (TAccumulate => TResult)): TResult =
    {
      if (values == null) throw new NullPointerException("values")
      if (func == null) throw new NullPointerException("func")
      if (resultSelector == null) throw new NullPointerException("resultSelector")

      var result = seed

      for (item <- values)
        result = func(result, item)

      resultSelector(result)
    }

  /**
   * Applies an accumulator function over an array. The specified seed value is used as the initial accumulator value, and the specified
   * function is used to select the result value from the accumulator's type.
   *
   * @throws NullPointerException An argument is null.
   */
  def aggregate[TSource, TAccumulate, TResult](values: Array[TSource], seed: TAccumulate,
    func: (TAccumulate, TSource) => TAccumulate, resultSelector: (TAccumulate => TResult)): TResult =
    {
      if (values == null) throw new NullPointerException("values")
      if (func == null) throw new NullPointerException("func")
      if (resultSelector == null) throw new NullPointerException("resultSelector")

      var result = seed
      for (item <- values)
        result = func(result, item)

      resultSelector(result)
    }

  /**
   * Applies an accumulator function over a JDK iterable. The specified seed value is used as the initial accumulator value, and the specified
   * function is used to select the result value from the accumulator's type.
   *
   * @throws NullPointerException An argument is null.
   */
  def aggregate[TSource, TAccumulate, TResult](values: java.lang.Iterable[TSource], seed: TAccumulate,
    func: (TAccumulate, TSource) => TAccumulate, resultSelector: (TAccumulate => TResult)): TResult =
    {
      if (values == null) throw new NullPointerException("values")

      val it = JavaConversions.iterableAsScalaIterable(values)
      aggregate(it, seed, func, resultSelector)
    }

  /**
   * Returns true if a condition is true for all items in a Traversable. Otherwise returns false.
   *
   * @throws NullPointerException When an argument is null.
   */
  def all[T](values: Traversable[T], predicate: ((_ >: T) => Boolean)): Boolean =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      values.forall(predicate)
    }

  /**
   * Returns true if a condition is true for all items in an array. Otherwise returns false.
   *
   * @throws NullPointerException When an argument is null.
   */
  def all[T](values: Array[T], predicate: (T => Boolean)): Boolean =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      values.forall(predicate)
    }

  /**
   * Returns true if a condition is true for all items in a JDK Iterable. Otherwise returns false.
   *
   * @throws NullPointerException When an argument is null.
   */
  def all[T](values: java.lang.Iterable[T], predicate: (T => Boolean)): Boolean =
    {
      if (values == null) throw new NullPointerException("values")

      val it = JavaConversions.iterableAsScalaIterable(values)
      all(it, predicate)
    }

  /**
   * Returns true if a condition is true for any of the items in a Traversable. Otherwise returns false.
   *
   * @throws NullPointerException An argument is null.
   */
  def any[T](values: Traversable[T], predicate: (T => Boolean)): Boolean =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      values.exists(predicate)
    }

  /**
   * Returns true if a condition is true for any of the items in an array. Otherwise returns false.
   *
   * @throws NullPointerException An argument is null.
   */
  def any[T](values: Array[T], predicate: (T => Boolean)): Boolean =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      values.exists(predicate)
    }

  /**
   * Returns true if a condition is true for any of the items in a JDK iterable. Otherwise returns false.
   *
   * @throws NullPointerException An argument is null.
   */
  def any[T](values: java.lang.Iterable[T], predicate: (T => Boolean)): Boolean =
    {
      if (values == null) throw new NullPointerException("values")

      val it = JavaConversions.iterableAsScalaIterable(values)
      any(it, predicate)
    }

  /**
   * Casts a Traversable of values of a certain type to a Traversable of values of another type. Uses InvalidCastBehaviour.Remove i.e. excluding
   * any elements that do not successfully cast, without throwing exceptions. This operates differently to OfType, in that it forces a cast
   * rather than checking if a TSource is of TDest type.
   *
   * @throws NullPointerException When an argument is null.
   */
  def cast[TSource, TDest](values: Traversable[TSource], destinationClass: Class[TDest]): Traversable[TDest] =
    {
      cast(values, destinationClass, InvalidCastBehaviour.Remove)
    }

  /**
   * Casts an array of values of a certain type to an array of values of another type. Uses InvalidCastBehaviour.Remove i.e. excluding any
   * elements that do not successfully cast, without throwing exceptions. This operates differently to OfType, in that it forces a cast
   * rather than checking if a TSource is of TDest type.
   *
   * @throws NullPointerException When an argument is null.
   */
  def cast[TSource, TDest](values: Array[TSource], destinationClass: Class[TDest]): Traversable[TDest] =
    {
      cast(values, destinationClass, InvalidCastBehaviour.Remove)
    }

  /**
   * Casts a JDK Iterable of values of a certain type to a JDK Iterable of values of another type. Uses InvalidCastBehaviour.Remove i.e. excluding any
   * elements that do not successfully cast, without throwing exceptions. This operates differently to OfType, in that it forces a cast
   * rather than checking if a TSource is of TDest type.
   *
   * @throws NullPointerException When an argument is null.
   */
  def cast[TSource, TDest](values: java.lang.Iterable[TSource], destinationClass: Class[TDest]): java.lang.Iterable[TDest] =
    {
      propel.core.utils.Linq.cast(values, destinationClass, InvalidCastBehaviour.Remove)
    }

  /**
   * Casts a Traversable of values of a certain type to a Traversable of values of another type, using the specified behaviour upon the event of a
   * cast failure. This operates differently to OfType, in that it forces a cast rather than checking if a TSource is of TDest type.
   *
   * @throws NullPointerException When an argument is null.
   * @throws IllegalArgumentException Unrecognized cast behaviour.
   */
  def cast[TSource, TDest](values: Traversable[TSource], destinationClass: Class[TDest], castBehaviour: InvalidCastBehaviour): Traversable[TDest] =
    {
      if (values == null) throw new NullPointerException("values")
      if (destinationClass == null) throw new NullPointerException("destinationClass")

      castBehaviour match {
        case InvalidCastBehaviour.Throw => castThrow(values, destinationClass)
        case InvalidCastBehaviour.Remove => castRemove(values, destinationClass)
        case InvalidCastBehaviour.UseDefault => castUseDefault(values, destinationClass)
        case _ => throw new IllegalArgumentException("Unrecognised cast behaviour: " + castBehaviour)
      }
    }

  /**
   * Casts an array of values of a certain type to an array of values of another type, using the specified behaviour upon the event of a
   * cast failure. This operates differently to OfType, in that it forces a cast rather than checking if a TSource is of TDest type.
   *
   * @throws NullPointerException When an argument is null.
   * @throws IllegalArgumentException Unrecognized cast behaviour.
   */
  def cast[TSource, TDest](values: Array[TSource], destinationClass: Class[TDest], castBehaviour: InvalidCastBehaviour): Array[TDest] =
    {
      if (values == null) throw new NullPointerException("values")
      if (destinationClass == null) throw new NullPointerException("destinationClass")

      castBehaviour match {
        case InvalidCastBehaviour.Throw => castThrow(values, destinationClass)
        case InvalidCastBehaviour.Remove => castRemove(values, destinationClass)
        case InvalidCastBehaviour.UseDefault => castUseDefault(values, destinationClass)
        case _ => throw new IllegalArgumentException("Unrecognised cast behaviour: " + castBehaviour)
      }
    }

  /**
   * Casts a JDK Iterable of values of a certain type to a JDK Iterable of values of another type, using the specified behaviour upon the event of a
   * cast failure. This operates differently to OfType, in that it forces a cast rather than checking if a TSource is of TDest type.
   *
   * @throws NullPointerException When an argument is null.
   * @throws IllegalArgumentException Unrecognized cast behaviour.
   */
  def cast[TSource, TDest](values: java.lang.Iterable[TSource], destinationClass: Class[TDest], castBehaviour: InvalidCastBehaviour): java.lang.Iterable[TDest] =
    {
      propel.core.utils.Linq.cast(values, destinationClass, castBehaviour)
    }

  /**
   * Concatenates two or more sequences
   *
   * @throws NullPointerException When the values or one of its elements is null.
   */
  def concat[T](values: Traversable[_ <: T]*): Traversable[T] =
    {
      if (values == null) throw new NullPointerException("values")
      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)

      for (item <- values) {
        if (item == null) throw new NullPointerException("Item of values")

        for (v: T <- item)
          result += v
      }

      result
    }

  /**
   * Concatenates two or more arrays
   *
   * @throws NullPointerException When the values or one of its elements is null.
   */
  def concat[T](values: Array[T]*)(implicit manifest: ClassTag[T]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")
      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)

      for (vals <- values) {
        if (vals == null) throw new NullPointerException("Item of values")

        for (v <- vals)
          result += v
      }

      result.toArray(manifest)
    }

  /**
   * Returns true if an item is contained in a Traversable. Scans the sequence linearly. You may scan for nulls.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def contains[T](values: Traversable[T], item: T): Boolean =
    {
      if (item == null)
        containsNull(values)
      else
        containsNonNull(values, item)
    }

  /**
   * Returns true if an item is contained in an array. Scans the sequence linearly. You may scan for nulls.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def contains[T](values: Array[T], item: T): Boolean =
    {
      if (item == null)
        containsNull(values)
      else
        containsNonNull(values, item)
    }

  /**
   * Returns true if an item is contained in a JDK iterable. Scans the sequence linearly. You may scan for nulls.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def contains[T](values: java.lang.Iterable[T], item: T): Boolean =
    {
      if (values == null) throw new NullPointerException("values")

      val it = JavaConversions.iterableAsScalaIterable(values)
      contains(it, item)
    }

  /**
   * Returns true if an item is contained in a Traversable. Scans the sequence linearly. You may scan for nulls.
   *
   * @throws NullPointerException When the values argument or the comparator is null.
   */
  def contains[T](values: Traversable[T], item: T, comparison: ((T, T) => Int)): Boolean =
    {
      if (item == null)
        containsNull(values)
      else
        containsNonNull(values, item, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Returns true if an item is contained in an array. Scans the sequence linearly. You may scan for nulls.
   *
   * @throws NullPointerException When the array or the comparator is null.
   */
  def contains[T](values: Array[T], item: T, comparison: ((T, T) => Int)): Boolean =
    {
      if (item == null)
        containsNull(values)
      else
        containsNonNull(values, item, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Returns true if an item is contained in a JDK iterable. Scans the sequence linearly. You may scan for nulls.
   *
   * @throws NullPointerException When the array or the comparator is null.
   */
  def contains[T](values: java.lang.Iterable[T], item: T, comparison: ((T, T) => Int)): Boolean =
    {
      propel.core.utils.Linq.contains(values, item, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Returns true if any of the items are contained in the given values. Scans the values sequence linearly, up to items.Length number of
   * times. You may scan for null.
   *
   * @throws NullPointerException When an argument is null.
   */
  def containsAny[T](values: Traversable[T], items: Traversable[T]): Boolean =
    {
      if (items == null) throw new NullPointerException("items")

      for (item <- items)
        if (contains(values, item))
          return true

      return false
    }

  /**
   * Returns true if any of the items are contained in the given values. Scans the values sequence linearly, up to items.Length number of
   * times. You may scan for null.
   *
   * @throws NullPointerException When an argument is null.
   */
  def containsAny[T](values: Array[T], items: Array[T]): Boolean =
    {
      if (items == null) throw new NullPointerException("items")

      for (item <- items)
        if (contains(values, item))
          return true

      return false
    }

  /**
   * Returns true if any of the items are contained in the given values. Scans the values sequence linearly, up to items.Length number of
   * times. You may scan for null.
   *
   * @throws NullPointerException When an argument is null.
   */
  def containsAny[T](values: java.lang.Iterable[T], items: java.lang.Iterable[T]): Boolean =
    {
      propel.core.utils.Linq.containsAny(values, items)
    }

  /**
   * Returns true if any of the items are contained in the given values. Scans the values sequence linearly, up to items.Length number of
   * times. You may scan for null.
   *
   * @throws NullPointerException When an argument is null.
   */
  def containsAny[T](values: Traversable[T], items: Traversable[T], comparison: ((T, T) => Int)): Boolean =
    {
      if (items == null) throw new NullPointerException("items")

      for (item <- items)
        if (contains(values, item, comparison))
          return true

      return false
    }

  /**
   * Returns true if any of the items are contained in the given values. Scans the values sequence linearly, up to items.Length number of
   * times. You may scan for null.
   *
   * @throws NullPointerException When an argument is null.
   */
  def containsAny[T](values: Array[T], items: Array[T], comparison: ((T, T) => Int)): Boolean =
    {
      if (items == null) throw new NullPointerException("items")

      for (item <- items)
        if (contains(values, item, comparison))
          return true

      return false
    }

  /**
   * Returns true if any of the items are contained in the given values. Scans the values sequence linearly, up to items.Length number of
   * times. You may scan for null.
   *
   * @throws NullPointerException When an argument is null.
   */
  def containsAny[T](values: java.lang.Iterable[T], items: java.lang.Iterable[T], comparison: ((T, T) => Int)): Boolean =
    {
      if (items == null) throw new NullPointerException("items")

      val it = items.iterator
      while (it.hasNext) {
        val item = it.next
        if (contains(values, item, comparison))
          return true
      }

      return false
    }

  /**
   * Returns true if all of the items are contained in the given values. Scans the values sequence linearly, up to items.Length number of
   * times. You may scan for null.
   *
   * @throws NullPointerException When an argument is null.
   */
  def containsAll[T](values: Traversable[T], items: Traversable[T]): Boolean =
    {
      if (items == null) throw new NullPointerException("items")

      for (item <- items)
        if (!contains(values, item))
          return false

      return true
    }

  /**
   * Returns true if all of the items are contained in the given values. Scans the values sequence linearly, up to items.Length number of
   * times. You may scan for null.
   *
   * @throws NullPointerException When an argument is null.
   */
  def containsAll[T](values: Array[T], items: Array[T]): Boolean =
    {
      if (items == null) throw new NullPointerException("items")

      for (item <- items)
        if (!contains(values, item))
          return false

      return true
    }

  /**
   * Returns true if all of the items are contained in the given values. Scans the values sequence linearly, up to items.Length number of
   * times. You may scan for null.
   *
   * @throws NullPointerException When an argument is null.
   */
  def containsAll[T](values: java.lang.Iterable[T], items: java.lang.Iterable[T]): Boolean =
    {
      if (items == null) throw new NullPointerException("items")

      val it = items.iterator
      while (it.hasNext) {
        val item = it.next
        if (!contains(values, item))
          return false
      }

      return true
    }

  /**
   * Returns the Traversable's size.
   *
   * @throws NullPointerException When an argument is null.
   */
  def count[T](values: Traversable[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")
      values.size
    }

  /**
   * Returns the array's length.
   *
   * @throws NullPointerException When an argument is null.
   */
  def count[T](values: Array[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")
      values.length
    }

  /**
   * Counts an JDK Iterable in the most efficient manner possible.
   *
   * @throws NullPointerException When an argument is null.
   */
  def count[T](values: java.lang.Iterable[T]): Int =
    {
      propel.core.utils.Linq.count(values)
    }

  /**
   * Returns the number of occurrences of an object in a Traversable. It is possible to search for null objects.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def count[T](values: Traversable[T], item: T): Int =
    {
      if (item == null)
        countNulls(values)
      else
        countNonNull(values, item)
    }

  /**
   * Returns the number of occurrences of an object in an array. It is possible to search for null objects.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def count[T](values: Array[T], item: T): Int =
    {
      if (item == null)
        countNulls(values)
      else
        countNonNull(values, item)
    }

  /**
   * Returns the number of occurrences of an object in a JDK Iterable. It is possible to search for null objects.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def count[T](values: java.lang.Iterable[T], item: T): Int =
    {
      propel.core.utils.Linq.count(values, item)
    }

  /**
   * Returns the number of occurrences of an object in a Traversable. It is possible to search for null objects.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def count[T](values: Traversable[T], item: T, comparison: ((T, T) => Int)): Int =
    {
      if (item == null)
        countNulls(values)
      else
        countNonNull(values, item, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Returns the number of occurrences of an object in an array. It is possible to search for null objects.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def count[T](values: Array[T], item: T, comparison: ((T, T) => Int)): Int =
    {
      if (item == null)
        countNulls(values)
      else
        countNonNull(values, item, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Returns the number of occurrences of an object in an array. It is possible to search for null objects.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def count[T](values: java.lang.Iterable[T], item: T, comparison: ((T, T) => Int)): Int =
    {
      propel.core.utils.Linq.count(values, item, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Returns the number of occurrences in the Traversable that satisfy the given condition.
   *
   * @throws NullPointerException When an argument is null.
   */
  def countWhere[T](values: Traversable[T], predicate: (T => Boolean)): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      var counter = 0
      for (item <- values) {
        if (predicate(item))
          counter = counter + 1
      }

      counter
    }

  /**
   * Returns the number of occurrences in the Array that satisfy the given condition.
   *
   * @throws NullPointerException When an argument is null.
   */
  def countWhere[T](values: Array[T], predicate: (T => Boolean)): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      var counter = 0
      for (item <- values) {
        if (predicate(item))
          counter = counter + 1
      }

      counter
    }

  /**
   * Returns the number of occurrences in the Array that satisfy the given condition.
   *
   * @throws NullPointerException When an argument is null.
   */
  def countWhere[T](values: java.lang.Iterable[T], predicate: (T => Boolean)): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      val it = values.iterator
      var counter = 0

      while (it.hasNext) {
        val item = it.next
        if (predicate(item))
          counter = counter + 1
      }

      counter
    }

  /**
   * Returns the Traversable if at least one element exists in it, otherwise returns a collection consisting of a single element which has a
   * default value.
   *
   * @throws NullPointerException When an argument is null.
   */
  def defaultIfEmpty[T](values: Traversable[T]): Traversable[T] =
    {
      if (values == null) throw new NullPointerException("values")

      for (item <- values)
        values

      new ArrayBuffer[T](1)
    }

  /**
   * Returns the array if at least one element exists in it, otherwise returns a collection consisting of a single element which has a
   * null value.
   *
   * @throws NullPointerException When an argument is null.
   */
  def defaultIfEmpty[T](values: Array[T])(implicit manifest: ClassTag[T]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")

      if (values.size > 0)
        values

      new Array[T](1)
    }

  /**
   * Returns the array if at least one element exists in it, otherwise returns a collection consisting of a single element which has a
   * null value.
   *
   * @throws NullPointerException When an argument is null.
   */
  def defaultIfEmpty[T](values: java.lang.Iterable[T]): java.lang.Iterable[T] =
    {
      propel.core.utils.Linq.defaultIfEmpty(values)
    }

  /**
   * Returns the default value for a type
   */
  def defaultValue[U: ClassTag]: U =
    {
      new Array[U](1)(0)
    }

  /**
   * Returns distinct (i.e. no duplicate) elements from a Traversable.
   *
   * @throws NullPointerException When an argument is null.
   */
  def distinct[T](values: Traversable[T]): Traversable[T] =
    {
      distinct(values, null)
    }

  /**
   * Returns distinct (i.e. no duplicate) elements from an array.
   *
   * @throws NullPointerException When an argument is null.
   */
  def distinct[T](values: Array[T])(implicit manifest: ClassTag[T]): Array[T] =
    {
      distinct(values, null)
    }

  /**
   * Returns distinct (i.e. no duplicate) elements from a JDK Iterable.
   *
   * @throws NullPointerException When an argument is null.
   */
  def distinct[T](values: java.lang.Iterable[T]): java.lang.Iterable[T] =
    {
      distinct(values, null)
    }

  /**
   * Returns distinct (i.e. no duplicate) elements from a Traversable. Uses the specified comparator to identify duplicates.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def distinct[T](values: Traversable[T], comparison: ((T, T) => Int)): Traversable[T] =
    {
      if (values == null) throw new NullPointerException("values")

      var set: java.util.Set[T] = null
      if (comparison != null)
        set = new TreeSet[T](ComparatorUtils.toComparator(comparison))
      else
        set = new TreeSet[T]()

      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      for (item <- values)
        if (!set.contains(item)) {
          set.add(item)
          result += item
        }

      result
    }

  /**
   * Returns distinct (i.e. no duplicate) elements from an array. Uses the specified comparator to identify duplicates.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def distinct[T](values: Array[T], comparison: ((T, T) => Int))(implicit manifest: ClassTag[T]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")

      var set: java.util.Set[T] = null
      if (comparison != null)
        set = new TreeSet[T](ComparatorUtils.toComparator(comparison))
      else
        set = new TreeSet[T]()

      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      for (item <- values)
        if (!set.contains(item)) {
          set.add(item)
          result += item
        }

      result.toArray(manifest)
    }

  /**
   * Returns distinct (i.e. no duplicate) elements from a JDK Iterable. Uses the specified comparator to identify duplicates.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def distinct[T](values: java.lang.Iterable[T], comparison: ((T, T) => Int)): java.lang.Iterable[T] =
    {
      if (comparison == null)
        propel.core.utils.Linq.distinct(values)
      else
        propel.core.utils.Linq.distinct(values, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Returns the element at the given position in the provided Traversable.
   *
   * @throws NullPointerException When the values argument is null.
   * @throws IndexOutOfBoundsException When the index is out of range.
   */
  def elementAt[T](values: Traversable[T], index: Int): T =
    {
      if (values == null) throw new NullPointerException("values")
      if (index < 0) throw new IndexOutOfBoundsException("index=" + index)

      var i: Int = 0
      for (item <- values) {
        if (i == index)
          return item

        i += 1
      }

      throw new IndexOutOfBoundsException("max=" + i + " index=" + index)
    }

  /**
   * Returns the element at the given position in the provided array.
   *
   * @throws NullPointerException An argument is null.
   * @throws IndexOutOfBoundsException When the index is out of range.
   */
  def elementAt[T](values: Array[T], index: Int): T =
    {
      values(index)
    }

  /**
   * Returns the element at the given position in the provided JDK Iterable.
   *
   * @throws NullPointerException An argument is null.
   * @throws IndexOutOfBoundsException When the index is out of range.
   */
  def elementAt[T](values: java.lang.Iterable[T], index: Int): T =
    {
      propel.core.utils.Linq.elementAt(values, index)
    }

  /**
   * Returns the element at the given position in the provided Traversable. It will return null if there is no such index.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def elementAtOrDefault[T](values: Traversable[T], index: Int): T =
    {
      if (values == null) throw new NullPointerException("values")

      if (index >= 0) {
        var i = 0
        val it = values.toIterator

        while (it.hasNext) {
          val item = it.next

          if (i == index)
            return item

          i += 1
        }
      }

      null.asInstanceOf[T]
    }

  /**
   * Returns the element at the given position in the provided array. It will return null if there is no such index.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def elementAtOrDefault[T](values: Array[T], index: Int): T =
    {
      if (values == null) throw new NullPointerException("values")

      if (index >= 0 && index < values.size)
        values(index)

      null.asInstanceOf[T]
    }

  /**
   * Returns the element at the given position in the provided JDK Iterable. It will return null if there is no such index.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def elementAtOrDefault[T](values: java.lang.Iterable[T], index: Int): T =
    {
      propel.core.utils.Linq.elementAtOrDefault(values, index)
    }

  /**
   * Returns all distinct values except the specified removed values.
   *
   * @throws NullPointerException When an argument is null.
   */
  def except[T](values: Traversable[T], removedValues: Traversable[T]): Traversable[T] =
    {
      except(values, removedValues, null)
    }

  /**
   * Returns all distinct values except the specified removed values.
   *
   * @throws NullPointerException When an argument is null.
   */
  def except[T](values: Array[T], removedValues: Array[T])(implicit manifest: ClassTag[T]): Array[T] =
    {
      except(values, removedValues, null)
    }

  /**
   * Returns all distinct values except the specified removed values.
   *
   * @throws NullPointerException When an argument is null.
   */
  def except[T](values: java.lang.Iterable[T], removedValues: java.lang.Iterable[T]): java.lang.Iterable[T] =
    {
      except(values, removedValues, null)
    }

  /**
   * Returns all distinct values except the specified removed values.
   *
   * @throws NullPointerException When the values or removedValues argument is null.
   */
  def except[T](values: Traversable[T], removedValues: Traversable[T], comparison: ((T, T) => Int)): Traversable[T] =
    {
      if (values == null) throw new NullPointerException("values")
      if (removedValues == null) throw new NullPointerException("removedValues")

      var distinctValues: Traversable[T] = null
      var distinctRemovedValues: Traversable[T] = null

      distinctValues = distinct(values, comparison)
      distinctRemovedValues = distinct(removedValues, comparison)

      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      for (item <- distinctValues)
        if (!contains(distinctRemovedValues, item))
          result += item

      result
    }

  /**
   * Returns all distinct values except the specified removed values.
   *
   * @throws NullPointerException When the values or removedValues argument is null.
   */
  def except[T](values: Array[T], removedValues: Array[T], comparison: ((T, T) => Int))(implicit manifest: ClassTag[T]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")
      if (removedValues == null) throw new NullPointerException("removedValues")

      var distinctValues: Array[T] = null
      var distinctRemovedValues: Array[T] = null

      distinctValues = distinct(values, comparison)
      distinctRemovedValues = distinct(removedValues, comparison)

      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      for (item <- distinctValues)
        if (!contains(distinctRemovedValues, item))
          result += item

      result.toArray
    }

  /**
   * Returns all distinct values except the specified removed values.
   *
   * @throws NullPointerException When the values or removedValues argument is null.
   */
  def except[T](values: java.lang.Iterable[T], removedValues: java.lang.Iterable[T], comparison: ((T, T) => Int)): java.lang.Iterable[T] =
    {
      if (comparison == null)
        propel.core.utils.Linq.except(values, removedValues)
      else
        propel.core.utils.Linq.except(values, removedValues, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Returns the first element in the provided Traversable.
   *
   * @throws NullPointerException When an argument is null.
   * @throws NoSuchElementException There is no first element.
   */
  def first[T](values: Traversable[T]): T =
    {
      if (values == null) throw new NullPointerException("values")

      for (item <- values)
        return item

      throw new NoSuchElementException("The traversable is empty")
    }

  /**
   * Returns the first element in the provided array.
   *
   * @throws NullPointerException When an argument is null.
   * @throws NoSuchElementException There is no first element.
   */
  def first[T](values: Array[T]): T =
    {
      if (values == null) throw new NullPointerException("values")
      if (values.length <= 0) throw new NoSuchElementException("The array is empty")

      values(0)
    }

  /**
   * Returns the first element in the provided JDK Iterable.
   *
   * @throws NullPointerException When an argument is null.
   * @throws NoSuchElementException There is no first element.
   */
  def first[T](values: java.lang.Iterable[T]): T =
    {
      propel.core.utils.Linq.first(values)
    }

  /**
   * Returns the first element in the provided Traversable that matches a condition.
   *
   * @throws NullPointerException When an argument is null.
   * @throws NoSuchElementException There is no match to the given predicate.
   */
  def first[T](values: Traversable[T], predicate: (T => Boolean)): T =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      for (element <- values)
        if (predicate(element))
          return element

      throw new NoSuchElementException("There is no match to the given predicate")
    }

  /**
   * Returns the first element in the provided array that matches a condition.
   *
   * @throws NullPointerException When an argument is null.
   * @throws NoSuchElementException There is no match to the given predicate.
   */
  def first[T](values: Array[T], predicate: (T => Boolean)): T =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      for (element <- values)
        if (predicate(element))
          return element

      throw new NoSuchElementException("There is no match to the given predicate")
    }

  /**
   * Returns the first element in the provided JDK iterable that matches a condition.
   *
   * @throws NullPointerException When an argument is null.
   * @throws NoSuchElementException There is no match to the given predicate.
   */
  def first[T](values: java.lang.Iterable[T], predicate: (T => Boolean)): T =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      val it = values.iterator

      while (it.hasNext) {
        val element = it.next

        if (predicate(element))
          return element
      }

      throw new NoSuchElementException("There is no match to the given predicate")
    }

  /**
   * Returns the first element in the provided Traversable. If no element is found, then null is returned.
   *
   * @throws NullPointerException When an argument is null.
   */
  def firstOrDefault[T](values: Traversable[T]): T =
    {
      if (values == null) throw new NullPointerException("values")

      for (item <- values)
        return item

      null.asInstanceOf[T]
    }

  /**
   * Returns the first element in the provided array. If no element is found, then null is returned.
   *
   * @throws NullPointerException When an argument is null.
   */
  def firstOrDefault[T](values: Array[T]): T =
    {
      if (values == null) throw new NullPointerException("values")

      for (item <- values)
        return item

      null.asInstanceOf[T]
    }

  /**
   * Returns the first element in the provided JDK Iterable. If no element is found, then null is returned.
   *
   * @throws NullPointerException When an argument is null.
   */
  def firstOrDefault[T](values: java.lang.Iterable[T]): T =
    {
      propel.core.utils.Linq.firstOrDefault(values)
    }

  /**
   * Returns the first element in the provided Traversable. If no element is found, then null is returned.
   *
   * @throws NullPointerException When an argument is null.
   */
  def firstOrDefault[T](values: Traversable[T], predicate: (T => Boolean)): T =
    {
      if (values == null) throw new NullPointerException("values")

      for (item <- values)
        if (predicate(item))
          return item

      null.asInstanceOf[T]
    }

  /**
   * Returns the first element in the provided array. If no element is found, then null is returned.
   *
   * @throws NullPointerException When an argument is null.
   */
  def firstOrDefault[T](values: Array[T], predicate: (T => Boolean)): T =
    {
      if (values == null) throw new NullPointerException("values")

      for (item <- values)
        if (predicate(item))
          return item

      null.asInstanceOf[T]
    }

  /**
   * Returns the first element in the provided JDK Iterable. If no element is found, then null is returned.
   *
   * @throws NullPointerException When an argument is null.
   */
  def firstOrDefault[T](values: java.lang.Iterable[T], predicate: (T => Boolean)): T =
    {
      if (values == null) throw new NullPointerException("values")

      val it = values.iterator

      while (it.hasNext) {
        val item = it.next
        if (predicate(item))
          return item
      }

      null.asInstanceOf[T]
    }

  /**
   * Executes an action against all elements, returning them
   *
   * @throws NullPointerException When an argument is null.
   */
  def forAll[T](values: Traversable[T], action: (T => Unit)): Traversable[T] = {
    for (v <- values)
      action.apply(v)

    values
  }

  /**
   * Executes an action against all elements, returning them
   *
   * @throws NullPointerException When an argument is null.
   */
  def forAll[T](values: Array[T], action: (T => Unit)): Array[T] = {
    for (v <- values)
      action.apply(v)

    values
  }

  /**
   * Executes an action against all elements, returning them
   *
   * @throws NullPointerException When an argument is null.
   */
  def forAll[T](values: java.lang.Iterable[T], action: (T => Unit)): java.lang.Iterable[T] = {
    val it = values.iterator
    while (it.hasNext)
      action.apply(it.next)

    values
  }

  /**
   * Groups elements by a specified key.
   *
   * @throws NullPointerException When an argument is null.
   */
  def groupBy[TKey, TResult](values: Traversable[TResult], keySelector: (TResult => TKey)): Traversable[TResult] =
    {
      groupBy(values, keySelector, null)
    }

  /**
   * Groups elements by a specified key.
   *
   * @throws NullPointerException When an argument is null.
   */
  def groupBy[TKey, TResult](values: Array[TResult], keySelector: (TResult => TKey))(implicit manifest: ClassTag[TResult]): Array[TResult] =
    {
      groupBy(values, keySelector, null)
    }

  /**
   * Groups elements by a specified key.
   *
   * @throws NullPointerException When an argument is null.
   */
  def groupBy[TKey, TResult](values: java.lang.Iterable[TResult], keySelector: (TResult => TKey)): java.lang.Iterable[TResult] =
    {
      groupBy(values, keySelector, null)
    }

  /**
   * Groups elements by a specified key and comparator.
   *
   * @throws NullPointerException When the values argument or the key selector is null.
   */
  def groupBy[TKey, TResult](values: Traversable[TResult], keySelector: (TResult => TKey), comparison: ((TKey, TKey) => Int)): Traversable[TResult] =
    {
      if (values == null) throw new NullPointerException("values")
      if (keySelector == null) throw new NullPointerException("keySelector")

      var lookup: java.util.Map[TKey, TResult] = null
      if (comparison == null)
        lookup = new TreeMap[TKey, TResult]()
      else
        lookup = new TreeMap[TKey, TResult](ComparatorUtils.toComparator(comparison))

      for (item <- values) {
        val key = keySelector(item)

        if (!lookup.containsKey(key))
          lookup.put(key, item)
      }

      val lookupValues = lookup.values()
      val result = new ArrayBuffer[TResult](lookupValues.size)
      val it = lookupValues.iterator

      while (it.hasNext) {
        val item = it.next
        result += item
      }

      result
    }

  /**
   * Groups elements by a specified key and comparator.
   *
   * @throws NullPointerException When the values argument or the key selector is null.
   */
  def groupBy[TKey, TResult](values: Array[TResult], keySelector: (TResult => TKey), comparison: ((TKey, TKey) => Int))(implicit manifest: ClassTag[TResult]): Array[TResult] =
    {
      if (values == null) throw new NullPointerException("values")
      if (keySelector == null) throw new NullPointerException("keySelector")

      var lookup: java.util.Map[TKey, TResult] = null
      if (comparison == null)
        lookup = new TreeMap[TKey, TResult]()
      else
        lookup = new TreeMap[TKey, TResult](ComparatorUtils.toComparator(comparison))

      for (item <- values) {
        val key = keySelector(item)

        if (!lookup.containsKey(key))
          lookup.put(key, item)
      }

      val lookupValues = lookup.values()
      val result = new ArrayBuffer[TResult](lookupValues.size)
      val it = lookupValues.iterator

      while (it.hasNext) {
        val item = it.next
        result += item
      }

      result.toArray
    }

  /**
   * Groups elements by a specified key and comparator.
   *
   * @throws NullPointerException When the values argument or the key selector is null.
   */
  def groupBy[TKey, TResult](values: java.lang.Iterable[TResult], keySelector: (TResult => TKey), comparison: ((TKey, TKey) => Int)): java.lang.Iterable[TResult] =
    {
      if (values == null) throw new NullPointerException("values")
      if (keySelector == null) throw new NullPointerException("keySelector")

      var lookup: java.util.Map[TKey, TResult] = null
      if (comparison == null)
        lookup = new TreeMap[TKey, TResult]()
      else
        lookup = new TreeMap[TKey, TResult](ComparatorUtils.toComparator(comparison))

      val vit = values.iterator
      while (vit.hasNext) {
        val item = vit.next
        val key = keySelector(item)

        if (!lookup.containsKey(key))
          lookup.put(key, item)
      }

      val lookupValues = lookup.values()
      val result = new ArrayList[TResult](lookupValues.size)
      val it = lookupValues.iterator

      while (it.hasNext) {
        val item = it.next
        result.add(item)
      }

      result
    }

  /**
   * Returns the index where the specified element is first found. You may search for nulls. If the element is not found, this returns -1.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def indexOf[T](values: Traversable[T], element: T): Int =
    {
      if (element == null)
        indexOfNull(values)
      else
        indexOfNotNull(values, element)
    }

  /**
   * Returns the index where the specified element is first found. You may search for nulls. If the element is not found, this returns -1.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def indexOf[T](values: Array[T], element: T): Int =
    {
      if (element == null)
        indexOfNull(values)
      else
        indexOfNotNull(values, element)
    }

  /**
   * Returns the index where the specified element is first found. You may search for nulls. If the element is not found, this returns -1.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def indexOf[T](values: java.lang.Iterable[T], element: T): Int =
    {
      if (element == null)
        indexOfNull(values)
      else
        indexOfNotNull(values, element)
    }

  /**
   * Returns the index where the specified element is first found. You may search for nulls. If the element is not found, this returns -1.
   *
   * @throws NullPointerException When the values or the comparator argument is null.
   */
  def indexOf[T](values: Traversable[T], element: T, comparison: ((T, T) => Int)): Int =
    {
      if (element == null)
        indexOfNull(values)
      else
        indexOfNotNull(values, element, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Returns the index where the specified element is first found. You may search for nulls. If the element is not found, this returns -1.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def indexOf[T](values: Array[T], element: T, comparison: ((T, T) => Int)): Int =
    {
      if (element == null)
        indexOfNull(values)
      else
        indexOfNotNull(values, element, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Returns the index where the specified element is first found. You may search for nulls. If the element is not found, this returns -1.
   *
   * @throws NullPointerException When the values argument is null.
   */
  def indexOf[T](values: java.lang.Iterable[T], element: T, comparison: ((T, T) => Int)): Int =
    {
      if (element == null)
        indexOfNull(values)
      else
        indexOfNotNull(values, element, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Returns the intersection of the distinct elements of two Traversables.
   *
   * @throws NullPointerException When an argument is null.
   */
  def intersect[T](first: Traversable[T], second: Traversable[T]): Traversable[T] =
    {
      intersect(first, second, null)
    }

  /**
   * Returns the intersection of the distinct elements of two arrays.
   *
   * @throws NullPointerException When an argument is null.
   */
  def intersect[T](first: Array[T], second: Array[T])(implicit manifest: ClassTag[T]): Array[T] =
    {
      intersect(first, second, null)
    }

  /**
   * Returns the intersection of the distinct elements of two JDK Iterables.
   *
   * @throws NullPointerException When an argument is null.
   */
  def intersect[T](first: java.lang.Iterable[T], second: java.lang.Iterable[T]): java.lang.Iterable[T] =
    {
      intersect(first, second, null)
    }

  /**
   * Returns the intersection of the distinct elements of two Traversables.
   *
   * @throws NullPointerException When the first or second argument is null.
   */
  def intersect[T](first: Traversable[T], second: Traversable[T], comparison: ((T, T) => Int)): Traversable[T] =
    {
      if (first == null) throw new NullPointerException("first")
      if (second == null) throw new NullPointerException("second")

      var distinctFirst: Traversable[T] = null
      var distinctSecond: Traversable[T] = null

      distinctFirst = distinct(first, comparison)
      distinctSecond = distinct(second, comparison)

      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      for (item <- distinctFirst)
        if (contains(distinctSecond, item))
          result += item

      result
    }

  /**
   * Returns the intersection of the distinct elements of two arrays.
   *
   * @throws NullPointerException When the first or second argument is null.
   */
  def intersect[T](first: Array[T], second: Array[T], comparison: ((T, T) => Int))(implicit manifest: ClassTag[T]): Array[T] =
    {
      if (first == null) throw new NullPointerException("first")
      if (second == null) throw new NullPointerException("second")

      var distinctFirst: Array[T] = null
      var distinctSecond: Array[T] = null

      distinctFirst = distinct(first, comparison)
      distinctSecond = distinct(second, comparison)

      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      for (item <- distinctFirst)
        if (contains(distinctSecond, item))
          result += item

      result.toArray
    }

  /**
   * Returns the intersection of the distinct elements of two JDK Iterables.
   *
   * @throws NullPointerException When the first or second argument is null.
   */
  def intersect[T](first: java.lang.Iterable[T], second: java.lang.Iterable[T], comparison: ((T, T) => Int)): java.lang.Iterable[T] =
    {
      if (first == null) throw new NullPointerException("first")
      if (second == null) throw new NullPointerException("second")

      var distinctFirst: java.lang.Iterable[T] = null
      var distinctSecond: java.lang.Iterable[T] = null

      distinctFirst = distinct(first, comparison)
      distinctSecond = distinct(second, comparison)

      val result = new ArrayList[T](DEFAULT_LIST_SIZE)
      val it = distinctFirst.iterator
      while (it.hasNext()) {
        val item = it.next
        if (contains(distinctSecond, item))
          result.add(item)
      }

      result
    }

  /**
   * Returns true if the Traversable is empty.
   *
   * @throws NullPointerException Is the argument is null.
   */
  def isEmpty[T](values: Traversable[T]): Boolean =
    {
      if (values == null) throw new NullPointerException("values")

      for (x <- values)
        return false

      return true
    }

  /**
   * Returns true if the array is empty.
   *
   * @throws NullPointerException Is the argument is null.
   */
  def isEmpty[T](values: Array[T]): Boolean =
    {
      if (values == null) throw new NullPointerException("values")

      values.size == 0
    }

  /**
   * Returns true if the JDK Iterable is empty.
   *
   * @throws NullPointerException Is the argument is null.
   */
  def isEmpty[T](values: java.lang.Iterable[T]): Boolean =
    {
      propel.core.utils.Linq.isEmpty(values)
    }

  /**
   * Returns the last element in the Traversable.
   *
   * @throws NullPointerException If the array is null
   * @throws NoSuchElementException If the iterable is empty
   */
  def last[T](values: Traversable[T]): T =
    {
      if (values == null) throw new NullPointerException("values")

      // check if any items present
      try {
        first(values)
      } catch {
        case e: NoSuchElementException => throw new NoSuchElementException("The Traversable is empty")
      }

      var last: T = first(values)
      // find last
      for (item <- values)
        last = item

      last
    }

  /**
   * Returns the last element in the array.
   *
   * @throws NullPointerException If the array is null
   * @throws NoSuchElementException If the iterable is empty
   */
  def last[T](values: Array[T]): T =
    {
      if (values == null) throw new NullPointerException("values")

      // check if any items present
      try {
        first(values)
      } catch {
        case e: NoSuchElementException => throw new NoSuchElementException("The array is empty")
      }

      var last: T = first(values)
      // find last
      for (item <- values)
        last = item

      last
    }

  /**
   * Returns the last element in the array.
   *
   * @throws NullPointerException If the array is null
   * @throws NoSuchElementException If the iterable is empty
   */
  def last[T](values: java.lang.Iterable[T]): T =
    {
      propel.core.utils.Linq.last(values)
    }

  /**
   * Returns the last element in the Traversable that satisfies the given predicate.
   *
   * @throws NullPointerException If the array is null
   * @throws NoSuchElementException If the iterable is empty
   */
  def last[T](values: Traversable[T], predicate: (T => Boolean)): T =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      // check if any items present
      var last: T = first(values, predicate)
      // find last
      for (item <- values)
        if (predicate(item))
          last = item

      last
    }

  /**
   * Returns the last element in the array that satisfies the given predicate.
   *
   * @throws NullPointerException If the array is null
   * @throws NoSuchElementException If the iterable is empty
   */
  def last[T](values: Array[T], predicate: (T => Boolean)): T =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      // check if any items present
      var last: T = first(values, predicate)
      // find last
      for (item <- values)
        if (predicate(item))
          last = item

      last
    }

  /**
   * Returns the last element in the JDK iterable that satisfies the given predicate.
   *
   * @throws NullPointerException If the array is null
   * @throws NoSuchElementException If the iterable is empty
   */
  def last[T](values: java.lang.Iterable[T], predicate: (T => Boolean)): T =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      // check if any items present
      var last: T = first(values, predicate)

      val it = values.iterator
      // find last
      while (it.hasNext) {
        val item = it.next
        if (predicate(item))
          last = item
      }

      last
    }

  /**
   * Returns the last element in the provided Traversable. It will return null if there is no match.
   *
   * @throws NullPointerException When an argument is null.
   */
  def lastOrDefault[T](values: Traversable[T]): T =
    {
      if (values == null) throw new NullPointerException("values")

      var result: T = defaultValue
      var found = false

      for (item <- values) {
        result = item
        found = true
      }

      if (found)
        result
      else
        null.asInstanceOf[T]
    }

  /**
   * Returns the last element in the provided array. It will return null if there is no match.
   *
   * @throws NullPointerException When an argument is null.
   */
  def lastOrDefault[T](values: Array[T]): T =
    {
      if (values == null) throw new NullPointerException("values")

      var result: T = defaultValue
      var found = false

      for (item <- values) {
        result = item
        found = true
      }

      if (found)
        result
      else
        null.asInstanceOf[T]
    }

  /**
   * Returns the last element in the provided array. It will return null if there is no match.
   *
   * @throws NullPointerException When an argument is null.
   */
  def lastOrDefault[T](values: java.lang.Iterable[T]): T =
    {
      propel.core.utils.Linq.lastOrDefault(values)
    }

  /**
   * Returns the last element in the provided Traversable. It will return null if there is no match.
   *
   * @throws NullPointerException When an argument is null.
   */
  def lastOrDefault[T](values: Traversable[T], predicate: (T => Boolean)): T =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      var result: T = defaultValue
      var found = false

      for (item <- values)
        if (predicate(item)) {
          result = item
          found = true
        }

      if (found)
        result
      else
        null.asInstanceOf[T]
    }

  /**
   * Returns the last element in the provided array. It will return null if there is no match.
   *
   * @throws NullPointerException When an argument is null.
   */
  def lastOrDefault[T](values: Array[T], predicate: (T => Boolean)): T =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      var result: T = defaultValue
      var found = false

      for (item <- values)
        if (predicate(item)) {
          result = item
          found = true
        }

      if (found)
        result
      else
        null.asInstanceOf[T]
    }

  /**
   * Returns the last element in the provided array. It will return null if there is no match.
   *
   * @throws NullPointerException When an argument is null.
   */
  def lastOrDefault[T](values: java.lang.Iterable[T], predicate: (T => Boolean)): T =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      var result: T = defaultValue
      var found = false

      val it = values.iterator
      while (it.hasNext) {
        val item = it.next
        if (predicate(item)) {
          val item = it.next
          result = item
          found = true
        }
      }

      if (found)
        result
      else
        null.asInstanceOf[T]
    }

  /**
   * Returns the last index where the specified element is found. You may search for nulls. If the element is not found, this returns -1.
   *
   * @throws NullPointerException If the values argument is null.
   */
  def lastIndexOf[T](values: Traversable[T], element: T): Int =
    {
      if (element == null)
        lastIndexOfNull(values)
      else
        lastIndexOfNotNull(values, element)
    }

  /**
   * Returns the last index where the specified element is found. You may search for nulls. If the element is not found, this returns -1.
   *
   * @throws NullPointerException If the values argument is null.
   */
  def lastIndexOf[T](values: Array[T], element: T): Int =
    {
      if (element == null)
        lastIndexOfNull(values)
      else
        lastIndexOfNotNull(values, element)
    }

  /**
   * Returns the last index where the specified element is found. You may search for nulls. If the element is not found, this returns -1.
   *
   * @throws NullPointerException If the values argument is null.
   */
  def lastIndexOf[T](values: java.lang.Iterable[T], element: T): Int =
    {
      if (element == null)
        lastIndexOfNull(values)
      else
        lastIndexOfNotNull(values, element)
    }

  /**
   * Returns the last index where the specified element is found. You may search for nulls. If the element is not found, this returns -1.
   *
   * @throws NullPointerException If the values argument is null.
   */
  def lastIndexOf[T](values: Traversable[T], element: T, comparison: ((T, T) => Int)): Int =
    {
      if (element == null)
        lastIndexOfNull(values)
      else
        lastIndexOfNotNull(values, element, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Returns the last index where the specified element is found. You may search for nulls. If the element is not found, this returns -1.
   *
   * @throws NullPointerException If the values argument is null.
   */
  def lastIndexOf[T](values: Array[T], element: T, comparison: ((T, T) => Int)): Int =
    {
      if (element == null)
        lastIndexOfNull(values)
      else
        lastIndexOfNotNull(values, element, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Returns the last index where the specified element is found. You may search for nulls. If the element is not found, this returns -1.
   *
   * @throws NullPointerException If the values argument is null.
   */
  def lastIndexOf[T](values: java.lang.Iterable[T], element: T, comparison: ((T, T) => Int)): Int =
    {
      if (element == null)
        lastIndexOfNull(values)
      else
        lastIndexOfNotNull(values, element, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Returns the maximum of the given values. If not values are given, null is returned.
   *
   * @throws NullPointerException An argument is null
   */
  def max[T <: Comparable[T]](items: Array[T]): T =
    {
      if (items == null)
        throw new NullPointerException("items")

      if (items.length <= 0)
        return null.asInstanceOf[T];

      var max = items(0);
      for (item <- items)
        if (max.compareTo(item) < 0)
          max = item;

      return max;
    }

  /**
   * Returns the maximum of the given values. If not values are given, 0 is returned.
   *
   * @throws NullPointerException An argument is null
   */
  def max(items: Array[Int]): Int =
    {
      if (items == null) throw new NullPointerException("items")

      if (items.length <= 0)
        0
      else
        items.max
    }

  /**
   * Returns the maximum of the given values. If not values are given, 0 is returned.
   *
   * @throws NullPointerException An argument is null
   */
  def max(items: Array[Long]): Long =
    {
      if (items == null) throw new NullPointerException("items")

      if (items.length <= 0)
        0L
      else
        items.max
    }

  /**
   * Returns the minimum of the given values. If not values are given, null is returned.
   *
   * @throws NullPointerException An argument is null
   */
  def min[T <: Comparable[T]](items: Array[T]): T = {
    if (items == null)
      throw new NullPointerException("items")

    if (items.length <= 0)
      return null.asInstanceOf[T];

    var min = items(0)
    for (item <- items)
      if (min.compareTo(item) > 0)
        min = item;

    return min;
  }

  /**
   * Returns the minimum of the given values. If not values are given, 0 is returned.
   *
   * @throws NullPointerException An argument is null
   */
  def min(items: Array[Int]): Int =
    {
      if (items == null) throw new NullPointerException("items")

      if (items.length <= 0)
        0
      else {
        var min = items(0)
        for (item <- items)
          if (min > item)
            min = item

        min
      }
    }

  /**
   * Returns the minimum of the given values. If not values are given, 0 is returned.
   *
   * @throws NullPointerException An argument is null
   */
  def min(items: Array[Long]): Long =
    {
      if (items == null) throw new NullPointerException("items")

      if (items.length <= 0)
        0
      else {
        var min = items(0)
        for (item <- items)
          if (min > item)
            min = item

        min
      }
    }

  /**
   * Returns the item with the most occurrences in the given Traversable. If no items are given then null is returned. This method overload uses
   * the natural item ordering for grouping.
   *
   * @throws NullPointerException An argument is null
   */
  def maxOccurring[T](items: Traversable[T]): T =
    {
      maxOccurring(items, null)
    }

  /**
   * Returns the item with the most occurrences in the given array. If no items are given then null is returned. This method overload uses
   * the natural item ordering for grouping.
   *
   * @throws NullPointerException An argument is null
   */
  def maxOccurring[T](items: Array[T]): T =
    {
      maxOccurring(items, null)
    }

  /**
   * Returns the item with the most occurrences in the given JDK iterable. If no items are given then null is returned. This method overload uses
   * the natural item ordering for grouping.
   *
   * @throws NullPointerException An argument is null
   */
  def maxOccurring[T](items: java.lang.Iterable[T]): T =
    {
      maxOccurring(items, null)
    }

  /**
   * Returns the item with the most occurrences in the given Traversable. If no items are given then null is returned. This method overload
   * uses the specified comparator for grouping.
   *
   * @throws NullPointerException An argument is null
   */
  def maxOccurring[T](items: Traversable[T], comparison: ((T, T) => Int)): T =
    {
      if (items == null) throw new NullPointerException("items")
      var lookup: TreeMap[T, ModuloCounter] = null

      if (comparison == null)
        lookup = new TreeMap[T, ModuloCounter]()
      else
        lookup = new TreeMap[T, ModuloCounter](ComparatorUtils.toComparator(comparison))

      for (item <- items)
        if (!lookup.containsKey(item))
          lookup.put(item, new ModuloCounter(Long.MaxValue - 1))
        else
          lookup.get(item).next()

      if (lookup.size() <= 0)
        null.asInstanceOf[T]
      else {
        var max = -1L
        var result: T = defaultValue

        val it = lookup.entrySet.iterator
        while (it.hasNext) {
          val kvp = it.next

          if (kvp.getValue().getValue() > max) {
            max = kvp.getValue().getValue()
            result = kvp.getKey()
          }
        }

        result
      }
    }

  /**
   * Returns the item with the most occurrences in the given array. If no items are given then null is returned. This method overload
   * uses the specified comparator for grouping.
   *
   * @throws NullPointerException An argument is null
   */
  def maxOccurring[T](items: Array[T], comparison: ((T, T) => Int)): T =
    {
      if (items == null) throw new NullPointerException("items")
      var lookup: TreeMap[T, ModuloCounter] = null

      if (comparison == null)
        lookup = new TreeMap[T, ModuloCounter]()
      else
        lookup = new TreeMap[T, ModuloCounter](ComparatorUtils.toComparator(comparison))

      for (item <- items)
        if (!lookup.containsKey(item))
          lookup.put(item, new ModuloCounter(Long.MaxValue - 1))
        else
          lookup.get(item).next()

      if (lookup.size() <= 0)
        null.asInstanceOf[T]
      else {
        var max = -1L
        var result: T = defaultValue

        val it = lookup.entrySet.iterator
        while (it.hasNext) {
          val kvp = it.next

          if (kvp.getValue().getValue() > max) {
            max = kvp.getValue().getValue()
            result = kvp.getKey()
          }
        }

        result
      }
    }

  /**
   * Returns the item with the most occurrences in the given JDK iterable. If no items are given then null is returned. This method overload
   * uses the specified comparator for grouping.
   *
   * @throws NullPointerException An argument is null
   */
  def maxOccurring[T](items: java.lang.Iterable[T], comparison: ((T, T) => Int)): T =
    {
      if (comparison == null)
        propel.core.utils.Linq.maxOccurring(items)
      else
        propel.core.utils.Linq.maxOccurring(items, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Returns the item with the least occurrences in the given Traversable. If no items are given then null is returned. This method overload uses
   * the natural item ordering for grouping.
   *
   * @throws NullPointerException An argument is null
   */
  def minOccurring[T](items: Traversable[T]): T =
    {
      minOccurring(items, null)
    }

  /**
   * Returns the item with the least occurrences in the given array. If no items are given then null is returned. This method overload uses
   * the natural item ordering for grouping.
   *
   * @throws NullPointerException An argument is null
   */
  def minOccurring[T](items: Array[T]): T =
    {
      minOccurring(items, null)
    }

  /**
   * Returns the item with the least occurrences in the given JDK iterable. If no items are given then null is returned. This method overload uses
   * the natural item ordering for grouping.
   *
   * @throws NullPointerException An argument is null
   */
  def minOccurring[T](items: java.lang.Iterable[T]): T =
    {
      minOccurring(items, null)
    }

  /**
   * Returns the item with the least occurrences in the given Traversable. If no items are given then null is returned. This method overload uses
   * the natural item ordering for grouping.
   *
   * @throws NullPointerException An argument is null
   */
  def minOccurring[T](items: Traversable[T], comparison: ((T, T) => Int)): T =
    {
      if (items == null) throw new NullPointerException("items")

      if (items.size <= 0)
        null.asInstanceOf[T]
      else {
        var lookup: TreeMap[T, ModuloCounter] = null

        if (comparison == null)
          lookup = new TreeMap[T, ModuloCounter]()
        else
          lookup = new TreeMap[T, ModuloCounter](ComparatorUtils.toComparator(comparison))

        for (item <- items)
          if (!lookup.containsKey(item))
            lookup.put(item, new ModuloCounter(Long.MaxValue - 1))
          else
            lookup.get(item).next()

        var min = Long.MaxValue
        var result: T = defaultValue

        val it = lookup.entrySet.iterator
        while (it.hasNext) {
          val kvp = it.next

          if (kvp.getValue().getValue() < min) {
            min = kvp.getValue().getValue()
            result = kvp.getKey()
          }
        }

        result
      }
    }

  /**
   * Returns the item with the least occurrences in the given array. If no items are given then null is returned. This method overload uses
   * the natural item ordering for grouping.
   *
   * @throws NullPointerException An argument is null
   */
  def minOccurring[T](items: Array[T], comparison: ((T, T) => Int)): T =
    {
      if (items == null) throw new NullPointerException("items")

      if (items.length <= 0)
        null.asInstanceOf[T]
      else {
        var lookup: TreeMap[T, ModuloCounter] = null

        if (comparison == null)
          lookup = new TreeMap[T, ModuloCounter]()
        else
          lookup = new TreeMap[T, ModuloCounter](ComparatorUtils.toComparator(comparison))

        for (item <- items)
          if (!lookup.containsKey(item))
            lookup.put(item, new ModuloCounter(Long.MaxValue - 1))
          else
            lookup.get(item).next()

        var min = Long.MaxValue
        var result: T = defaultValue

        val it = lookup.entrySet.iterator
        while (it.hasNext) {
          val kvp = it.next

          if (kvp.getValue().getValue() < min) {
            min = kvp.getValue().getValue()
            result = kvp.getKey()
          }
        }

        result
      }
    }

  /**
   * Returns the item with the least occurrences in the given JDK Iterable. If no items are given then null is returned. This method overload uses
   * the natural item ordering for grouping.
   *
   * @throws NullPointerException An argument is null
   */
  def minOccurring[T](items: java.lang.Iterable[T], comparison: ((T, T) => Int)): T =
    {
      if (comparison == null)
        propel.core.utils.Linq.minOccurring(items)
      else
        propel.core.utils.Linq.minOccurring(items, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Performs an inner join (more specifically an equi-join) over two Traversables. The outer values are filtered for key uniqueness (first
   * encountered key kept), whereas inner values may be non-unique.
   *
   * @throws NullPointerException If an argument is null.
   */
  def join[TOuter, TInner, TKey <: Comparable[TKey], TResult](outerValues: Traversable[TOuter], innerValues: Traversable[TInner],
    outerKeySelector: (TOuter => TKey), innerKeySelector: (TInner => TKey),
    resultSelector: ((TOuter, TInner) => TResult)): Traversable[TResult] =
    {
      if (outerValues == null) throw new NullPointerException("outerValues")
      if (innerValues == null) throw new NullPointerException("innerValues")
      if (outerKeySelector == null) throw new NullPointerException("outerKeySelector")
      if (innerKeySelector == null) throw new NullPointerException("innerKeySelector")
      if (resultSelector == null) throw new NullPointerException("resultSelector")

      val lookup = new TreeMap[TKey, TOuter]()

      for (outer <- outerValues) {
        val outerKey = outerKeySelector(outer)
        lookup.put(outerKey, outer)
      }

      val result = new ArrayBuffer[TResult](DEFAULT_LIST_SIZE)

      for (inner <- innerValues) {
        val innerKey = innerKeySelector(inner)

        if (lookup.containsKey(innerKey)) {
          val outer = lookup.get(innerKey)
          val res = resultSelector(outer, inner)
          result += res
        }
      }

      result
    }

  /**
   * Performs an inner join (more specifically an equi-join) over two arrays. The outer values are filtered for key uniqueness (first
   * encountered key kept), whereas inner values may be non-unique.
   *
   * @throws NullPointerException If an argument is null.
   */
  def join[TOuter, TInner, TKey <: Comparable[TKey], TResult](outerValues: Array[TOuter], innerValues: Array[TInner],
    outerKeySelector: (TOuter => TKey), innerKeySelector: (TInner => TKey),
    resultSelector: ((TOuter, TInner) => TResult))(implicit manifest: ClassTag[TResult]): Array[TResult] =
    {
      if (outerValues == null) throw new NullPointerException("outerValues")
      if (innerValues == null) throw new NullPointerException("innerValues")
      if (outerKeySelector == null) throw new NullPointerException("outerKeySelector")
      if (innerKeySelector == null) throw new NullPointerException("innerKeySelector")
      if (resultSelector == null) throw new NullPointerException("resultSelector")

      val lookup = new TreeMap[TKey, TOuter]()

      for (outer <- outerValues) {
        val outerKey = outerKeySelector(outer)
        lookup.put(outerKey, outer)
      }

      val result = new ArrayBuffer[TResult](DEFAULT_LIST_SIZE)

      for (inner <- innerValues) {
        val innerKey = innerKeySelector(inner)

        if (lookup.containsKey(innerKey)) {
          val outer = lookup.get(innerKey)
          val res = resultSelector(outer, inner)
          result += res
        }
      }

      result.toArray
    }

  /**
   * Performs an inner join (more specifically an equi-join) over two JDK Iterables. The outer values are filtered for key uniqueness (first
   * encountered key kept), whereas inner values may be non-unique.
   *
   * @throws NullPointerException If an argument is null.
   */
  def join[TOuter, TInner, TKey <: Comparable[TKey], TResult](outerValues: java.lang.Iterable[TOuter], innerValues: java.lang.Iterable[TInner],
    outerKeySelector: (TOuter => TKey), innerKeySelector: (TInner => TKey),
    resultSelector: ((TOuter, TInner) => TResult)): java.lang.Iterable[TResult] =
    {
      if (outerValues == null) throw new NullPointerException("outerValues")
      if (innerValues == null) throw new NullPointerException("innerValues")
      if (outerKeySelector == null) throw new NullPointerException("outerKeySelector")
      if (innerKeySelector == null) throw new NullPointerException("innerKeySelector")
      if (resultSelector == null) throw new NullPointerException("resultSelector")

      val lookup = new TreeMap[TKey, TOuter]()

      val outerIt = outerValues.iterator
      while (outerIt.hasNext) {
        val outer = outerIt.next
        val outerKey = outerKeySelector(outer)
        lookup.put(outerKey, outer)
      }

      val result = new ArrayList[TResult](DEFAULT_LIST_SIZE)

      val innerIt = innerValues.iterator
      while (innerIt.hasNext) {
        val inner = innerIt.next
        val innerKey = innerKeySelector(inner)

        if (lookup.containsKey(innerKey)) {
          val outer = lookup.get(innerKey)
          val res = resultSelector(outer, inner)
          result.add(res)
        }
      }

      result
    }

  /**
   * Returns all values in a Traversable that are of a particular type. This operates differently to Cast, in that it does not force a cast it
   * rather checks if a TSource is of TDest type.
   *
   * @throws NullPointerException When the argument is null.
   */
  def ofType[TSource, TDest](values: Traversable[TSource], destinationClass: Class[TDest]): Traversable[TDest] =
    {
      if (values == null) throw new NullPointerException("values")
      if (destinationClass == null) throw new NullPointerException("destinationClass")

      val result = new ArrayBuffer[TDest](DEFAULT_LIST_SIZE)
      for (item <- values) {
        var temp: TDest = defaultValue
        try {
          temp = destinationClass.cast(item)
          result += temp
        } catch {
          case e: ClassCastException => {}
        }
      }

      result
    }

  /**
   * Returns all values in an array that are of a particular type. This operates differently to Cast, in that it does not force a cast it
   * rather checks if a TSource is of TDest type.
   *
   * @throws NullPointerException When the argument is null.
   */
  def ofType[TSource, TDest](values: Array[TSource], destinationClass: Class[TDest])(implicit manifest: ClassTag[TDest]): Array[TDest] =
    {
      if (values == null) throw new NullPointerException("values")
      if (destinationClass == null) throw new NullPointerException("destinationClass")

      val result = new ArrayBuffer[TDest](DEFAULT_LIST_SIZE)
      for (item <- values) {
        var temp: TDest = defaultValue
        try {
          temp = destinationClass.cast(item)
          result += temp
        } catch {
          case e: ClassCastException => {}
        }
      }

      result.toArray
    }

  /**
   * Returns all values in a JDK Iterable that are of a particular type. This operates differently to Cast, in that it does not force a cast it
   * rather checks if a TSource is of TDest type.
   *
   * @throws NullPointerException When the argument is null.
   */
  def ofType[TSource, TDest](values: java.lang.Iterable[TSource], destinationClass: Class[TDest]): java.lang.Iterable[TDest] =
    {
      if (values == null) throw new NullPointerException("values")
      if (destinationClass == null) throw new NullPointerException("destinationClass")

      val result = new ArrayList[TDest](DEFAULT_LIST_SIZE)
      val it = values.iterator
      while (it.hasNext) {
        val item = it.next
        var temp: TDest = defaultValue
        try {
          temp = destinationClass.cast(item)
          result.add(temp)
        } catch {
          case e: ClassCastException => {}
        }
      }

      result
    }

  /**
   * Orders a Traversable by a specified key.
   *
   * @throws NullPointerException When an argument is null.
   */
  def orderBy[TKey <: Comparable[TKey], TResult](values: Traversable[TResult], keySelector: (TResult => TKey)): Traversable[TResult] =
    {
      orderBy(values, keySelector, null)
    }

  /**
   * Orders an array by a specified key.
   *
   * @throws NullPointerException When an argument is null.
   */
  def orderBy[TKey <: Comparable[TKey], TResult](values: Array[TResult], keySelector: (TResult => TKey))(implicit manifest: ClassTag[TResult]): Array[TResult] =
    {
      orderBy(values, keySelector, null)
    }

  /**
   * Orders a JDK iterable by a specified key.
   *
   * @throws NullPointerException When an argument is null.
   */
  def orderBy[TKey <: Comparable[TKey], TResult](values: java.lang.Iterable[TResult], keySelector: (TResult => TKey)): java.lang.Iterable[TResult] =
    {
      orderBy(values, keySelector, null)
    }

  /**
   * Orders a Traversable by a specified key.
   *
   * @throws NullPointerException When an argument is null.
   */
  def orderBy[TKey, TResult](values: Traversable[TResult], keySelector: (TResult => TKey), comparison: ((TKey, TKey) => Int)): Traversable[TResult] =
    {
      if (values == null) throw new NullPointerException("values")
      if (keySelector == null) throw new NullPointerException("keySelector")

      var dict: TreeMap[TKey, ArrayBuffer[TResult]] = null
      if (comparison == null)
        dict = new TreeMap[TKey, ArrayBuffer[TResult]]()
      else
        dict = new TreeMap[TKey, ArrayBuffer[TResult]](ComparatorUtils.toComparator(comparison))

      for (item <- values) {
        val key = keySelector(item)
        if (dict.containsKey(key))
          dict.get(key) += item
        else {
          val v = new ArrayBuffer[TResult]()
          v += item
          dict.put(key, v)
        }
      }

      val result = new ArrayBuffer[TResult](values.size)
      val it = dict.values.iterator
      while (it.hasNext) {
        val list = it.next
        result ++= list
      }

      result
    }

  /**
   * Orders a Traversable by a specified key.
   *
   * @throws NullPointerException When an argument is null.
   */
  def orderBy[TKey, TResult](values: Array[TResult], keySelector: (TResult => TKey), comparison: ((TKey, TKey) => Int))(implicit manifest: ClassTag[TResult]): Array[TResult] =
    {
      if (values == null) throw new NullPointerException("values")
      if (keySelector == null) throw new NullPointerException("keySelector")

      var dict: TreeMap[TKey, ArrayBuffer[TResult]] = null
      if (comparison == null)
        dict = new TreeMap[TKey, ArrayBuffer[TResult]]()
      else
        dict = new TreeMap[TKey, ArrayBuffer[TResult]](ComparatorUtils.toComparator(comparison))

      for (item <- values) {
        val key = keySelector(item)
        if (dict.containsKey(key))
          dict.get(key) += item
        else {
          val v = new ArrayBuffer[TResult]()
          v += item
          dict.put(key, v)
        }
      }

      val result = new ArrayBuffer[TResult](values.size)
      val it = dict.values.iterator
      while (it.hasNext) {
        val list = it.next
        result ++= list
      }

      result.toArray
    }

  /**
   * Orders a JDK Iterable by a specified key.
   *
   * @throws NullPointerException When an argument is null.
   */
  def orderBy[TKey, TResult](values: java.lang.Iterable[TResult], keySelector: (TResult => TKey), comparison: ((TKey, TKey) => Int)): java.lang.Iterable[TResult] =
    {
      if (values == null) throw new NullPointerException("values")
      if (keySelector == null) throw new NullPointerException("keySelector")

      var dict: TreeMap[TKey, ArrayList[TResult]] = null
      if (comparison == null)
        dict = new TreeMap[TKey, ArrayList[TResult]]()
      else
        dict = new TreeMap[TKey, ArrayList[TResult]](ComparatorUtils.toComparator(comparison))

      val valueIt = values.iterator
      while (valueIt.hasNext) {
        val item = valueIt.next
        val key = keySelector(item)
        if (dict.containsKey(key))
          dict.get(key).add(item)
        else {
          val v = new ArrayList[TResult]()
          v.add(item)
          dict.put(key, v)
        }
      }

      val result = new ArrayList[TResult](DEFAULT_LIST_SIZE)
      val it = dict.values.iterator
      while (it.hasNext) {
        val list = it.next
        result.addAll(list)
      }

      result
    }

  /**
   * Orders a sequence by a specified key and matching key results get sorted by a second key.
   *
   * @throws NullPointerException When an argument is null.
   */
  def orderByThenBy[TKey <: Comparable[TKey], TKey2 <: Comparable[TKey2], TResult](values: Traversable[TResult],
    keySelector: (TResult => TKey), keySelector2: (TResult => TKey2)): Traversable[TResult] =
    {
      orderByThenBy(values, keySelector, null, keySelector2, null)
    }

  /**
   * Orders a sequence by a specified key and matching key results get sorted by a second key.
   *
   * @throws NullPointerException When an argument is null.
   */
  def orderByThenBy[TKey <: Comparable[TKey], TKey2 <: Comparable[TKey2], TResult](values: Array[TResult],
    keySelector: (TResult => TKey), keySelector2: (TResult => TKey2))(implicit manifest: ClassTag[TResult]): Array[TResult] =
    {
      orderByThenBy(values, keySelector, null, keySelector2, null)
    }

  /**
   * Orders a sequence by a specified key and matching key results get sorted by a second key.
   *
   * @throws NullPointerException When an argument is null.
   */
  def orderByThenBy[TKey <: Comparable[TKey], TKey2 <: Comparable[TKey2], TResult](values: java.lang.Iterable[TResult],
    keySelector: (TResult => TKey), keySelector2: (TResult => TKey2)): java.lang.Iterable[TResult] =
    {
      orderByThenBy(values, keySelector, null, keySelector2, null)
    }

  /**
   * Orders a Traversable by a specified key and matching key results get sorted by a second key.
   *
   * @throws NullPointerException When an argument is null.
   */
  def orderByThenBy[TKey, TKey2, TResult](values: Traversable[TResult],
    keySelector: (TResult => TKey), comparison: ((TKey, TKey) => Int),
    keySelector2: (TResult => TKey2), comparison2: ((TKey2, TKey2) => Int)): Traversable[TResult] =
    {
      var dict: TreeMap[TKey, TreeMap[TKey2, ArrayBuffer[TResult]]] = null
      if (comparison == null)
        dict = new TreeMap[TKey, TreeMap[TKey2, ArrayBuffer[TResult]]]()
      else
        dict = new TreeMap[TKey, TreeMap[TKey2, ArrayBuffer[TResult]]](ComparatorUtils.toComparator(comparison))

      for (item <- values) {
        val key = keySelector.apply(item)
        val key2 = keySelector2.apply(item)

        if (dict.containsKey(key)) {
          if (dict.get(key).containsKey(key2))
            dict.get(key).get(key2) += item
          else {
            val lst = new ArrayBuffer[TResult]()
            lst += item
            dict.get(key).put(key2, lst)
          }
        } else {
          var secondDictionary: TreeMap[TKey2, ArrayBuffer[TResult]] = null

          if (comparison2 == null)
            secondDictionary = new TreeMap[TKey2, ArrayBuffer[TResult]]()
          else
            secondDictionary = new TreeMap[TKey2, ArrayBuffer[TResult]](ComparatorUtils.toComparator(comparison2))

          val lst = new ArrayBuffer[TResult]()
          lst += item
          secondDictionary.put(key2, lst)
          dict.put(key, secondDictionary)
        }
      }

      // get all lists and combine into one resultant list
      val result = new ArrayBuffer[TResult](DEFAULT_LIST_SIZE)
      // get all secondary dictionaries
      val valueIt = dict.values.iterator
      while (valueIt.hasNext) {
        val tree = valueIt.next
        val treeIt = tree.values.iterator
        while (treeIt.hasNext) {
          val list = treeIt.next
          result ++= list
        }
      }

      result
    }

  /**
   * Orders an array by a specified key and matching key results get sorted by a second key.
   *
   * @throws NullPointerException When an argument is null.
   */
  def orderByThenBy[TKey, TKey2, TResult](values: Array[TResult],
    keySelector: (TResult => TKey), comparison: ((TKey, TKey) => Int),
    keySelector2: (TResult => TKey2), comparison2: ((TKey2, TKey2) => Int))(implicit manifest: ClassTag[TResult]): Array[TResult] =
    {
      var dict: TreeMap[TKey, TreeMap[TKey2, ArrayBuffer[TResult]]] = null
      if (comparison == null)
        dict = new TreeMap[TKey, TreeMap[TKey2, ArrayBuffer[TResult]]]()
      else
        dict = new TreeMap[TKey, TreeMap[TKey2, ArrayBuffer[TResult]]](ComparatorUtils.toComparator(comparison))

      for (item <- values) {
        val key = keySelector.apply(item)
        val key2 = keySelector2.apply(item)

        if (dict.containsKey(key)) {
          if (dict.get(key).containsKey(key2))
            dict.get(key).get(key2) += item
          else {
            val lst = new ArrayBuffer[TResult]()
            lst += item
            dict.get(key).put(key2, lst)
          }
        } else {
          var secondDictionary: TreeMap[TKey2, ArrayBuffer[TResult]] = null

          if (comparison2 == null)
            secondDictionary = new TreeMap[TKey2, ArrayBuffer[TResult]]()
          else
            secondDictionary = new TreeMap[TKey2, ArrayBuffer[TResult]](ComparatorUtils.toComparator(comparison2))

          val lst = new ArrayBuffer[TResult]()
          lst += item
          secondDictionary.put(key2, lst)
          dict.put(key, secondDictionary)
        }
      }

      // get all lists and combine into one resultant list
      val result = new ArrayBuffer[TResult](DEFAULT_LIST_SIZE)
      // get all secondary dictionaries
      val valueIt = dict.values.iterator
      while (valueIt.hasNext) {
        val tree = valueIt.next
        val treeIt = tree.values.iterator
        while (treeIt.hasNext) {
          val list = treeIt.next
          result ++= list
        }
      }

      result.toArray
    }

  /**
   * Orders a JDK Iterable by a specified key and matching key results get sorted by a second key.
   *
   * @throws NullPointerException When an argument is null.
   */
  def orderByThenBy[TKey, TKey2, TResult](values: java.lang.Iterable[TResult],
    keySelector: (TResult => TKey), comparison: ((TKey, TKey) => Int),
    keySelector2: (TResult => TKey2), comparison2: ((TKey2, TKey2) => Int)): java.lang.Iterable[TResult] =
    {
      var dict: TreeMap[TKey, TreeMap[TKey2, ArrayList[TResult]]] = null
      if (comparison == null)
        dict = new TreeMap[TKey, TreeMap[TKey2, ArrayList[TResult]]]()
      else
        dict = new TreeMap[TKey, TreeMap[TKey2, ArrayList[TResult]]](ComparatorUtils.toComparator(comparison))

      val valuesIt = values.iterator
      while (valuesIt.hasNext) {
        val item = valuesIt.next
        val key = keySelector.apply(item)
        val key2 = keySelector2.apply(item)

        if (dict.containsKey(key)) {
          if (dict.get(key).containsKey(key2))
            dict.get(key).get(key2).add(item)
          else {
            val lst = new ArrayList[TResult]()
            lst.add(item)
            dict.get(key).put(key2, lst)
          }
        } else {
          var secondDictionary: TreeMap[TKey2, ArrayList[TResult]] = null

          if (comparison2 == null)
            secondDictionary = new TreeMap[TKey2, ArrayList[TResult]]()
          else
            secondDictionary = new TreeMap[TKey2, ArrayList[TResult]](ComparatorUtils.toComparator(comparison2))

          val lst = new ArrayList[TResult]()
          lst.add(item)
          secondDictionary.put(key2, lst)
          dict.put(key, secondDictionary)
        }
      }

      // get all lists and combine into one resultant list
      val result = new ArrayList[TResult](DEFAULT_LIST_SIZE)
      // get all secondary dictionaries
      val valueIt = dict.values.iterator
      while (valueIt.hasNext) {
        val tree = valueIt.next
        val treeIt = tree.values.iterator
        while (treeIt.hasNext) {
          val list = treeIt.next
          result.addAll(list)
        }
      }

      result
    }

  /**
   * Partitions the given Traversable using the specified predicate. Items matching the predicate are
   * first in the returned pair. Non-matching items are second.
   *
   * @throws NullPointerException The values argument is null.
   */
  def partition[T](values: Traversable[T], predicate: (T => Boolean)): Pair[Traversable[T], Traversable[T]] =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      val first = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      val second = new ArrayBuffer[T](DEFAULT_LIST_SIZE)

      for (item <- values)
        if (predicate(item))
          first += item
        else
          second += item

      new Pair[Traversable[T], Traversable[T]](first, second)
    }

  /**
   * Partitions the given array using the specified predicate. Items matching the predicate are
   * first in the returned pair. Non-matching items are second.
   *
   * @throws NullPointerException The values argument is null.
   */
  def partition[T](values: Array[T], predicate: (T => Boolean))(implicit manifest: ClassTag[T]): Pair[Array[T], Array[T]] =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      val first = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      val second = new ArrayBuffer[T](DEFAULT_LIST_SIZE)

      for (item <- values)
        if (predicate(item))
          first += item
        else
          second += item

      new Pair[Array[T], Array[T]](first.toArray, second.toArray)
    }

  /**
   * Partitions the given Traversable using the specified predicate. Items matching the predicate are
   * first in the returned pair. Non-matching items are second.
   *
   * @throws NullPointerException The values argument is null.
   */
  def partition[T](values: java.lang.Iterable[T], predicate: (T => Boolean)): Pair[java.lang.Iterable[T], java.lang.Iterable[T]] =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      val first = new ArrayList[T](DEFAULT_LIST_SIZE)
      val second = new ArrayList[T](DEFAULT_LIST_SIZE)

      val it = values.iterator
      while (it.hasNext) {
        val item = it.next
        if (predicate(item))
          first.add(item)
        else
          second.add(item)
      }

      new Pair[java.lang.Iterable[T], java.lang.Iterable[T]](first, second)
    }

  /**
   * Returns a range from the provided Traversable. Inclusiveness is [start, finish) i.e. as in a For loop.
   *
   * @throws NullPointerException The values argument is null.
   * @throws IndexOutOfBoundsException An index is out of range.
   */
  def range[T](values: Traversable[T], start: Int, finish: Int): Traversable[T] =
    {
      if (values == null) throw new NullPointerException("values")
      if (start < 0) throw new IndexOutOfBoundsException("start=" + start)
      if (finish < start || finish > values.size) throw new IndexOutOfBoundsException("start=" + start + " finish=" + finish + " size=" + values.size)

      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      var index = 0

      breakable {
        for (item <- values) {
          if (index >= finish)
            break
          if (index >= start)
            result += item

          index = index + 1
        }
      }

      if (index < finish)
        throw new IndexOutOfBoundsException("max=" + index + " finish=" + finish)

      result
    }

  /**
   * Returns a range from the provided array. Inclusiveness is [start, finish) i.e. as in a For loop.
   *
   * @throws NullPointerException The values argument is null.
   * @throws IndexOutOfBoundsException An index is out of range.
   */
  def range[T](values: Array[T], start: Int, finish: Int)(implicit manifest: ClassTag[T]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")
      if (start < 0) throw new IndexOutOfBoundsException("start=" + start)
      if (finish < start || finish > values.length) throw new IndexOutOfBoundsException("start=" + start + " finish=" + finish + " length=" + values.length)

      val result = new ArrayBuffer[T](finish - start)

      for (i <- start to finish)
        result += values(i)

      result.toArray(manifest)
    }

  /**
   * Returns a range from the provided JDK Iterable. Inclusiveness is [start, finish) i.e. as in a For loop.
   *
   * @throws NullPointerException The values argument is null.
   * @throws IndexOutOfBoundsException An index is out of range.
   */
  def range[T](values: java.lang.Iterable[T], start: Int, finish: Int): java.lang.Iterable[T] =
    {
      propel.core.utils.Linq.range(values, start, finish)
    }

  /**
   * Returns a range of values, from start to end (exclusive). The value is incremented using the specified stepping function.
   *
   * @throws NullPointerException The step function argument is null.
   */
  def range[T](start: T, end: T, stepFunction: (T => T)): Traversable[T] =
    {
      if (start == null) throw new NullPointerException("start")
      if (end == null) throw new NullPointerException("end")
      if (stepFunction == null) throw new NullPointerException("stepFunction")

      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      var current: T = start

      if (start == null || end == null)
        while (current != end) {
          result += current
          current = stepFunction.apply(current)
        }
      else
        while (!current.equals(end)) {
          result += current
          current = stepFunction.apply(current)
        }

      result
    }

  /**
   * Returns a range of values, by using a step function, until the predicate returns false
   *
   * @throws NullPointerException The predicate or step function argument is null.
   */
  def range[T](start: T, predicate: (T => Boolean), stepFunction: (T => T)): Traversable[T] =
    {
      if (start == null) throw new NullPointerException("start")
      if (predicate == null) throw new NullPointerException("predicate")
      if (stepFunction == null) throw new NullPointerException("stepFunction")

      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)

      var current: T = start
      while (predicate(current)) {
        result += current
        current = stepFunction(current)
      }

      result
    }

  /**
   * Returns a collection of specified size
   *
   * @throws IllegalArgumentException The count is out of range.
   * @throws NullPointerException When the destination class is null.
   */
  def repeat[T](value: T, count: Int): Traversable[T] =
    {
      if (value == null) throw new NullPointerException("value")
      if (count < 0) throw new IllegalArgumentException("count=" + count)

      val result = new ArrayBuffer[T](count)
      for (i <- 0 to count)
        result += value

      result
    }

  /**
   * Returns a reversed version of the provided Traversable
   *
   * @throws NullPointerException When the argument is null.
   */
  def reverse[T](values: Traversable[T]): Traversable[T] =
    {
      if (values == null) throw new NullPointerException("values")

      val list = new ArrayBuffer[T](values.size)
      val it = values.toIterator
      while (it.hasNext) {
        val item = it.next
        list += item
      }

      list.reverse
    }

  /**
   * Returns a reversed version of the provided array
   *
   * @throws NullPointerException When the argument is null.
   */
  def reverse[T](values: Array[T]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")

      val length = values.length
      val result = java.lang.reflect.Array.newInstance(values.getClass().getComponentType(), length).asInstanceOf[Array[T]]

      for (i <- 0 to length)
        result(length - i - 1) = values(i)

      result
    }

  /**
   * Returns a reversed version of the provided JDK Iterable
   *
   * @throws NullPointerException When the argument is null.
   */
  def reverse[T](values: java.lang.Iterable[T]): java.lang.Iterable[T] =
    {
      if (values == null) throw new NullPointerException("values")

      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      val it = values.iterator()

      while (it.hasNext) {
        val item = it.next
        result += item
      }

      reverse(values)
    }

  /**
   * Returns all elements in the Traversable using a specified mapping
   *
   * @throws NullPointerException When an argument is null.
   */
  def select[TSource, TResult](values: Traversable[TSource], selector: (TSource => TResult)): Traversable[TResult] =
    {
      if (values == null) throw new NullPointerException("values")
      if (selector == null) throw new NullPointerException("selector")

      values.map(x => selector(x))
    }

  /**
   * Returns all elements in the array using a specified mapping
   *
   * @throws NullPointerException When an argument is null.
   */
  def select[TSource, TResult](values: Array[TSource], selector: (TSource => TResult))(implicit manifest: ClassTag[TResult]): Array[TResult] =
    {
      if (values == null) throw new NullPointerException("values")
      if (selector == null) throw new NullPointerException("selector")

      values.map(x => selector(x))
    }

  /**
   * Returns all elements in the JDK Iterable using a specified mapping
   *
   * @throws NullPointerException When an argument is null.
   */
  def select[TSource, TResult](values: java.lang.Iterable[TSource], selector: (TSource => TResult)): java.lang.Iterable[TResult] =
    {
      if (values == null) throw new NullPointerException("values")

      val result = new ArrayList[TResult](DEFAULT_LIST_SIZE)
      val it = values.iterator
      while (it.hasNext) {
        val item = it.next
        val selection = selector(item)
        result.add(selection)
      }

      result
    }

  /**
   * Acts as a SelectMany LINQ function, to allow selection of Traversables and return all their sub-items.
   *
   * @throws NullPointerException When an argument is null.
   */
  def selectMany[TSource, TResult](values: Traversable[TSource], selector: (TSource => Traversable[TResult])): Traversable[TResult] =
    {
      if (values == null) throw new NullPointerException("values")
      if (selector == null) throw new NullPointerException("selector")

      val result = new ArrayBuffer[TResult](DEFAULT_LIST_SIZE)
      for (item <- values) {
        val subItems = selector(item)
        if (subItems != null)
          for (subItem <- subItems)
            result += subItem
      }

      result
    }

  /**
   * Acts as a SelectMany LINQ function, to allow selection of arrays and return all their sub-items.
   *
   * @throws NullPointerException When an argument is null.
   */
  def selectMany[TSource, TResult](values: Array[TSource], selector: (TSource => Array[TResult]))(implicit manifest: ClassTag[TResult]): Array[TResult] =
    {
      if (values == null) throw new NullPointerException("values")
      if (selector == null) throw new NullPointerException("selector")

      val result = new ArrayBuffer[TResult](DEFAULT_LIST_SIZE)
      for (item <- values) {
        val subItems = selector(item)
        if (subItems != null)
          for (subItem <- subItems)
            result += subItem
      }

      result.toArray
    }

  /**
   * Acts as a SelectMany LINQ function, to allow selection of arrays and return all their sub-items.
   *
   * @throws NullPointerException When an argument is null.
   */
  def selectMany[TSource, TResult](values: java.lang.Iterable[TSource], selector: (TSource => java.lang.Iterable[TResult])): java.lang.Iterable[TResult] =
    {
      if (values == null) throw new NullPointerException("values")
      if (selector == null) throw new NullPointerException("selector")

      val result = new ArrayList[TResult](DEFAULT_LIST_SIZE)
      val valuesIt = values.iterator
      while (valuesIt.hasNext) {
        val item = valuesIt.next
        val subItems = selector(item)
        if (subItems != null) {
          val subValuesIt = subItems.iterator
          while (subValuesIt.hasNext) {
            val subItem = subValuesIt.next
            result.add(subItem)
          }
        }
      }

      result
    }

  /**
   * Returns true if both Traversables have the same values in the exact same positions.
   *
   * @throws NullPointerException An argument is null
   */
  def sequenceEqual[T](values1: Traversable[_ >: T], values2: Traversable[T]): Boolean =
    {
      if (values1 == null) throw new NullPointerException("values1")
      if (values2 == null) throw new NullPointerException("values2")

      if (count(values1) != count(values2))
        false
      else {
        val i1 = values1.toIterator
        val i2 = values2.toIterator

        // enumerate both
        while (i1.hasNext) {
          val v1 = i1.next()
          val v2 = i2.next()

          // compare using Equals if both not null
          if (v1 != null && v2 != null) {
            if (!v1.equals(v2))
              return false
          } else // check if one is null and the other is not
          if (v1 != null || v2 != null)
            return false
        }

        true
      }
    }

  /**
   * Returns true if both arrays have the same values in the exact same positions.
   *
   * @throws NullPointerException An argument is null
   */
  def sequenceEqual[T](values1: Array[_ >: T], values2: Array[T]): Boolean =
    {
      if (values1 == null) throw new NullPointerException("values1")
      if (values2 == null) throw new NullPointerException("values2")

      if (values1.length != values2.length)
        false
      else {
        val count = values1.length
        for (i <- 0 until count) {
          val v1 = values1(i)
          val v2 = values2(i)

          // compare using Equals if both not null
          if (v1 != null && v2 != null) {
            if (!v1.equals(v2))
              return false
          } else // check if one is null and the other is not
          if (v1 != null || v2 != null)
            return false
        }

        true
      }
    }

  /**
   * Returns true if both JDK Iterables have the same values in the exact same positions.
   *
   * @throws NullPointerException An argument is null
   */
  def sequenceEqual[T](values1: java.lang.Iterable[T], values2: java.lang.Iterable[T]): Boolean =
    {
      propel.core.utils.Linq.sequenceEqual(values1, values2)
    }

  /**
   * Returns true if the item sequences within two arrays are equal. Arrays may contain null elements.
   *
   * @throws NullPointerException An array is null
   * @throws IllegalArgumentException Length is out of range.
   * @throws IndexOutOfBoundsException Offsets are out of range.
   * @throws ArithmeticException When very large numbers are used and overflow occurs.
   */
  def sequenceEqual[T](a: Array[T], offsetA: Int, b: Array[T], offsetB: Int, count: Int): Boolean =
    {
      if (a == null) throw new NullPointerException("array1")
      if (b == null) throw new NullPointerException("array2")

      if (count == 0)
        true
      else {
        if (offsetA < 0) throw new IndexOutOfBoundsException("offsetA=" + offsetA)
        if (offsetB < 0) throw new IndexOutOfBoundsException("offsetB=" + offsetB)

        if (count < 0) throw new IllegalArgumentException("count=" + count)

        if (offsetA + count > a.length || offsetA + count < 0) throw new IndexOutOfBoundsException("offsetA=" + offsetA + " count=" + count + " length=" + a.length)
        if (offsetB + count > b.length || offsetB + count < 0) throw new IndexOutOfBoundsException("offsetB=" + offsetB + " count=" + count + " length=" + b.length)

        // comparisons
        for (i <- 0 to count) {
          val elemA = a(offsetA + i)
          val elemB = b(offsetB + i)

          // check not null to use Equals
          if (elemA != null && elemB != null) {
            if (!elemA.equals(elemB))
              return false
          } else if (elemA != null || elemB != null)
            return false
        }

        true
      }
    }

  // TODO: sequenceEqual((a: Array[T], offsetA: Int, b: Array[T], offsetB: Int, count: Int)) for Iterable & Traversable types

  /**
   * Throws an exception if the given Traversable does not have a single element (e.g. none, 2, 3, etc.)
   * If a single element exists, this is returned.
   *
   * @throws NullPointerException When the values argument is null
   * @throws IllegalArgumentException When count is out of range.
   */
  def single[T](values: Traversable[T]): T =
    {
      if (values == null) throw new NullPointerException("values")

      try {
        first(values)
      } catch {
        case e: Exception => throw new IllegalArgumentException("The given iterable should contain a single element", e)
      }
    }

  /**
   * Throws an exception if the given array does not have a single element (e.g. none, 2, 3, etc.)
   * If a single element exists, this is returned.
   *
   * @throws NullPointerException When the values argument is null
   * @throws IllegalArgumentException When count is out of range.
   */
  def single[T](values: Array[T]): T =
    {
      if (values == null) throw new NullPointerException("values")
      if (values.length != 1) throw new IllegalArgumentException("The given array should contain a single element")

      values(0)
    }

  /**
   * Throws an exception if the given JDK Iterable does not have a single element (e.g. none, 2, 3, etc.)
   * If a single element exists, this is returned.
   *
   * @throws NullPointerException When the values argument is null
   * @throws IllegalArgumentException When count is out of range.
   */
  def single[T](values: java.lang.Iterable[T]): T =
    {
      propel.core.utils.Linq.single(values)
    }

  /**
   * Skips up to the specified number of elements in the given Traversable.
   *
   * @throws NullPointerException When the values argument is null
   * @throws IllegalArgumentException When count is out of range.
   */
  def skip[T](values: Traversable[T], count: Int): Traversable[T] =
    {
      if (values == null) throw new NullPointerException("values")
      if (count < 0) throw new IllegalArgumentException("count=" + count)

      // skip phase
      var skipped = 0
      val iterator = values.toIterator
      for (item <- values) {
        if (iterator.hasNext && skipped < count) {
          iterator.next
          skipped += 1
        }
      }

      // return remaining phase
      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      while (iterator.hasNext) {
        result += iterator.next
        skipped += 1
      }

      result
    }

  /**
   * Skips up to the specified number of elements in the given array.
   *
   * @throws NullPointerException When the values argument is null
   * @throws IllegalArgumentException When count is out of range.
   */
  def skip[T](values: Array[T], count: Int)(implicit manifest: ClassTag[T]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")
      if (count < 0) throw new IllegalArgumentException("count=" + count)

      // skip phase
      var skipped = 0
      val iterator = values.toIterator
      for (item <- values) {
        if (iterator.hasNext && skipped < count) {
          iterator.next
          skipped += 1
        }
      }

      // return remaining phase
      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      while (iterator.hasNext) {
        result += iterator.next
        skipped += 1
      }

      result.toArray(manifest)
    }

  /**
   * Skips up to the specified number of elements in the given sequence.
   *
   * @throws NullPointerException When the values argument is null
   * @throws IllegalArgumentException When count is out of range.
   */
  def skip[T](values: java.lang.Iterable[T], count: Int): java.lang.Iterable[T] =
    {
      propel.core.utils.Linq.skip(values, count)
    }

  /**
   * Skips items in the Traversable for which a predicate is true, returning the rest.
   *
   * @throws NullPointerException When an argument is null
   */
  def skipWhile[T](values: Traversable[T], predicate: (T => Boolean)): Traversable[T] =
    {
      if (values == null) throw new NullPointerException("values")

      var skipping = true

      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      for (item <- values) {
        if (skipping) {
          if (!predicate(item)) {
            skipping = false
            result += item
          }
        } else
          result += item
      }

      result
    }

  /**
   * Skips items in the array for which a predicate is true, returning the rest.
   *
   * @throws NullPointerException When an argument is null
   */
  def skipWhile[T](values: Array[T], predicate: (T => Boolean))(implicit manifest: ClassTag[T]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")

      var skipping = true

      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      for (item <- values) {
        if (skipping) {
          if (!predicate(item)) {
            skipping = false
            result += item
          }
        } else
          result += item
      }

      result.toArray
    }

  /**
   * Skips items in the JDK iterable for which a predicate is true, returning the rest.
   *
   * @throws NullPointerException When an argument is null
   */
  def skipWhile[T](values: java.lang.Iterable[T], predicate: (T => Boolean)): java.lang.Iterable[T] =
    {
      if (values == null) throw new NullPointerException("values")

      var skipping = true

      val result = new ArrayList[T](DEFAULT_LIST_SIZE)
      val it = values.iterator
      while (it.hasNext) {
        val item = it.next
        if (skipping) {
          if (!predicate(item)) {
            skipping = false
            result.add(item)
          }
        } else
          result.add(item)
      }

      result
    }

  /**
   * Sorts a Traversable
   *
   * @throws NullPointerException When an argument is null
   */
  def sort[T](values: Traversable[T])(implicit ord: Ordering[T]): Traversable[T] =
    {
      if (values == null) throw new NullPointerException("values")
      val buf = toBuffer(values)
      buf.sorted
    }

  /**
   * Sorts an array
   *
   * @throws NullPointerException When an argument is null
   */
  def sort[T](values: Array[T])(implicit ord: Ordering[T]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")
      values.sorted
    }

  /**
   * Sorts an array
   *
   * @throws NullPointerException When an argument is null
   */
  def sort[T](values: Array[T], comparison: ((T, T) => Boolean)): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")
      if (comparison == null) throw new NullPointerException("comparison")
      values.sortWith(comparison)
    }

  /**
   * Sorts a JDK Iterable in O(n^2)
   *
   * @throws NullPointerException When an argument is null
   */
  def sort[T](values: java.lang.Iterable[T])(implicit ord: Ordering[T]): java.lang.Iterable[T] =
    {
      if (values == null) throw new NullPointerException("values")
      val buffer = toBuffer(values)
      val sorted = buffer.sorted
      toArrayList(sorted)
    }

  /**
   * Sorts a JDK Iterable in O(n^2)
   *
   * @throws NullPointerException When an argument is null
   */
  def sort[T](values: java.lang.Iterable[T], comparison: ((T, T) => Boolean)): java.lang.Iterable[T] =
    {
      if (values == null) throw new NullPointerException("values")
      if (comparison == null) throw new NullPointerException("comparison")
      val buffer = toBuffer(values)
      val sorted = buffer.sortWith(comparison)
      toArrayList(sorted)
    }

  /**
   * Splits a sequence into parts delimited by the specified delimited. Empty entries between delimiters are removed.
   *
   * @throws NullPointerException When an argument is null, or an item in the iterable is null.
   */
  def split[T](values: Traversable[T], delimiter: T)(implicit manifest: ClassTag[T]): Array[Buffer[T]] =
    {
      if (values == null) throw new NullPointerException("values")
      if (delimiter == null) throw new NullPointerException("delimiter")

      val parts = new ArrayBuffer[Buffer[T]](DEFAULT_LIST_SIZE)
      parts += new ArrayBuffer[T](DEFAULT_LIST_SIZE)

      for (item <- values) {
        if (!item.equals(delimiter))
          // not a delimiter, add to parts
          parts(parts.size - 1) += item
        else
          parts += new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      }

      val result = where(parts, elementsExistInTraversable _)
      toArray(result)
    }

  /**
   * Splits a sequence into parts delimited by the specified delimited. Empty entries between delimiters are removed.
   *
   * @throws NullPointerException When an argument is null, or an item in the iterable is null.
   */
  def split[T](values: Array[T], delimiter: T)(implicit manifest: ClassTag[T]): Array[Buffer[T]] =
    {
      if (values == null) throw new NullPointerException("values")
      if (delimiter == null) throw new NullPointerException("delimiter")

      val parts = new ArrayBuffer[Buffer[T]](DEFAULT_LIST_SIZE)
      parts += new ArrayBuffer[T](DEFAULT_LIST_SIZE)

      for (item <- values) {
        if (!item.equals(delimiter))
          // not a delimiter, add to parts
          parts(parts.size - 1) += item
        else
          parts += new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      }

      val result = where(parts, elementsExistInTraversable _)
      toArray(result)
    }

  /**
   * Splits a sequence into parts delimited by the specified delimited. Empty entries between delimiters are removed.
   *
   * @throws NullPointerException When an argument is null, or an item in the iterable is null.
   */
  def split[T](values: java.lang.Iterable[T], delimiter: T)(implicit manifest: ClassTag[T]): Array[java.util.List[T]] =
    {
      if (values == null) throw new NullPointerException("values")
      if (delimiter == null) throw new NullPointerException("delimiter")

      val parts = new ArrayList[java.util.List[T]](DEFAULT_LIST_SIZE)
      parts.add(new ArrayList[T](DEFAULT_LIST_SIZE))

      val it = values.iterator
      while (it.hasNext) {
        val item = it.next
        if (!item.equals(delimiter))
          // not a delimiter, add to parts
          parts.get(parts.size - 1).add(item)
        else
          parts.add(new ArrayList[T](DEFAULT_LIST_SIZE))
      }

      val result = where(parts, elementsExistInList _)
      toArray(result)
    }

  /**
   * Splits a sequence into parts delimited by the specified delimited. Empty entries between delimiters are removed.
   *
   * @throws NullPointerException When an argument is null, or an item in the iterable is null and the comparator does not handle this case.
   */
  def split[T](values: Traversable[T], delimiter: T, comparison: ((T, T) => Int)): Array[Buffer[T]] =
    {
      if (values == null) throw new NullPointerException("values")
      if (delimiter == null) throw new NullPointerException("delimiter")
      if (comparison == null) throw new NullPointerException("comparison")

      val parts = new ArrayBuffer[Buffer[T]](DEFAULT_LIST_SIZE)
      parts += new ArrayBuffer[T](DEFAULT_LIST_SIZE)

      for (item <- values) {
        if (comparison(item, delimiter) != 0)
          // not a delimiter, add to parts
          parts(parts.size - 1) += item
        else
          parts += new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      }

      val result = where(parts, elementsExistInTraversable _)
      toArray(result)
    }

  /**
   * Splits a sequence into parts delimited by the specified delimited. Empty entries between delimiters are removed.
   *
   * @throws NullPointerException When an argument is null, or an item in the iterable is null and the comparator does not handle this case.
   */
  def split[T](values: Array[T], delimiter: T, comparison: ((T, T) => Int)): Array[Buffer[T]] =
    {
      if (values == null) throw new NullPointerException("values")
      if (delimiter == null) throw new NullPointerException("delimiter")
      if (comparison == null) throw new NullPointerException("comparison")

      val parts = new ArrayBuffer[Buffer[T]](DEFAULT_LIST_SIZE)
      parts += new ArrayBuffer[T](DEFAULT_LIST_SIZE)

      for (item <- values) {
        if (comparison(item, delimiter) != 0)
          // not a delimiter, add to parts
          parts(parts.size - 1) += item
        else
          parts += new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      }

      val result = where(parts, elementsExistInTraversable _)
      toArray(result)
    }

  /**
   * Splits a sequence into parts delimited by the specified delimited. Empty entries between delimiters are removed.
   *
   * @throws NullPointerException When an argument is null, or an item in the iterable is null.
   */
  def split[T](values: java.lang.Iterable[T], delimiter: T, comparison: ((T, T) => Int))(implicit manifest: ClassTag[T]): Array[java.util.List[T]] =
    {
      if (values == null) throw new NullPointerException("values")
      if (delimiter == null) throw new NullPointerException("delimiter")
      if (comparison == null) throw new NullPointerException("comparison")

      val parts = new ArrayList[java.util.List[T]](DEFAULT_LIST_SIZE)
      parts.add(new ArrayList[T](DEFAULT_LIST_SIZE))

      val it = values.iterator
      while (it.hasNext) {
        val item = it.next
        if (comparison(item, delimiter) != 0)
          // not a delimiter, add to parts
          parts.get(parts.size - 1).add(item)
        else
          parts.add(new ArrayList[T](DEFAULT_LIST_SIZE))
      }

      val result = where(parts, elementsExistInList _)
      toArray(result)
    }

  /**
   * Swaps two elements in an array.
   *
   * @throws NullPointerException Array is null
   * @throws IndexOutOfBoundsException Array indices are out of range.
   */
  def swap[T](array: Array[T], a: Int, b: Int): Unit =
    {
      if (array == null) throw new NullPointerException("array")
      if (a < 0 || a >= array.length) throw new IndexOutOfBoundsException("a=" + a + " length=" + array.length)
      if (b < 0 || b >= array.length) throw new IndexOutOfBoundsException("b=" + b + " length=" + array.length)

      if (a != b) {
        val value = array(a)
        array(a) = array(b)
        array(b) = value
      }
    }

  /**
   * Swaps two or more elements in an array.
   *
   * @throws NullPointerException Array is null
   * @throws IllegalArgumentException The length of index arrays are not equal.
   * @throws IndexOutOfBoundsException An array index in the indices is out of range
   */
  def swap[T](array: Array[T], a: Array[Int], b: Array[Int]): Unit =
    {
      if (array == null) throw new NullPointerException("array")
      if (a == null) throw new NullPointerException("a")
      if (b == null) throw new NullPointerException("b")

      if (a.length != b.length) throw new IllegalArgumentException("a=" + a.length + " b=" + b.length)

      for (i <- 0 to a.length) {
        val posA = a(i)
        val posB = b(i)

        if (posA < 0 || posA >= array.length) throw new IndexOutOfBoundsException("posA=" + posA + " length=" + array.length)
        if (posB < 0 || posB >= array.length) throw new IndexOutOfBoundsException("posB=" + posB + " length=" + array.length)

        val value = array(posA)
        array(posA) = array(posB)
        array(posB) = value
      }
    }

  // TODO: swap for List/Buffer types

  /**
   * Returns up to the specified number of elements from the given Traversable.
   *
   * @throws NullPointerException The values argument is null.
   */
  def take[T](values: Traversable[T], count: Int): Traversable[T] =
    {
      val it = values.toIterator
      val result = new ArrayBuffer[T](count)

      var index = 0
      while (index < count && it.hasNext) {
        index = index + 1
        result += it.next
      }

      result
    }

  /**
   * Returns up to the specified number of elements from the given array.
   *
   * @throws NullPointerException The values argument is null.
   * @throws IllegalArgumentException The count argument is out of range.
   */
  def take[T](values: Array[T], count: Int)(implicit manifest: ClassTag[T]): Array[T] =
    {
      val result = new ArrayBuffer[T](count)
      var i = 0
      while (i < count && i < values.size) {
        result += values(i)
        i = i + 1
      }

      toArray(result)
    }

  /**
   * Returns up to the specified number of elements from the given array.
   *
   * @throws NullPointerException The values argument is null.
   * @throws IllegalArgumentException The count argument is out of range.
   */
  def take[T](values: java.lang.Iterable[T], count: Int): java.lang.Iterable[T] =
    {
      val result = new ArrayList[T](count)
      val it = values.iterator
      var i = 0
      while (i < count && it.hasNext) {
        result.add(it.next)
        i = i + 1
      }

      result
    }

  /**
   * Returns items in the sequence while a predicate is true. Breaks when the condition is not satisfied.
   *
   * @throws NullPointerException An argument is null.
   */
  def takeWhile[T](values: Traversable[T], predicate: (T => Boolean)): Traversable[T] =
    {
      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)
      breakable {
        for (item <- values)
          if (predicate(item))
            result += item
          else
            break
      }

      result
    }

  /**
   * Returns items in the sequence while a predicate is true. Breaks when the condition is not satisfied.
   *
   * @throws NullPointerException An argument is null.
   */
  def takeWhile[T](values: Array[T], predicate: (T => Boolean))(implicit manifest: ClassTag[T]): Array[T] =
    {
      val result = new ArrayBuffer[T](DEFAULT_LIST_SIZE)

      breakable {
        for (item <- values) {
          if (predicate(item))
            result += item
          else
            break
        }
      }

      return toArray(result)
    }

  /**
   * Returns items in the sequence while a predicate is true. Breaks when the condition is not satisfied.
   *
   * @throws NullPointerException An argument is null.
   */
  def takeWhile[T](values: java.lang.Iterable[T], predicate: (T => Boolean)): java.lang.Iterable[T] =
    {
      val result = new ArrayList[T](DEFAULT_LIST_SIZE)
      breakable {
        val it = values.iterator
        while (it.hasNext) {
          val item = it.next
          if (predicate(item))
            result.add(item)
          else
            break
        }
      }

      result
    }

  /**
   * Converts an enumeration to an array.
   *
   * @throws NullPointerException An argument is null.
   */
  def toArray[T](values: java.util.Enumeration[T])(implicit manifest: ClassTag[T]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")

      val result = new ArrayBuffer[T]()
      while (values.hasMoreElements())
        result += values.nextElement()

      result.toArray(manifest)
    }

  /**
   * Converts an iterable to an array.
   *
   * @throws NullPointerException An argument is nul.
   */
  def toArray[T](values: Traversable[T])(implicit manifest: ClassTag[T]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")
      values.toArray
    }

  /**
   * Converts an iterable to an array.
   *
   * @throws NullPointerException An argument is nul.
   */
  def toArray[T](values: java.lang.Iterable[T])(implicit manifest: ClassTag[T]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")

      val result = new Array[T](DEFAULT_LIST_SIZE)
      val it = values.iterator

      var i = 0
      while (it.hasNext) {
        result(i) = it.next
        i = i + 1
      }

      result
    }

  /**
   * Converts an iterable to an array.
   *
   * @throws NullPointerException An argument is nul.
   */
  def toArray[T](values: java.lang.Iterable[T], componentType: Class[_]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")

      val result = ArrayUtils.create[T](componentType, count(values))
      val it = values.iterator

      var i = 0
      while (it.hasNext) {
        result(i) = it.next
        i = i + 1
      }

      result
    }

  /**
   * Converts a collection to an array.
   *
   * @throws NullPointerException An argument is null.
   */
  def toArray[T](values: Collection[T])(implicit manifest: ClassTag[T]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")

      // count items
      val size = values.size()
      val result = new Array[T](size)

      val it = values.iterator

      var i = 0
      while (it.hasNext) {
        result(i) = it.next
        i = i + 1
      }

      result
    }

  /**
   * Converts a collection to an array.
   *
   * @throws NullPointerException An argument is null.
   */
  def toArray[T](values: Collection[T], componentType: Class[_]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")

      // count items
      val size = values.size()
      val result = ArrayUtils.create[T](componentType, values.size)

      val it = values.iterator

      var i = 0
      while (it.hasNext) {
        result(i) = it.next
        i = i + 1
      }

      result
    }

  /**
   * Converts an enumeration to a list
   *
   * @throws NullPointerException An argument is null.
   */
  def toArrayList[T](values: java.util.Enumeration[T]): ArrayList[T] =
    {
      if (values == null) throw new NullPointerException("values")

      val result = new ArrayList[T]
      while (values.hasMoreElements)
        result.add(values.nextElement)

      result
    }

  /**
   * Converts a Traversable to a list
   *
   * @throws NullPointerException The values argument is null.
   */
  def toArrayList[T](values: Traversable[T]): ArrayList[T] =
    {
      if (values == null) throw new NullPointerException("values")

      val result = new ArrayList[T](values.size)
      for (item <- values)
        result.add(item)

      result
    }

  /**
   * Converts an array to a list
   *
   * @throws NullPointerException The values argument is null.
   */
  def toArrayList[T](values: Array[T]): ArrayList[T] =
    {
      if (values == null) throw new NullPointerException("values")

      val result = new ArrayList[T](values.size)
      for (item <- values)
        result.add(item)

      result
    }

  /**
   * Converts a JDK Iterable to a list
   *
   * @throws NullPointerException The values argument is null.
   */
  def toArrayList[T](values: java.lang.Iterable[T]): ArrayList[T] =
    {
      if (values == null) throw new NullPointerException("values")

      if (values.isInstanceOf[ArrayList[T]])
        values.asInstanceOf[ArrayList[T]]
      else {
        val result = new ArrayList[T](DEFAULT_LIST_SIZE)
        val it = values.iterator
        while (it.hasNext)
          result.add(it.next)

        result
      }
    }

  /**
   * Converts an enumeration to an ArrayBuffer.
   *
   * @throws NullPointerException An argument is null.
   */
  def toBuffer[T](values: java.util.Enumeration[T]): Buffer[T] =
    {
      if (values == null) throw new NullPointerException("values")

      val result = new ArrayBuffer[T]()
      while (values.hasMoreElements())
        result += values.nextElement()

      result
    }

  /**
   * Converts an iterable to an ArrayBuffer.
   *
   * @throws NullPointerException An argument is nul.
   */
  def toBuffer[T](values: Traversable[T]): Buffer[T] =
    {
      if (values == null) throw new NullPointerException("values")

      if (values.isInstanceOf[ArrayBuffer[T]])
        values.asInstanceOf[ArrayBuffer[T]]
      else {
        val result = new ArrayBuffer[T](values.size)
        result ++= values
        result
      }
    }

  /**
   * Converts a collection to an ArrayBuffer.
   *
   * @throws NullPointerException An argument is nul.
   */
  def toBuffer[T](values: Collection[T]): Buffer[T] =
    {
      if (values == null) throw new NullPointerException("values")

      val result = new ArrayBuffer[T](values.size)
      val it = values.iterator
      while (it.hasNext)
        result += it.next

      result
    }

  /**
   * Converts an iterable to an ArrayBuffer.
   *
   * @throws NullPointerException An argument is nul.
   */
  def toBuffer[T](values: java.lang.Iterable[T]): Buffer[T] =
    {
      if (values == null) throw new NullPointerException("values")

      val result = new ArrayBuffer[T]
      val it = values.iterator
      while (it.hasNext)
        result += it.next

      result
    }

  /**
   * Converts an enumeration to an immutable IndexedSeq.
   *
   * @throws NullPointerException An argument is null.
   */
  def toSeq[T](values: java.util.Enumeration[T]): IndexedSeq[T] =
    {
      toBuffer(values).toIndexedSeq
    }

  /**
   * Converts a Traversable to an immutable IndexedSeq.
   *
   * @throws NullPointerException An argument is nul.
   */
  def toSeq[T](values: Traversable[T]): IndexedSeq[T] =
    {
      if (values.isInstanceOf[IndexedSeq[T]])
        values.asInstanceOf[IndexedSeq[T]]
      else if (values.isInstanceOf[Buffer[T]])
        values.asInstanceOf[Buffer[T]].toIndexedSeq
      else
        toBuffer(values).toIndexedSeq
    }

  /**
   * Converts a collection to an immutable IndexedSeq
   *
   * @throws NullPointerException An argument is nul.
   */
  def toSeq[T](values: Collection[T]): IndexedSeq[T] =
    {
      toBuffer(values).toIndexedSeq
    }

  /**
   * Converts an iterable to an immutable IndexedSeq
   *
   * @throws NullPointerException An argument is nul.
   */
  def toSeq[T](values: java.lang.Iterable[T]): IndexedSeq[T] =
    {
      toBuffer(values).toIndexedSeq
    }

  /**
   * Returns a toString() of the given array
   */
  def toString[T](values: Array[T]): String = "["+values.deep.mkString(", ")+"]"

  /**
   * Returns a toString() of the given collection
   */
  def toString[T](values: java.lang.Iterable[T]): String = propel.core.utils.Linq.toString(values)

  /**
   * Returns a toString() of the given collection
   */
  def toString[T](values: Traversable[T]): String = {
    val sb = new StringBuilder();
    sb.append('[');
    values.foreach(x => if(values == x) sb.append("(this Collection)") else sb.append(x).append(", "));
    if(sb.length > 1)
      sb.delete(sb.length-2, sb.length);
    sb.append(']');
    
    sb.toString
  }

  /**
   * Produces the union of two Traversables.
   *
   * @throws NullPointerException When an argument is null.
   */
  def union[T](first: Traversable[T], second: Traversable[T]): Traversable[T] =
    {
      union(first, second, null)
    }

  /**
   * Produces the union of two arrays.
   *
   * @throws NullPointerException When an argument is null.
   */
  def union[T](first: Array[T], second: Array[T])(implicit manifest: ClassTag[T]): Array[T] =
    {
      union(first, second, null)
    }

  /**
   * Produces the union of two JDK Iterables.
   *
   * @throws NullPointerException When an argument is null.
   */
  def union[T](first: java.lang.Iterable[T], second: java.lang.Iterable[T]): java.lang.Iterable[T] =
    {
      propel.core.utils.Linq.union(first, second)
    }

  /**
   * Produces the union of two Traversables.
   *
   * @throws NullPointerException When the first or second argument is null.
   */
  def union[T](first: Traversable[T], second: Traversable[T], comparison: ((T, T) => Int)): Traversable[T] =
    {
      var firstDistinct: Traversable[T] = null
      var secondDistinct: Traversable[T] = null

      firstDistinct = distinct(first, comparison)
      secondDistinct = distinct(second, comparison)
      concat(firstDistinct, secondDistinct)
    }

  /**
   * Produces the union of two arrays.
   *
   * @throws NullPointerException When the first or second argument is null.
   */
  def union[T](first: Array[T], second: Array[T], comparison: ((T, T) => Int))(implicit manifest: ClassTag[T]): Array[T] =
    {
      var firstDistinct: Array[T] = null
      var secondDistinct: Array[T] = null

      firstDistinct = distinct(first, comparison)
      secondDistinct = distinct(second, comparison)
      concat(firstDistinct, secondDistinct)
    }

  /**
   * Produces the union of two JDK Iterables.
   *
   * @throws NullPointerException When the first or second argument is null.
   */
  def union[T](first: java.lang.Iterable[T], second: java.lang.Iterable[T], comparison: ((T, T) => Int)): java.lang.Iterable[T] =
    {
      if (comparison == null)
        propel.core.utils.Linq.union(first, second)
      else
        propel.core.utils.Linq.union(first, second, ComparatorUtils.toComparator(comparison))
    }

  /**
   * Performs the reverse operation to zip()
   *
   * @throws NullPointerException When an argument is null
   */
  def unzip[T, TResult1, TResult2](values: Traversable[T], func: (T => Pair[TResult1, TResult2])): Traversable[Pair[TResult1, TResult2]] =
    {
      if (values == null) throw new NullPointerException("values")

      val result = new ArrayBuffer[Pair[TResult1, TResult2]](values.size)
      for (item <- values) {
        val kvp = func(item)
        result += kvp
      }

      result
    }

  /**
   * Performs the reverse operation to zip()
   *
   * @throws NullPointerException When an argument is null
   */
  def unzip[T, TResult1, TResult2](values: Array[T], func: (T => Pair[TResult1, TResult2]))(implicit manifest: ClassTag[Pair[TResult1, TResult2]]): Array[Pair[TResult1, TResult2]] =
    {
      if (values == null) throw new NullPointerException("values")

      val result = new ArrayBuffer[Pair[TResult1, TResult2]](values.size)
      for (item <- values) {
        val kvp = func(item)
        result += kvp
      }

      result.toArray
    }

  /**
   * Performs the reverse operation to zip()
   *
   * @throws NullPointerException When an argument is null
   */
  def unzip[T, TResult1, TResult2](values: java.lang.Iterable[T], func: (T => Pair[TResult1, TResult2])): java.lang.Iterable[Pair[TResult1, TResult2]] =
    {
      if (values == null) throw new NullPointerException("values")

      val result = new ArrayList[Pair[TResult1, TResult2]](DEFAULT_LIST_SIZE)
      val it = values.iterator
      while (it.hasNext) {
        val item = it.next
        val kvp = func(item)
        result.add(kvp)
      }

      result
    }

  /**
   * Returns a subset of the provided Traversable, which conforms to the given predicate i.e. acts like a Where LINQ function.
   * It will never return null.
   *
   * @throws NullPointerException When an argument is null
   */
  def where[T](values: Traversable[T], predicate: (T => Boolean)): Traversable[T] =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      values.filter(predicate)
    }

  /**
   * Returns a subset of the provided array, which conforms to the given predicate i.e. acts like a Where LINQ function.
   * It will never return null.
   *
   * @throws NullPointerException When an argument is null
   */
  def where[T](values: Array[T], predicate: (T => Boolean))(implicit manifest: ClassTag[T]): Array[T] =
    {
      if (values == null) throw new NullPointerException("values")
      if (predicate == null) throw new NullPointerException("predicate")

      values.filter(predicate).toArray(manifest)
    }

  /**
   * Returns a subset of the provided JDK iterable, which conforms to the given predicate i.e. acts like a Where LINQ function.
   * It will never return null.
   *
   * @throws NullPointerException When an argument is null
   */
  def where[T](values: java.lang.Iterable[T], predicate: (T => Boolean)): java.lang.Iterable[T] =
    {
      if (values == null) throw new NullPointerException("values")

      val result = new ArrayList[T](DEFAULT_LIST_SIZE)
      val it = values.iterator

      while (it.hasNext) {
        val item = it.next
        if (predicate(item))
          result.add(item)
      }

      result
    }

  /**
   * Merges two sequences by using the specified predicate function.
   *
   * @throws NullPointerException When an argument is null.
   */
  def zip[TFirst, TSecond, TResult](first: Traversable[TFirst], second: Traversable[TSecond],
    func: ((TFirst, TSecond) => TResult)): Traversable[TResult] =
    {
      if (first == null) throw new NullPointerException("first")
      if (second == null) throw new NullPointerException("second")
      if (func == null) throw new NullPointerException("func")

      val iterator1 = first.toIterator
      val iterator2 = second.toIterator
      val result = new ArrayBuffer[TResult](DEFAULT_LIST_SIZE)

      while (iterator1.hasNext && iterator2.hasNext) {
        val item = func(iterator1.next, iterator2.next)
        result += item
      }

      result
    }

  /**
   * Merges two sequences by using the specified predicate function.
   *
   * @throws NullPointerException When an argument is null.
   */
  def zip[TFirst, TSecond, TResult](first: Array[TFirst], second: Array[TSecond],
    func: ((TFirst, TSecond) => TResult))(implicit manifest: ClassTag[TResult]): Array[TResult] =
    {
      if (first == null) throw new NullPointerException("first")
      if (second == null) throw new NullPointerException("second")
      if (func == null) throw new NullPointerException("func")

      val iterator1 = first.toIterator
      val iterator2 = second.toIterator
      val result = new ArrayBuffer[TResult](DEFAULT_LIST_SIZE)

      while (iterator1.hasNext && iterator2.hasNext) {
        val item = func(iterator1.next, iterator2.next)
        result += item
      }

      result.toArray
    }

  /**
   * Merges two sequences by using the specified predicate function.
   *
   * @throws NullPointerException When an argument is null.
   */
  def zip[TFirst, TSecond, TResult](first: java.lang.Iterable[TFirst], second: java.lang.Iterable[TSecond],
    func: ((TFirst, TSecond) => TResult)): java.lang.Iterable[TResult] =
    {
      if (first == null) throw new NullPointerException("first")
      if (second == null) throw new NullPointerException("second")
      if (func == null) throw new NullPointerException("func")

      val result = new ArrayList[TResult](DEFAULT_LIST_SIZE)

      val iterator1 = first.iterator
      val iterator2 = second.iterator
      while (iterator1.hasNext && iterator2.hasNext) {
        val item = func(iterator1.next, iterator2.next)
        result.add(item)
      }

      result
    }

  /**
   * Casts a Traversable of values of a certain type to a Traversable of values of another type, throwing an InvalidCastException if any elements
   * are not cast successfully.
   */
  private def castThrow[TSource, TDest](values: Traversable[TSource], destinationClass: Class[TDest]): Traversable[TDest] =
    {
      val buff = new ArrayBuffer[TDest](values.size)

      for (v <- values) {
        val castVal = destinationClass.cast(v).asInstanceOf[TDest]
        buff += castVal
      }

      buff
    }

  /**
   * Casts an array of values of a certain type to an array of values of another type, throwing an InvalidCastException if any elements
   * are not cast successfully.
   */
  private def castThrow[TSource, TDest](values: Array[TSource], destinationClass: Class[TDest]): Array[TDest] =
    {
      val len = values.length
      val result = java.lang.reflect.Array.newInstance(destinationClass, len).asInstanceOf[Array[TDest]]

      for (i <- 0 to len) {
        val castVal = values(i).asInstanceOf[TDest]
        result(i) = castVal
      }

      result
    }

  /**
   * Casts a Traversable of values of a certain type to a Traversable of values of another type, discarding elements that are not cast successfully.
   */
  private def castRemove[TSource, TDest](values: Traversable[TSource], destinationClass: Class[TDest]): Traversable[TDest] =
    {
      val result = new ArrayBuffer[TDest](values.size)

      for (v <- values) {
        var cce = false
        var castVal: TDest = null.asInstanceOf[TDest]
        try {
          castVal = destinationClass.cast(v).asInstanceOf[TDest]
        } catch {
          case e: ClassCastException => cce = true
        }
        if (!cce)
          result += castVal
      }

      result
    }

  /**
   * Casts a sequence of values of a certain type to an array of values of another type, discarding elements that are not cast successfully.
   */
  private def castRemove[TSource, TDest](values: Array[TSource], destinationClass: Class[TDest]): Array[TDest] =
    {
      val len = values.length
      val result = java.lang.reflect.Array.newInstance(destinationClass, len).asInstanceOf[Array[TDest]]

      for (i <- 0 to len) {
        try {
          val castVal = values(i).asInstanceOf[TDest]
          result(i) = castVal
        } catch {
          case e: ClassCastException => { /* skip */ }
        }
      }

      result
    }

  /**
   * Casts a sequence of values of a certain type to an array of values of another type, using nulls for elements that are not cast
   * successfully
   */
  private def castUseDefault[TSource, TDest](values: Traversable[TSource], destinationClass: Class[TDest]): Traversable[TDest] =
    {
      val result = new ArrayBuffer[TDest](values.size)

      for (v <- values) {
        var castVal: TDest = null.asInstanceOf[TDest]
        try {
          castVal = destinationClass.cast(v).asInstanceOf[TDest]
        } catch {
          case e: ClassCastException => {}
        }

        result += castVal
      }

      result
    }

  /**
   * Casts a sequence of values of a certain type to an array of values of another type, using nulls for elements that are not cast
   * successfully
   *
   * @throws NullPointerException An argument is null.
   */
  private def castUseDefault[TSource, TDest](values: Array[TSource], destinationClass: Class[TDest]): Array[TDest] =
    {
      val len = values.length
      val result = java.lang.reflect.Array.newInstance(destinationClass, len).asInstanceOf[Array[TDest]]

      for (i <- 0 to len) {
        var castVal = null.asInstanceOf[TDest]
        try {
          castVal = values(i).asInstanceOf[TDest]
        } catch {
          case e: ClassCastException => {}
        }

        result(i) = castVal
      }

      result
    }

  /**
   * Returns true if a non-null item is contained in the sequence of values
   */
  private def containsNonNull[T](values: Traversable[T], item: T): Boolean =
    {
      if (values == null) throw new NullPointerException("values")
      if (item == null) throw new NullPointerException("item")

      for (x <- values)
        // if a value is null, we cannot use equals
        if (x != null)
          if (x.equals(item))
            return true

      return false
    }

  /**
   * Returns true if a non-null item is contained in the sequence of values
   */
  private def containsNonNull[T](values: Array[T], item: T): Boolean =
    {
      if (values == null) throw new NullPointerException("values")
      if (item == null) throw new NullPointerException("item")

      for (x <- values)
        // if a value is null, we cannot use equals
        if (x != null)
          if (x.equals(item))
            return true

      return false
    }

  /**
   * Returns true if a non-null item is contained in the sequence of values
   */
  private def containsNonNull[T](values: Traversable[T], item: T, comparator: Comparator[T]): Boolean =
    {
      if (values == null) throw new NullPointerException("values")
      if (item == null) throw new NullPointerException("item")
      if (comparator == null) throw new NullPointerException("comparator")

      for (x <- values)
        // if a value is null, we cannot use equals
        if (x != null)
          if (comparator.compare(x, item) == 0)
            return true

      return false
    }

  /**
   * Returns true if a non-null item is contained in the sequence of values
   */
  private def containsNonNull[T](values: Array[T], item: T, comparator: Comparator[T]): Boolean =
    {
      if (values == null) throw new NullPointerException("values")
      if (item == null) throw new NullPointerException("item")
      if (comparator == null) throw new NullPointerException("comparator")

      val count = values.length
      for (i <- 0 to count) {
        val x = values(i)
        // if a value is null, we cannot use equals
        if (x != null)
          if (comparator.compare(x, item) == 0)
            return true
      }

      return false
    }

  /**
   * Returns true if null is contained in the sequence of values
   */
  private def containsNull[T](values: Traversable[T]): Boolean =
    {
      for (x <- values)
        if (x == null)
          return true

      return false
    }

  /**
   * Returns true if null is contained in the sequence of values
   */
  private def containsNull[T](values: Array[T]): Boolean =
    {
      for (x <- values)
        if (x == null)
          return true

      return false
    }

  /**
   * Returns the number of null entries in the Traversable
   */
  private def countNulls[T](values: Traversable[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")

      // check if value contained in sequence
      var result = 0

      // the values is confirmed to be a non-null IEnumerable prior to this
      for (v <- values)
        if (v == null)
          result = result + 1

      result
    }

  /**
   * Returns the number of null entries in the Array
   */
  private def countNulls[T](values: Array[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")

      // check if value contained in sequence
      var result = 0

      // the values is confirmed to be a non-null IEnumerable prior to this
      for (v <- values)
        if (v == null)
          result = result + 1

      result
    }

  /**
   * Returns the number of occurences of a non-null value in the Traversable
   */
  private def countNonNull[T](values: Traversable[T], value: T): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (value == null) throw new NullPointerException("value")

      // check if value contained collection
      var result = 0

      // the values is confirmed to be a non-null IEnumerable prior to this
      for (v <- values)
        if (v != null)
          if (v.equals(value))
            result = result + 1

      result
    }

  /**
   * Returns the number of occurences of a non-null value in the Array
   */
  private def countNonNull[T](values: Array[T], value: T): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (value == null) throw new NullPointerException("value")

      // check if value contained collection
      var result = 0

      // the values is confirmed to be a non-null IEnumerable prior to this
      for (v <- values)
        if (v != null)
          if (v.equals(value))
            result = result + 1

      result
    }

  /**
   * Returns the number of occurences of a non-null value in the collection
   */
  private def countNonNull[T](values: Traversable[T], value: T, comparator: Comparator[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (value == null) throw new NullPointerException("value")
      if (comparator == null) throw new NullPointerException("comparator")

      // check if value contained collection
      var result = 0

      // the values is confirmed to be a non-null IEnumerable prior to this
      for (v <- values)
        if (v != null)
          if (comparator.compare(v, value) == 0)
            result = result + 1

      result
    }

  /**
   * Returns the number of occurences of a non-null value in the collection
   */
  private def countNonNull[T](values: Array[T], value: T, comparator: Comparator[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (value == null) throw new NullPointerException("value")
      if (comparator == null) throw new NullPointerException("comparator")

      // check if value contained collection
      var result = 0

      // the values is confirmed to be a non-null IEnumerable prior to this
      var count = values.length
      for (i <- 0 to count) {
        val v = values(i)
        if (v != null)
          if (comparator.compare(v, value) == 0)
            result = result + 1
      }

      result
    }

  /**
   * Returns the index where the first null element is found. If no null elements are found, this returns -1.
   */
  private def indexOfNull[T](values: Traversable[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")

      var i = 0
      for (item <- values) {
        if (item == null)
          return i

        i = i + 1
      }

      return -1
    }

  /**
   * Returns the index where the first null element is found. If no null elements are found, this returns -1.
   */
  private def indexOfNull[T](values: Array[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")

      var i = 0
      for (item <- values) {
        if (item == null)
          return i

        i = i + 1
      }

      return -1
    }

  /**
   * Returns the index where the first null element is found. If no null elements are found, this returns -1.
   */
  private def indexOfNull[T](values: java.lang.Iterable[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")

      var i = 0
      val it = values.iterator
      while (it.hasNext) {
        val item = it.next
        if (item == null)
          return i

        i = i + 1
      }

      return -1
    }

  /**
   * Returns the index where the specified not-null element is first found. If the element is not found, this returns -1.
   */
  private def indexOfNotNull[T](values: Traversable[T], element: T): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (element == null) throw new NullPointerException("element")

      var i = 0
      for (item <- values) {
        if (element.equals(item))
          return i

        i = i + 1
      }

      return -1
    }

  /**
   * Returns the index where the specified not-null element is first found. If the element is not found, this returns -1.
   */
  private def indexOfNotNull[T](values: Array[T], element: T): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (element == null) throw new NullPointerException("element")

      var i = 0
      for (item <- values) {
        if (element.equals(item))
          return i

        i = i + 1
      }

      return -1
    }

  /**
   * Returns the index where the specified not-null element is first found. If the element is not found, this returns -1.
   */
  private def indexOfNotNull[T](values: java.lang.Iterable[T], element: T): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (element == null) throw new NullPointerException("element")

      var i = 0
      val it = values.iterator
      while (it.hasNext) {
        val item = it.next
        if (element.equals(item))
          return i

        i = i + 1
      }

      return -1
    }

  /**
   * Returns the index where the specified not-null element is first found. If the element is not found, this returns -1.
   */
  private def indexOfNotNull[T](values: Traversable[T], element: T, comparator: Comparator[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (element == null) throw new NullPointerException("element")
      if (comparator == null) throw new NullPointerException("comparator")

      var i = 0
      for (item <- values) {
        if (comparator.compare(element, item) == 0)
          return i

        i = i + 1
      }

      return -1
    }

  /**
   * Returns the index where the specified not-null element is first found. If the element is not found, this returns -1.
   */
  private def indexOfNotNull[T](values: Array[T], element: T, comparator: Comparator[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (element == null) throw new NullPointerException("element")
      if (comparator == null) throw new NullPointerException("comparator")

      var i = 0
      for (item <- values) {
        if (comparator.compare(element, item) == 0)
          return i

        i = i + 1
      }

      return -1
    }

  /**
   * Returns the index where the specified not-null element is first found. If the element is not found, this returns -1.
   */
  private def indexOfNotNull[T](values: java.lang.Iterable[T], element: T, comparator: Comparator[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (element == null) throw new NullPointerException("element")
      if (comparator == null) throw new NullPointerException("comparator")

      var i = 0
      val it = values.iterator
      while (it.hasNext) {
        val item = it.next
        if (comparator.compare(element, item) == 0)
          return i

        i = i + 1
      }

      return -1
    }

  /**
   * Returns the last index where the first null element is found. If no null elements are found, this returns -1.
   */
  private def lastIndexOfNull[T](values: Traversable[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")

      var lastPos = -1
      var i = 0
      for (item <- values) {
        if (item == null)
          lastPos = i

        i = i + 1
      }

      lastPos
    }

  /**
   * Returns the last index where the first null element is found. If no null elements are found, this returns -1.
   */
  private def lastIndexOfNull[T](values: Array[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")

      var count = values.length
      for (i <- count to 0)
        if (values(i) == null)
          return i

      return -1
    }

  /**
   * Returns the last index where the first null element is found. If no null elements are found, this returns -1.
   */
  private def lastIndexOfNull[T](values: java.lang.Iterable[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")

      var lastPos = -1
      var i = 0
      val it = values.iterator
      while (it.hasNext) {
        val item = it.next
        if (item == null)
          lastPos = i

        i = i + 1
      }

      lastPos
    }

  /**
   * Returns the last index where the specified not-null element is first found. If the element is not found, this returns -1.
   */
  private def lastIndexOfNotNull[T](values: Traversable[T], element: T): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (element == null) throw new NullPointerException("element")

      var lastPos = -1
      var i = 0
      for (item <- values) {
        if (element.equals(item))
          lastPos = i

        i = i + 1
      }

      lastPos
    }

  /**
   * Returns the last index where the specified not-null element is first found. If the element is not found, this returns -1.
   */
  private def lastIndexOfNotNull[T](values: Array[T], element: T): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (element == null) throw new NullPointerException("element")

      var lastPos = -1
      var i = 0
      for (item <- values) {
        if (element.equals(item))
          lastPos = i

        i = i + 1
      }

      lastPos
    }

  /**
   * Returns the last index where the specified not-null element is first found. If the element is not found, this returns -1.
   */
  private def lastIndexOfNotNull[T](values: java.lang.Iterable[T], element: T): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (element == null) throw new NullPointerException("element")

      var lastPos = -1
      var i = 0
      val it = values.iterator
      while (it.hasNext) {
        val item = it.next
        if (element.equals(item))
          lastPos = i

        i = i + 1
      }

      lastPos
    }

  /**
   * Returns the last index where the specified not-null element is first found. If the element is not found, this returns -1.
   */
  private def lastIndexOfNotNull[T](values: Traversable[T], element: T, comparator: Comparator[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (element == null) throw new NullPointerException("element")
      if (comparator == null) throw new NullPointerException("comparator")

      var lastPos = -1
      var i = 0
      for (item <- values) {
        if (comparator.compare(element, item) == 0)
          lastPos = i

        i = i + 1
      }

      lastPos
    }

  /**
   * Returns the last index where the specified not-null element is first found. If the element is not found, this returns -1.
   */
  private def lastIndexOfNotNull[T](values: Array[T], element: T, comparator: Comparator[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (element == null) throw new NullPointerException("element")
      if (comparator == null) throw new NullPointerException("comparator")

      var lastPos = -1
      var i = 0
      for (item <- values) {
        if (comparator.compare(element, item) == 0)
          lastPos = i

        i = i + 1
      }

      lastPos
    }

  /**
   * Returns the last index where the specified not-null element is first found. If the element is not found, this returns -1.
   */
  private def lastIndexOfNotNull[T](values: java.lang.Iterable[T], element: T, comparator: Comparator[T]): Int =
    {
      if (values == null) throw new NullPointerException("values")
      if (element == null) throw new NullPointerException("element")
      if (comparator == null) throw new NullPointerException("comparator")

      var lastPos = -1
      var i = 0
      val it = values.iterator
      while (it.hasNext) {
        val item = it.next
        if (comparator.compare(element, item) == 0)
          lastPos = i

        i = i + 1
      }

      lastPos
    }

  /**
   * Predicate returns true if elements exist in a Traversable
   */
  private def elementsExistInTraversable[T](values: Traversable[T]): Boolean =
    {
      values.size > 0
    }

  /**
   * Predicate returns true if elements exist in a Traversable
   */
  private def elementsExistInList(values: java.util.List[_]): Boolean =
    {
      values.size > 0
    }
}