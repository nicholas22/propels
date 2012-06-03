package propels.core.utils

import java.util.Arrays
import java.util.ArrayList
import java.util.Comparator
import propel.core.common.CONSTANT
import propel.core.collections.lists.ReifiedArrayList
import propel.core.collections.lists.SortedList
import propel.core.collections.lists.ReifiedLinkedList
import propel.core.collections.lists.ReifiedList
import propel.core.utils.PrimitiveArrayType
import propel.core.utils.StringUtils
import propel.core.utils.ReflectionUtils
import scala.collection.mutable.Buffer

object ArrayUtils {

  /**
   * Adds an element to the given array
   *
   * @throws NullPointerException Array is null.
   */
  def add[T](array: Array[T], element: T): Array[T] =
    {
      if (array == null) throw new NullPointerException("array")

      val result = create[T](array.getClass.getComponentType, array.size + 1)
      System.arraycopy(array, 0, result, 0, array.length)
      result(array.length) = element
      result
    }

  /* REMOVED: box() */

  /**
   * Clears an array with the default Type T value, i.e. null.
   *
   * @throws NullPointerException Array is null.
   */
  def clear[T](array: Array[T]): Unit =
    {
      val default: T = Linq.defaultValue
      clear(array, default)
    }

  /**
   * Clears an array with the provided value
   *
   * @throws NullPointerException Array is null.
   */
  def clear[T](array: Array[T], element: T): Unit =
    {
      if (array == null) throw new NullPointerException("array")

      // set to default value
      for (i <- 0 to array.length)
        array(i) = element
    }

  /**
   * Clones the given array
   *
   * @throws NullPointerException Array is null.
   */
  def clone[T](array: Array[T]): Array[T] =
    {
      if (array == null) throw new NullPointerException("array")

      val result = create[T](array.getClass.getComponentType, array.length)
      for (i <- 0 to array.length)
        result(i) = array(i)

      result
    }

  /**
   * Returns the length of any array (primitive or object array). This works for one-dimensional arrays.
   *
   * @throws NullPointerException An argument is null.
   * @throws IllegalArgumentException The argument is not a primitive or object array
   */
  def count(array: AnyRef): Int =
    {
      if (array == null) throw new NullPointerException("array")

      // get the array type
      val arrayType = getType(array)

      arrayType match {
        case PrimitiveArrayType.NotPrimitive => array.asInstanceOf[Array[_]].length
        case PrimitiveArrayType.Char => array.asInstanceOf[Array[Char]].length
        case PrimitiveArrayType.Short => array.asInstanceOf[Array[Short]].length
        case PrimitiveArrayType.Int => array.asInstanceOf[Array[Int]].length
        case PrimitiveArrayType.Long => array.asInstanceOf[Array[Long]].length
        case PrimitiveArrayType.Float => array.asInstanceOf[Array[Float]].length
        case PrimitiveArrayType.Double => array.asInstanceOf[Array[Double]].length
        case PrimitiveArrayType.Boolean => array.asInstanceOf[Array[Boolean]].length
        case PrimitiveArrayType.Byte => array.asInstanceOf[Array[Byte]].length
        case _ => throw new IllegalArgumentException("Unrecognized array type: " + arrayType)
      }
    }

  /**
   * Creates a generic array
   *
   * @throws NullPointerException An argument is null
   * @throws IllegalArgumentException If componentType is {@link Void#TYPE}
   * @throws ClassCastException An invalid type parameter was specified
   * @throws NegativeArraySizeException If the specified size is negative
   */
  def create[T](componentType: Class[_], size: Int): Array[T] =
    {
      java.lang.reflect.Array.newInstance(componentType, size).asInstanceOf[Array[T]]
    }

  /**
   * Creates a 1-dimensional array populated with the specified element in all places
   *
   * @throws NullPointerException Value is null.
   * @throws IllegalArgumentException Value is null, or length is out of range.
   * @throws OutOfMemoryError No enough memory to allocate array
   */
  def create[T](value: T, length: Int): Array[T] =
    {
      if (value == null) throw new NullPointerException("value")
      if (length < 0) throw new IllegalArgumentException("length=" + length)

      val result: Array[T] = create(value.getClass(), length)
      for (i <- 0 to length)
        result(i) = value

      result
    }

  /**
   * Creates a 2-dimensional array populated with the specified element's reference in all places
   *
   * @throws NullPointerException The value is null
   * @throws IllegalArgumentException A dimension is out of range.
   * @throws OutOfMemoryError No enough memory to allocate array
   */
  def create[T](value: T, lengthX: Int, lengthY: Int): Array[Array[T]] =
    {
      if (value == null) throw new NullPointerException("value")
      if (lengthX < 0) throw new IllegalArgumentException("lengthX=" + lengthX)
      if (lengthY < 0) throw new IllegalArgumentException("lengthY=" + lengthY)

      val valueType = value.getClass()
      val array: Array[T] = create(valueType, 0)
      val result: Array[Array[T]] = create(array.getClass(), lengthX)

      for (x <- 0 to lengthX) {
        result(x) = create(valueType, lengthY)
        for (y <- 0 to lengthY)
          result(x)(y) = value
      }

      result
    }

  /**
   * Clones the given array, by invoking clone on all non-null elements of the array. The elements are responsible for performing a
   * successful clone operation of themselves. If the element cloning operation faults, an IllegalArgumentException will be thrown,
   * encapsulating the underlying exception.
   *
   * @throws IllegalArgumentException The underlying clone() method is inaccessible due to Java access control, or an invocation error
   *           occurred
   * @throws NullPointerException Array is null.
   */
  def deepClone[T](array: Array[T]): Array[T] =
    {
      if (array == null) throw new NullPointerException("array")

      try {
        val result: Array[T] = create(array.getClass().getComponentType(), array.length)

        for (i <- 0 to array.length)
          if (array(i) != null) {
            val method = ReflectionUtils.getMethod(array(i).getClass(), "clone", true)
            val item = method.invoke(array(i), null.asInstanceOf[Array[Object]])
            result(i) = item.asInstanceOf[T]
          }

        result
      } catch {
        case e: Exception => throw new IllegalArgumentException(e)
      }
    }

  /**
   * Returns true if the provided object refers to a primitive array, e.g. int[]. This returns false for object arrays.
   *
   * @throws NullPointerException An argument is null
   * @throws IllegalArgumentException The argument is not a primitive or object array
   */
  def getType(array: Any): PrimitiveArrayType =
    {
      if (array == null) throw new NullPointerException("array")

      var className = array.getClass().getName()
      if (className.charAt(0) != CONSTANT.BRACKET_OPEN_CHAR)
        throw new IllegalArgumentException("The provided object is not an array: " + array.getClass())

      // trim [ from class name
      className = StringUtils.trimStart(className, Array(CONSTANT.BRACKET_OPEN_CHAR))

      if (className.length() < 1)
        throw new IllegalArgumentException("The provided object is not an array: " + array.getClass())

      className.charAt(0) match {
        case 'C' => PrimitiveArrayType.Char
        case 'I' => PrimitiveArrayType.Int
        case 'J' => PrimitiveArrayType.Long
        case 'S' => PrimitiveArrayType.Short
        case 'B' => PrimitiveArrayType.Byte
        case 'F' => PrimitiveArrayType.Float
        case 'D' => PrimitiveArrayType.Double
        case 'Z' => PrimitiveArrayType.Boolean
        case _ => PrimitiveArrayType.NotPrimitive
      }
    }

  /**
   * Returns the dimensions of the provided object. The array can be a primitive array, e.g. int[] or an object array e.g. Integer[]
   *
   * @throws NullPointerException An argument is null
   * @throws IllegalArgumentException The argument is not a primitive or object array
   */
  def getDimensions(array: Any): Int =
    {
      if (array == null)
        throw new NullPointerException("array")

      var className = array.getClass().getName()
      if (className.charAt(0) != CONSTANT.BRACKET_OPEN_CHAR)
        throw new IllegalArgumentException("The provided object is not an array: " + array.getClass())

      // trim [ from class name
      var result = 0
      while (className.startsWith(CONSTANT.BRACKET_OPEN)) {
        result = result + 1
        className = className.substring(1)
      }

      result
    }

  /**
   * Joins two arrays in the order they were provided.
   *
   * @throws NullPointerException Array collection is null
   * @throws IllegalStateException Component type cannot be determined.
   */
  def join[T](first: Array[T], second: Array[T]): Array[T] =
    {
      if (first == null) throw new NullPointerException("first")
      if (second == null) throw new NullPointerException("second")

      if (second.length == 0)
        first
      else if (first.length == 0)
        second
      else {
        val firstLen = first.length
        val secondLen = second.length
        val totalLen = firstLen + secondLen
        val result = resize(first, totalLen)

        System.arraycopy(second, 0, result, firstLen, secondLen)

        result
      }
    }

  /**
   * Joins two or more arrays in the order they were provided. Null arrays are ignored.
   *
   * @throws NullPointerException Array collection is null
   * @throws IllegalStateException Component type cannot be determined.
   */
  def join[T](arrays: Traversable[Array[T]]): Array[T] =
    {
      if (arrays == null) throw new NullPointerException("arrays")

      var componentType: Class[_] = null

      // determine full length
      var length = 0
      for (array <- arrays)
        if (array != null) {
          length += array.length
          // determine array component type
          if (componentType == null)
            componentType = arrays.getClass().getComponentType()
        }

      if (componentType == null)
        throw new IllegalStateException("Cannot determine component type from array collection.")

      // create big array
      val result: Array[T] = create(componentType, length)

      // copy all elements over
      var index = 0
      for (array <- arrays)
        if (array != null)
          for (item <- array) {
            result(index) = item
            index = index + 1
          }

      result
    }

  /**
   * Adds an element to the given array, at position 0
   *
   * @throws NullPointerException Array is null.
   */
  def prepend[T](array: Array[T], element: T): Array[T] =
    {
      if (array == null) throw new NullPointerException("array")

      val result: Array[T] = create(array.getClass().getComponentType(), array.length + 1)
      System.arraycopy(array, 0, result, 1, array.length)
      result(0) = element
      result
    }

  /**
   * Removes the first encounter of element from the given array, returning an new array if found. Otherwise returns a copy of the array
   * containing all original items.
   *
   * @throws NullPointerException Array is null.
   */
  def remove[T](array: Array[T], element: T): Array[T] =
    {
      if (array == null) throw new NullPointerException("array")

      val index = Linq.indexOf[T](array, element)
      if (index < 0)
        array

      remove(array, index)
    }

  /**
   * Removes all occurences of element from the given array, returning an new array if found. Otherwise returns a copy of the array
   * containing all original items.
   *
   * @throws NullPointerException Array is null.
   */
  def removeAll[T](array: Array[T], element: T): Array[T] =
    {
      if (array == null) throw new NullPointerException("array")

      var result = array
      while (array.length > 0 && Linq.indexOf(array, element) >= 0)
        result = remove(result, element)

      result
    }

  /**
   * Removes the element at the given position from the given array.
   *
   * @throws NullPointerException Array is null
   * @throws IndexOutOfBoundsException Index is out of range
   */
  def remove[T](array: Array[T], index: Int): Array[T] =
    {
      if (array == null) throw new NullPointerException("array")
      if (index < 0 || index >= array.length) throw new IndexOutOfBoundsException("index=" + index + " length=" + array.length)

      val result = create[T](array.getClass.getComponentType, array.length - 1)

      if (index > 0)
        System.arraycopy(array, 0, result, 0, index)
      if (index != array.length - 1)
        System.arraycopy(array, index + 1, result, index, array.length - index - 1)

      result
    }

  /**
   * Re-sizes an array to the specified size. If the new size is smaller, elements get truncated. If the new size if bigger, the new array
   * will have several null-valued elements near its end (i.e. newSize-oldSize in count). If the sizes are equal, the same array is
   * returned.
   *
   * @throws NullPointerException Array is null
   * @throws IllegalArgumentException Length is out of range
   */
  def resize[T](array: Array[T], length: Int): Array[T] =
    {
      if (array == null) throw new NullPointerException("array")
      if (length < 0) throw new IllegalArgumentException("length=" + length)

      val oldLength = array.length

      // check if the sizes match
      if (length == oldLength)
        array
      else {
        // create new array
        val newArray: Array[T] = create(array.getClass().getComponentType(), length)

        // select strategy based on given scenario
        if (length > oldLength)
          // newer is larger, use old size as upper bound
          System.arraycopy(array, 0, newArray, 0, oldLength)
        else
          // newer is smaller, use new size as upper bound
          System.arraycopy(array, 0, newArray, 0, length)

        newArray
      }
    }

  /**
   * Reverses an array (in place)
   *
   * @throws NullPointerException Array is null.
   */
  def reverse[T](array: Array[T]): Array[T] =
    {
      if (array == null) throw new NullPointerException("array")

      if (array.length > 1) {
        var left = 0 // index of leftmost element
        var right = array.length - 1 // index of rightmost element

        while (left < right) {
          // exchange the left and right elements
          val temp = array(left)
          array(left) = array(right)
          array(right) = temp

          // move the bounds toward the center
          left += 1
          right -= 1
        }
      }

      array
    }

  /**
   * Reverses an array (creates a new copy)
   *
   * @throws NullPointerException Array is null.
   */
  def reverseCopy[T](array: Array[T]): Array[T] =
    {
      if (array == null) throw new NullPointerException("array")

      val length = array.length
      val result: Array[T] = create(array.getClass().getComponentType(), length)

      for (i <- 0 to length)
        result(i) = array(length - 1 - i);

      result
    }

  /**
   * Returns a sub-array of the given array
   *
   * @throws NullPointerException Array is null.
   * @throws IndexOutOfBoundsException Start index / end index are out of range.
   */
  def subArray[T](array: Array[T], startIndex: Int, endIndex: Int): Array[T] =
    {
      if (array == null) throw new NullPointerException("array")

      if (startIndex < 0 || startIndex > endIndex) throw new IndexOutOfBoundsException("startIndex=" + startIndex + " endIndex=" + endIndex)
      if (endIndex > array.length) throw new IndexOutOfBoundsException("endIndex=" + endIndex + " length=" + array.length)

      val result: Array[T] = create(array.getClass().getComponentType(), endIndex - startIndex)
      System.arraycopy(array, startIndex, result, 0, endIndex - startIndex)

      result
    }

  /**
   * Converts an array to a list.
   *
   * @throws NullPointerException The array is null.
   */
  def toArrayList[T](array: Array[T]): ArrayList[T] =
    {
      if (array == null) throw new NullPointerException("array")

      val result = new ArrayList[T](array.length)
      for (item <- array)
        result.add(item)

      result
    }

  /**
   * Puts all elements from an array to a buffer
   *
   * @throws NullPointerException An argument is null.
   */
  def toBuffer[T](from: Array[T], to: Buffer[T]): Unit =
    {
      if (from == null) throw new NullPointerException("from")
      if (to == null) throw new NullPointerException("to")

      for (item <- from)
        to += item
    }

  /**
   * Puts all elements from an array to a collection
   *
   * @throws NullPointerException An argument is null.
   */
  def toCollection[T](from: Array[T], to: java.util.Collection[T]): Unit =
    {
      if (from == null) throw new NullPointerException("from")
      if (to == null) throw new NullPointerException("to")

      for (item <- from)
        to.add(item)
    }

  /**
   * Converts an array to a list.
   *
   * @throws NullPointerException The array is null.
   */
  def toReifiedList[T <: AnyRef](array: Array[T]): ReifiedList[T] =
    {
      if (array == null) throw new NullPointerException("array")
      new ReifiedArrayList[T](array)
    }

  /**
   * Returns the given array as a PriorityQueue. Elements are sorted based on their Comparable implementation.
   *
   * @throws NullPointerException The array is null.
   */
  def toSortedList[T <: Comparable[T]](array: Array[T]): SortedList[T] =
    {
      if (array == null) throw new NullPointerException("array")

      val result = new SortedList[T](array.getClass().getComponentType())
      for (element <- array)
        result.add(element)

      result
    }

  /**
   * Returns the given array as a PriorityQueue. Elements are sorted based on their Comparable implementation.
   *
   * @throws NullPointerException The array is null.
   */
  def toSortedList[T <: Comparable[T]](array: Array[T], comparison: ((T, T) => Int)): SortedList[T] =
    {
      if (array == null) throw new NullPointerException("array")

      var result: SortedList[T] = null

      if (comparison == null)
        result = new SortedList[T](array.getClass().getComponentType())
      else
        result = new SortedList[T](ComparatorUtils.toComparator(comparison), array.getClass().getComponentType())

      for (element <- array)
        result.add(element)

      result
    }

  /**
   * Returns the given array as a LinkedList (a class operating as a Queue or Stack).
   *
   * @throws NullPointerException The array is null.
   */
  def toLinkedList[T <: AnyRef](array: Array[T]): ReifiedLinkedList[T] =
    {
      if (array == null) throw new NullPointerException("array")

      val result = new ReifiedLinkedList[T](array)

      for (item <- array)
        result.addLast(item)

      result
    }

  /* REMOVED: unbox() */

}