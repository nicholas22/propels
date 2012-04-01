import propels.core.utils.Linq
import propel.core.utils.StringUtils
import scala.collection.mutable.Buffer

object Main {

  def main(args: Array[String]): Unit = { 

    // LINQ sample
    linqSample()    
    
    // collection conversion samples
    toArray()
    toBuffer()
    toSeq()
    toArrayList()
    
  }
  
  def linqSample() {
    val buff = Buffer("john", "james", "eddie")
    val upper = Linq.select(buff, (x : String)=> StringUtils.titleCase(x) )
    val jays = Linq.where(upper, (x : String) => x.startsWith("J"))
    println(StringUtils.delimit(Linq.toArray(jays), ", "))
  }
   
  /**
   * This example indicates how to convert various collection types to Scala Array types.
   */
  def toArray() {
    // Scala Traversable -> Array conversion
    val buff = Buffer("a", "b", "c", "d").asInstanceOf[Traversable[String]]
    Linq.toArray(buff)
    
    // JDK ArrayList -> Array conversion
    val list = new java.util.ArrayList[String]()
    Linq.toArray(list)

    // JDK Iterable -> Array conversion
    val iterable = new java.util.ArrayList[String]().asInstanceOf[java.lang.Iterable[String]]
    Linq.toArray(iterable)
    
    // JDK Enumeration -> Array conversion
    val enum = new java.util.StringTokenizer("test")  // note: implements Enumeration[Object]
    Linq.toArray(enum)
  }
  
  /**
   * This example indicates how to convert various collection types to Scala Buffer types. 
   * These resemble Java's ArrayLists, in that they are mutable and indexed access is allowed.
   */
  def toBuffer() {
    // Array -> Scala mutable Buffer
    val arr = Array("a", "b", "c", "d")
    Linq.toBuffer(arr)
    
    // Scala Traversable -> Scala mutable Buffer
    val seq = Buffer("a", "b", "c", "d").asInstanceOf[Traversable[String]]
    Linq.toBuffer(seq)    
    
    // JDK ArrayList -> Scala mutable Buffer
    val list = new java.util.ArrayList[String]()
    Linq.toBuffer(list)
    
    // JDK Iterable -> Scala mutable Buffer
    val iterable = new java.util.ArrayList[String]().asInstanceOf[java.lang.Iterable[String]]
    Linq.toBuffer(iterable)
    
    // JDK Enumeration -> Scala mutable Buffer
    val enum = new java.util.StringTokenizer("test")  // note: implements Enumeration[Object]
    Linq.toBuffer(enum)
  }
  
  /**
   * This example indicates how to convert various collection types to Scala IndexedSeq types. 
   * These resemble Java's ArrayLists, but are immutable. Indexed access is allowed.
   */
  def toSeq() {
	// Array -> Scala immutable IndexedSeq
    val arr = Array("a", "b", "c", "d")
    Linq.toSeq(arr)
    
    // Scala Traversable -> Scala immutable IndexedSeq
    val seq = Buffer("a", "b", "c", "d").asInstanceOf[Traversable[String]]
    Linq.toSeq(seq)    
    
    // JDK ArrayList -> Scala immutable IndexedSeq
    val list = new java.util.ArrayList[String]()
    Linq.toSeq(list)
    
    // JDK Iterable -> Scala immutable IndexedSeq
    val iterable = new java.util.ArrayList[String]().asInstanceOf[java.lang.Iterable[String]]
    Linq.toSeq(iterable)
    
    // JDK Enumeration -> Scala immutable IndexedSeq
    val enum = new java.util.StringTokenizer("test")  // note: implements Enumeration[Object]
    Linq.toSeq(enum)
  }
  
  /**
   * This example indicates how to convert various collection types to Java ArrayList types.
   */
  def toArrayList() {
	// Array -> JDK ArrayList
    val arr = Array("a", "b", "c", "d")
    Linq.toArrayList(arr)
    
    // Scala Traversable -> JDK ArrayList
    val seq = Buffer("a", "b", "c", "d").asInstanceOf[Traversable[String]]
    Linq.toArrayList(seq)    
    
    // JDK Iterable -> JDK ArrayList
    val iterable = new java.util.ArrayList[String]().asInstanceOf[java.lang.Iterable[String]]
    Linq.toArrayList(iterable)
    
    // JDK Enumeration -> JDK ArrayList
    val enum = new java.util.StringTokenizer("test")  // note: implements Enumeration[Object]
    Linq.toArrayList(enum)
  }

}