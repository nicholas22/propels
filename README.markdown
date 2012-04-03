## Hi there!

PropelS is a Scala library porting features of [JPropel](http://github.com/nicholas22/jpropel).

- LINQ for Scala, with Scala semantics (such as functions, implicits, etc)
- Conversions for easily working between Java and Scala collections
- Free and open-source, can be used for commercial purposes (LGPL license)



## LINQ

A very popular language feature of C#, Language INtegrated Query adds native data querying capabilities. The Scala collections library supports most of these features, but you may not 
have the time to learn an entirely new API. Using PropelS you can continue using an API you already know. Also, having a [Java port of LINQ](https://github.com/nicholas22/jpropel-light)
you may re-use your experience across three languages (C#, Java, Scala).

    // declaration
    val buff = Buffer("john", "james", "eddie")

    // transform & filter
    val upper = Linq.select(buff, (x : String)=> StringUtils.titleCase(x) )
    val jays = Linq.where(upper, (x : String) => x.startsWith("J"))

    // output: "John, James"
    println(StringUtils.delimit(Linq.toArray(jays), ", "))



## Java <--> Scala collections bridging

There are a lot moving parts in Scala collections and having them changed a few times did not help many developers.
Using PropelS (this project!) you can easily convert between Java/Scala collections:


#### toArray(...)

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

#### toBuffer(...)

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

#### toSeq(...)

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

#### toArrayList(...)

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



## Free and Open

You may use this project in open and closed source projects. The LGPL license simply requests any changes made to this library alone, to be contributed back. Help us evolve!



## Dependencies

As PropelS is in its early stages, it currently depends on the latest [JPropel-light library](https://github.com/nicholas22/jpropel-light) because it requires certain un-ported yet classes.
There are plans to port certain things over to Scala (especially if they are using functional features), which are currently handled using [lombok-pg](https://github.com/peichhorn/lombok-pg). 



## Changelog

#### 0.2: Almost all functional classes ported
ArrayUtils ported
LINQ bugfixes  


#### 0.1: Creation
All LINQ methods have been ported.  
Added extra methods: partition, unzip (these will be backported to jpropel)  

