package imapscraper.utils

import java.util.Enumeration

object RichTypes {
  implicit def enumerationToRichEnumeration[T](enumeration: Enumeration[T]):RichEnumeration[T] = {
    new RichEnumeration(enumeration)
  }
  
  implicit def BooleanBool(b: Boolean) = Bool(b)

  implicit def iteratorToWrapper[T](iter:java.util.Iterator[T]):IteratorWrapper[T] = new IteratorWrapper[T](iter)

  implicit def arr2str(ba: Array[Byte]) : String = new String(ba) 
}

class IteratorWrapper[A](iter:java.util.Iterator[A])
{
    def foreach(f: A => Unit): Unit = {
        while(iter.hasNext){
          f(iter.next)
        }
    }
}


class RichEnumeration[T](enumeration:Enumeration[T]) extends Iterator[T] {
  def hasNext:Boolean =  enumeration.hasMoreElements()
  def next:T = enumeration.nextElement()
}

case class Bool(b: Boolean) {
  def ?[X](t: => X) = new {
    def |(f: => X) = if(b) t else f
  }
}


