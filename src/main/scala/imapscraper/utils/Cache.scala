 /**
  * This class implements an LRU cache
  * 
  * @author David Taylor
  * @author Daniel LaGrotta
  * Copyright 2010
  * David Taylor
  * Daniel LaGrotta
  * Ashton Thomas
  * Stafford Brunk
  * Ryan Buckheit
  */

package imapscraper.utils

import java.util.{Map => JMap, LinkedHashMap => JLHM}

class Cache[K,E](val max : Int) 
  extends JLHM[K,E](max / 2, 0.75F, true) {
	
  override def removeEldestEntry( e: JMap.Entry[K,E] ) = {
    size > max
  }
  
  def get( key : K ) = {
	val attempt = super.get(key)
    attempt match { 
	  case null => None
	  case _ => Some(attempt)
	}
  }
  
  def putIfNew(key : K, value : E) = {
	  if (!containsKey(key)) { 
	 	put(key, value)
	 	true
	  } else
	    false
  }
  // adapted from scala/trunk/src/library/scala/collection/mutable/MapLike.scala
  def getOrElseUpdate(key: K, op: => E): E = {
    var i = super.get(key)
    if (i==null) {
    	i = op
    	put(key, i)
    }
    i
  }
}
