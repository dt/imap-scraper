 /**
  * This class stores runs the filters on the set of imported posts
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

package imapscraper

import filters._
import scala.collection._

object Processor extends log.Logging {
  val VERSION = "1.3"
  val TAG_NAME = "tagged"
  val prefix = "imapscraper.filters."
  val filterNames = immutable.HashSet[String](
		  "ForSale", "WTB", "Lost",  "Housing", "Ride", "Study", "Job", "Wanted" );

  val filterClasses = new mutable.HashMap[String, Class[Filter]]
  for (name <- filterNames)
  	filterClasses.put(name,(Class.forName(prefix+name).asInstanceOf[Class[Filter]]))
  
  def processNew = {
    var posts = Post.findWithoutAttribute(Processor.TAG_NAME)
    posts ++= Post.findByAttributeValue(Processor.TAG_NAME, "value < ?", Processor.VERSION)
    
    for (post <- posts) {
      debug("categorizing "+post.title)
      new Processor(post).run
    }

  }
  
}

class Processor(post:Post) extends log.Logging {
  val filters = Processor.filterClasses.mapValues(_.newInstance())
  
  var to_run : immutable.Set[String] = Processor.filterNames
  var done : immutable.Set[String] = new immutable.HashSet[String];
  
  /**
	 * This method runs all of the filters on newly imported posts
	 */
  def run() {
	  var progress = true
  
	  while( (!to_run.isEmpty) && progress) {
	    progress = false
	    for(i <- to_run) {
	      val filter = filters.get(i).get
	      if(filter.ready(done, post)) {
	        filter.transform(post)
	        to_run = to_run - i 
	        done = done + i
	        progress = true
	      }
	    }
	    if (!progress)
	    	error("Dependency Error!")
	  }
	  post.set(Processor.TAG_NAME, Processor.VERSION)
	  post commit
   }
}
