package imapscraper.filters

import scala.collection.immutable.HashSet
import imapscraper.Post

object Price extends Filter {
  val deps = Array("BuyOrSell", "Ride");
  
  override def transform(p:Post) = { 
    true 
  }
}
