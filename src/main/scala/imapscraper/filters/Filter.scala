package imapscraper.filters

import imapscraper.{Post,Source}

abstract class Filter {
  val deps : Array[String]
  
  def ready( done : Set[String], post : Post) : Boolean = {
    deps.forall(done.contains _)
  }

  def transform(p : Post) : Unit ;

  def external(p: Post) : Boolean = {
    p.src != Source.SELF
  }
}
