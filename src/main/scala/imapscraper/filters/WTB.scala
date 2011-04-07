package imapscraper.filters

import imapscraper.Post

class WTB extends KeywordsFilter {
  override val deps = Array[String]();
  
  val keywords = Array( ci("WTB") )
  
  override def transform(p:Post) = {
    if( external(p) && ( matchIn(p.title) || matchIn(p.body)) )
      p add "wanted"
    
  }
}
