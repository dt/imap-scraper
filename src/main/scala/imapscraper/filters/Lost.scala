package imapscraper.filters

import imapscraper.Post

class Lost extends KeywordsFilter {
  override val deps = Array[String]("ForSale", "WTB");
  
  val keywords = Array( ci("lost"),  ci("found") );
  
  override def transform(p:Post) = { 
    if( external(p) && matchIn(p.title) && !p.has("sale") && !p.has("wanted"))
      p add "lost" 
  }
}
