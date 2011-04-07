package imapscraper.filters

import imapscraper.Post

class Ride extends KeywordsFilter {
  override val deps = Array[String]("ForSale", "WTB", "Housing");
  
  val keywords = Array( ci("ride"), ci("driving to"), 
                        ci("driving up to"), ci("driving down to") );
  
  override def transform(p:Post) = { 
    if(external(p) && ( matchIn(p.title) || matchIn(p.body)) && !p.has("sale") && !p.has("housing") && !p.has("wanted"))
      p add "ride" 
  }
}
