package imapscraper.filters

import imapscraper.Post

class Housing extends KeywordsFilter {
  override val deps = Array[String]("ForSale", "WTB", "Lost");
  
  val keywords = Array( ci("roommate"), ci("sublet"), 
                        ci("landlord"), ci("apartment") );
  
  override def transform(p:Post) = { 
    if( external(p) &&  ( matchIn(p.title) || matchIn(p.body)) 
    		&& !p.has("sale") && !p.has("wanted") && !p.has("lost") )
      p add "housing" 
  }
}
