package imapscraper.filters

import imapscraper.Post

class Wanted extends KeywordsFilter {
  override val deps = Array[String]("Ride", "ForSale", "Study", "Housing", "Job");
  
  val keywords = Array( ci("Wanted"), ci("looking for"))
  
  override def transform(p:Post) = {
    if( external(p) && ( matchIn(p.title) || matchIn(p.body))
     && !p.has("ride") && !p.has("sale") && !p.has("study") && !p.has("housing") & !p.has("job"))
      p add "wanted"
  }
}
