package imapscraper.filters

import imapscraper.Post

class Job extends KeywordsFilter {
  override val deps = Array[String]("ForSale", "WTB", "Housing");
  
  val keywords = Array( ci("part-time"), ci("part time"), ci("intern"), ci("full time"), ci("hiring") )

  val titleKeywords = keywords ++ Array( ci("intern"), ci("position") )
  
  override def transform(p:Post) = { 
    if(external(p) && ( matchPatternsIn(titleKeywords, p.title) || matchIn(p.body)) &&
    		!p.has("sale") && !p.has("wanted") && !p.has("housing") )
      p add "job" 
  }
}
  
