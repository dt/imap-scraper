package imapscraper.filters

import imapscraper.Post

class Study extends KeywordsFilter {
  override val deps = Array[String]("ForSale", "WTB", "Job", "Housing");
  
  val keywords = Array( ci("study"), ci("participant"), ci("mri") );
  
  override def transform(p:Post) = { 
    if(external(p) && ( matchIn(p.title) || matchIn(p.body)) 
    		&& !p.has("sale") && !p.has("wanted") && !p.has("job") && !p.has("housing"))
      p add "study" 
  }
}
