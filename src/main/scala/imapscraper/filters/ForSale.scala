package imapscraper.filters

import imapscraper.Post

class ForSale extends KeywordsFilter {
  val deps = Array[String]("WTB");
  
  val keywords = Array(ci("fs"), ci("selling"), ci("for sale"), ci("WTS"), ci("asking \\$") )
  val titleKeywords = keywords ++ Array(ci("sale"), ci("obo") )
  
  
  override def transform(p:Post) = {
    if(external(p) && (matchPatternsIn(titleKeywords, p.title) || matchIn(p.body)) && !p.has("wanted") )
      p add "sale"
 
  }
}
