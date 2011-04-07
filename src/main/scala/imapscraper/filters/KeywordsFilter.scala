package imapscraper.filters

import java.util.regex._

abstract class KeywordsFilter extends Filter {
  val keywords : Array[Pattern];
  
  def matchIn(field : String) : Boolean = {
    keywords.find(k => k.matcher(field).find).isDefined
  }
  
  def matchPatternsIn(patterns : Array[Pattern], field : String) : Boolean = {
    patterns.find(k => k.matcher(field).find).isDefined
  }
  
  def ci = Pattern.compile(_:String, Pattern.CASE_INSENSITIVE);
  
}
