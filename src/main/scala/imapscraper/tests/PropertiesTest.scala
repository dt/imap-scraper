package imapscraper.tests

object PropertiesTest {
  def main(args : Array[String]) : Unit = {
	  System setProperty("foo", "bar")
	  println( System getProperty "foo" )
	  val props = System getProperties() 	  
	  props setProperty("foo", "baz")
	  println( System getProperty "foo" )
	  println( props getProperty "foo" )
	  
  }
}
