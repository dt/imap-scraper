package imapscraper.tests
import imapscraper.log.Logging
import java.util.ArrayList

object TestRunner extends Logging
{
	var tests = new ArrayList[Test]
	
	def main( args : Array[String]) = {
		registerTest(new LogTest)
		registerTest(new CacheTest)
		registerTest(new PostTest)
		
		info("Starting TestRunner.")
		for (i <- 0 until tests.size)
		{
			val test = tests.get(i)
			
			if (test.runTest())
			{
				info(test.name + " passed.")
			}
			else
			{
				info(test.name + " failed.")
				info("Stopping TestRunner.")
				System.exit(0)
			}
		}
		info("All tests passed.")
	}
	
	def registerTest ( test : Test ) = {
		tests.add(test)
	}
}