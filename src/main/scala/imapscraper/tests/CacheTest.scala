package imapscraper.tests

import imapscraper.utils.Cache

class CacheTest extends Test {
	 val MAX = 10
	 val cache = new Cache[Int, String](MAX);
	 
	def runTest : Boolean = {
		for( i <- 1 until 3 * MAX) {
			cache.put(i, i.toString);
			if ( cache.size > MAX)
				return false
		}
		true
	}
}