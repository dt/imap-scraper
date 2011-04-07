package imapscraper.tests

import imapscraper._

class PostTest extends Test {
	def runTest : Boolean = {
		val flag = "foobar"

		val p1 = Post.findByAttribute("tagged"){0}
		p1 add flag
		if (!(p1 has flag))
			return false
		p1 commit
		
		val p2 = Post.get(p1 id) get ;
		if (!(p2 has flag))
			return false
		p2 del flag
		if (p2 has flag)
			return false
		p2 commit
		
		val p3 = Post.get(p1 id) get ;
		if (p3 has flag)
			return false
		
		true
	}
}