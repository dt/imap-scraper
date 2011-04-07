package imapscraper.tests

trait Test {
	def name() = {
		(""+getClass).stripPrefix("class imapscraper.tests.")
	}
	def runTest() : Boolean
}