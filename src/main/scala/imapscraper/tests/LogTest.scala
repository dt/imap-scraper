package imapscraper.tests
import java.io.File

import imapscraper.log.Logging

class LogTest extends Test with Logging {
	def runTest : Boolean = {
		debug("LogTest... Debug")
		info("LogTest... Info")
		warn("LogTest... Warn")
		error("LogTest... Error")
		fatal("LogTest... Fatal")
		true
	}
}