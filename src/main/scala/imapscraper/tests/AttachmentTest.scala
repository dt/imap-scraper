package imapscraper.tests
import java.io.File ;
import javax.mail._ ;import javax.mail.internet._ ;import imapscraper._ ;import imapscraper.utils._ ;import imapscraper.importers._
object AttachmentTest {	def main(argv : Array[String]) : Unit = {		DB empty;		MiscMarket.openAndDo(MiscMarket.folderName)({ folder =>      val msgs = folder.getMessages();      println(MiscMarket.importer(msgs{3273}));        	});  }
}