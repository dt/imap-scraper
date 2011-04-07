package imapscraper.tests

import imapscraper._
import javax.mail._
import internet._

object PlainText {
	
	def main(args : Array[String]) : Unit = {
		val m:Message = new MimeMessage(Session getDefaultInstance(System.getProperties));
		// Create the message parts 
		val part1 = new MimeBodyPart();
		val part2 = new MimeBodyPart();
			
		part1.setContent("Hello World <br />", "text/html");
		part2.setContent("Hello World", "text/plain");

		val multipart = new MimeMultipart();
		multipart.addBodyPart(part1);
		multipart.addBodyPart(part2);

		// Put parts in message
		m.setContent(multipart, "multipart/mixed");
		
		
		rMsgWalk(m)
		
	}
	
	def rMsgWalk (p : Part) : Unit = {
		println("type:" + p.getContentType);
		println("disp:" + p.getDisposition);
		
		if ( p.isMimeType("multipart/*") ) {
			val mp = p.getContent().asInstanceOf[Multipart];
			for (i <- 0 until  mp.getCount()) {
				rMsgWalk(mp.getBodyPart(i));
			}	
		}
	}
}