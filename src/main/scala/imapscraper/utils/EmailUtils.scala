 /**
  * This class contains methods for working with emails
  * 
  * @author David Taylor
  * @author Daniel LaGrotta
  * Copyright 2010
  * David Taylor
  * Daniel LaGrotta
  * Ashton Thomas
  * Stafford Brunk
  * Ryan Buckheit
  */

package imapscraper.utils

import imapscraper._
import javax.mail._
import javax.mail.internet._
import java.util.Enumeration
import RichTypes._

object EmailUtils {
  def username(addr : String) : String = {
    addr.split("@"){0}
  }
  def host(addr : String) : String = {
    addr.split("@"){1}
  }
   
  def headersOf(msg : Message) : Enumeration[Header] = {
    (msg getAllHeaders).asInstanceOf[Enumeration[Header]]
  }
  
  def id (msg : Message) = (msg getHeader "Message-ID"){0}
  def replyid (msg : Message) : Option[String] = {
	if (isReply(msg)) 
	  Some( msg.getHeader("In-Reply-To"){0} )
    else None
  }
  
  def isReply(msg : Message) = ( (msg getHeader "In-Reply-To") != null)
  
  def hdr2s( h: Header ) = (h getName) +" : "+ (h getValue) + "\n"
  def hdrs2s( m: Message) = headersOf(m).foldLeft ("") ((s , h) => s + hdr2s(h))
  
  def plainTextify(msg : Message) : Option[String] = {
    val contentTypes = MimeUtils.findMimeTypes( msg, "text/html", "text/plain" );
    if( contentTypes.containsKey( "text/plain" ) ) {
      Some(contentTypes.get( "text/plain" ).getContent().toString())
    } else if( contentTypes.containsKey( "text/html" ) ) {
      Some(contentTypes.get( "text/html" ).getContent().toString().replaceAll("<.*?>","") )
    } else {
      None
    }
  }

  def getAttachments[T]( msg : Message, f : (Int,MimeBodyPart) => Option[T] ) : List[T] = {
    var attachments : List[T] = Nil
    val c = msg.getContent()
    if( c.isInstanceOf [Multipart] ) {
      val mp = c.asInstanceOf[Multipart]
	  for (i <- 0 until mp.getCount()) {
	    val bp = mp.getBodyPart(i)
	     if(Part.ATTACHMENT.equalsIgnoreCase(bp.getDisposition()) && bp.isInstanceOf[MimeBodyPart]) {
	       (f(i,bp.asInstanceOf[MimeBodyPart])) match { 
	         case Some(result) => attachments = result :: attachments
	         case None => //don't care
	      }
	    }
      }
    }
	attachments
  }
  
  def name(addr:Address) : Option[imapscraper.Name] = {
    val guess = { addr match {
      case email:InternetAddress => email getPersonal()
      case _ => return Some( imapscraper.Name((addr toString()), 1) )
    } }
    guess match {
      case null => Some(imapscraper.Name( EmailUtils.username(addr toString()), 0))
      case _ =>  Some(imapscraper.Name(guess, 3))
    }
  }

  def address(addr:Address) : (String) = {
    addr match {
      case email:InternetAddress => email getAddress()
      case _ => addr toString()
    }
  }

}
