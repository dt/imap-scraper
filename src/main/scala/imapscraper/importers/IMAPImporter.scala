 /**
  * This class contains many helpful routines for importing content 
  * stored in IMAP folders. To import from a specific folder, the 
  * IMAPImporter must be extended to provide the connection details 
  * as well as some callback functions.
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

package imapscraper.importers

import imapscraper._
import utils.{MimeUtils, EmailUtils}
import java.net.UnknownHostException
import javax.mail._
import javax.mail.internet._
import java.util.Enumeration

  sealed trait Imported
  case object Success extends Imported
  case object Seen extends Imported
  case object Wait extends Imported
  case object Fail extends Imported

 

trait IMAPImporter extends log.Logging {
  def getStore : Store
  val user : String
  val host : String
  val pass : String
  val messagesPerScrape = 2500
  val skipLimit = 5

  var forNextTime : List[(Int,Message)]= Nil
  
  /**
   * Create a set of default properties for a specified host
   * 
   * @param host Host to use
   * @param ssl  Whether or not to use ssl
   * @return     Set of properties
   */
  def defaultProperties(host : String, ssl: Boolean) = {
	val props = new java.util.Properties()
    props.setProperty("mail.imap.host", host);
    props.setProperty("mail.imap.auth.login.disable", "true");
    if(ssl) {
    	props.setProperty("mail.imap.ssl.enable", "true");
    	props.setProperty("mail.imap.port", "993");
    } else {
    	props.setProperty("mail.imap.ssl.enable", "false");
    	props.setProperty("mail.imap.port", "143");    	
    }
    props.setProperty("mail.imap.connectiontimeout", "5000");
    props.setProperty("mail.imap.timeout", "5000");
    
    props
  }
  
  def connectAndDo ( f: Store => Unit ) = {
	val store = getStore
	try {
      store.connect(host, user, pass)
      try {
    	  f(store)
      } catch {
    	  case (e) => error(e, "IMAP Task Error")
      }
	} catch {
		case (e:UnknownHostException) => error(e, "Could not connect")
    } finally {
    	debug("closing store")
    	store.close
    }
  }
  
  def openAndDo ( folderName : String) (f : Folder => Unit) = {
    connectAndDo { s =>
      val folder = s.getFolder(folderName)
	  try {
	    folder open(Folder.READ_ONLY)
	    f(folder)
	  } catch {
	 	  case (e:FolderClosedException) => error(e, "IMAP Error")
	  } finally {
	 	debug("closing folder")
	 	folder close(false)
	  }
    }
  }
 
  /**
   * The processor function will be invoked on each message, beginning at the (recent) end of
   * the folder. It will attempt to have `count` messages successfully processed
   * as reported by the processor function.  The processor function is given a single
   * message and should return true if the message was useful, false otherwise. For example
   * returning false if the message is read.
   * 
   * @param folder          The folder to process messages in
   * @param count           The number of messages to process
   * @param skipLimit       The number of messages to skip before stopping
   * @param simpleProcessor The method to call on each message
   */
  def processMessages(
		  folder : Folder, count : Int, skipLimit : Int, 
		  simpleProcessor :(Message => Imported)) :Int = {
	  val processor = {(i:Int, m : Message) => simpleProcessor(m)}; 
	  processMessagesWithIterator(folder, count, skipLimit, processor)
  }
  
  /**
   * The processor function will be invoked on each message, beginning at the (recent) end of
   * the folder. It will attempt to have `count` messages successfully processed
   * as reported by the processor function.  The processor function is given a single
   * message and should return true if the message was useful, false otherwise. For example
   * returning false if the message is read.
   * 
   * @param folder          The folder to process messages in
   * @param count           The number of messages to process
   * @param skipLimit       The number of messages to skip before stopping
   * @param processor       The method to call on each message
   */
  def processMessagesWithIterator(
		  folder : Folder, count : Int, skipLimit : Int, 
		  processor :((Int, Message) => Imported)
  	) : Int  = {
    val messages : Array[Message] = folder.getMessages()
    
    val toFetch =  scala.math.min(count, folder.getUnreadMessageCount)
    
    var i =  messages.length-1
    var fetched = 0
    var skipped = 0
    var waiting : List[(Int,Message)] = Nil
    
    while ( fetched < toFetch && i >= 0) { 
      processor(i, messages{i}) match {
    	  case Success => {
    	 	fetched = fetched + 1;
    	 	skipped = 0 }
    	  case Seen => 
    	  	skipped = skipped + 1
    	  case Wait =>
    	  	waiting = (i, messages{i}) :: waiting
    	  case Fail =>
    	  	
      }
      i = i - 1;
      if (skipped > skipLimit) {
    	debug("IMAP Import skipped past limit")
    	i = -2  
      }

    }
    for ((i,m) <- waiting) {
      debug("processing deferred message: "+EmailUtils.id(m))
      processor(i, m) match {
    	  case Success => fetched = fetched + 1
    	  case _ => 
      }
    }
    fetched
  }
  
  /**
   * Returns whether or not we have seen a given message before
   * 
   * @param msg    The message to check
   * @param search Takes a message id and checks if we have seen it
   */
  def seenBefore( msg : Message , searcher : String => Option[Int] ) : Boolean = {
    val msgid = EmailUtils.id(msg)
    val from = EmailUtils.address((msg getFrom()){0})
    if (from equals Config.POST_BACK_EMAIL)
    	return true

    searcher( msgid ) match {
    	case Some(_) =>
    		debug("Seen "+msgid+" before")
    		true
    	case None => false
    }

  }
  
  /**
   * Creates a new post object
   *
   * @param msg       Message to create a post from
   * @param src       Source where we got the message
   * @param onSuccess Method to be called on success
   * @param onFailure Method to be called on failure
   */
  def makePost( msg : Message, src : Source,
		  onSuccess : (Post, Message) => Unit ,
	  	  onFailure : (Message, String) => Unit ) = {
    val from = (msg getFrom()){0}    
    val poster = Person getByEmail(EmailUtils.address(from) , EmailUtils.name(from))
    val body = EmailUtils.plainTextify(msg)
  
    body match {
      case Some(text) => 
      	val dt = new java.util.Date()
        val p = Post makeNew(poster, msg.getSubject(), text, msg.getReceivedDate(), dt, src )
        onSuccess(p, msg)
        Success
      case None =>
        onFailure(msg, "no text body")
        Fail
    }
  }

  /**
   * Creates a new reply object
   *
   * @param msg       Message to create a reply from
   * @param parent    Parent of the reply
   * @param src       Source where we got the reply
   * @param onSuccess Method to be called on success
   * @param onFailure Method to be called on failure
   */
  def makeReply( msg : Message, parent : Post, src : Source,
		  onSuccess : (Post.Comment,  Post, Message) => Unit ,
	  	  onFailure : (Message, String) => Unit ) : Imported = {
    val from = (msg getFrom()){0}    
    val poster = Person getByEmail(EmailUtils.address(from) , EmailUtils.name(from))
    val body = EmailUtils.plainTextify(msg)
  
    body match {
      case Some(text) => 
        val c = parent.addComment(poster, msg.getSubject(), text, msg.getReceivedDate(), src )
        onSuccess(c, parent, msg)
        Success
      case None =>
        onFailure(msg, "no text body")
        Fail
    }
  }
}
