 /**
  * This class is an extension of the IMAP import for CMU's misc.market
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
import utils._
import utils.RichTypes._
import java.io.File
import javax.mail._
import javax.mail.internet._

object MiscMarket extends IMAPImporter with RichSQL with log.Logging {
  val host = "cyrus.andrew.cmu.edu"
  val user = Config.imapUser
  val pass = Config.imapPass
  val folderName = "cmu.misc.market"
  
  val src = Source.IMAP ;
  val cache = new Cache[String, Int](1024);
  
  def getStore = Session getInstance(defaultProperties(host, true), null) getStore("imaps"); 

  def doImport = openAndDo(folderName) { f =>
    info("Starting import from Misc Market")
    val imported = processMessages (f, messagesPerScrape, skipLimit, importer )
    info("Misc Market: "+imported+" messages imported")
  	}
  
  def importer (msg : Message) : Imported = {
    if (!seenBefore(msg, msgIdToPostId)) {
      if(EmailUtils.isReply(msg)) { 
	    findParent(msg) match {
    	  case Some(parent) => makeReply(msg, parent, src, logReplyCreation, logFailure)
    	  case None =>
    	   Wait
    	}
      } else
    	makePost(msg, src, postCreated, logFailure)
    } else
      Seen
  }
 
  def logFailure ( msg : Message, reason : String) = {
	  warn(reason)
  }

  def msgIdToPostId(msgid : String) : Option[Int] = {
	if (cache containsKey msgid) {
	  cache get msgid
	} else {
      val sql = prep ("SELECT post_id FROM misc_market_import WHERE msg_id = ? ")
	  val res = (sql << msgid) executeQuery ;
      if (res next) {
    	val pid = as_i(res, "post_id")
    	cache put(msgid, pid)
        Some(pid)
 	  } else None
	}
  }
  
  def findParent(msg : Message) : Option[Post] = {
	  val repid = EmailUtils.replyid(msg)
	  repid match {
	 	  case Some(id) =>
	 	  	msgIdToPostId(id) match {
	 	  		case Some(pid) => Post.get(pid)
	 	  		case None => 
  		    	   info("Do not yet have parent for reply: "+id)
  		    	   None
	 	  	}
	 	  case None => None
	  }
  }
  
  def saveImage(p:Post)(i: Int, bp : MimeBodyPart) = {
    val user_dir = p.poster.id.toString
    val path = src.id + File.separator + user_dir + File.separator
    val dir = new File(Config.imagesPath, path)

    if(bp.isMimeType("image/jpeg") ||bp.isMimeType("image/pjpeg")  ) {
      val file_name = p.id + "-"+i+".jpg"
      dir mkdirs ;
      val f = new File(dir, file_name);
      info("Saving attached jpeg to "+path+file_name)
      bp saveFile f;
      p.addImage(bp.getFileName, path+file_name, "image/jpeg")
      Some( (f, bp.getFileName) )
	} else {
	 warn("Unsupported attachment type: "+bp.getContentType )
	 None
	}

  }
  
  def postCreated(result : Post, src : Message) = {
	EmailUtils.getAttachments(src, saveImage(result))
    logPostCreation(result, src)
  }
    
  def logReplyCreation(result : Post.Comment, parent : Post, src : Message) {
    val msgid = EmailUtils.id(src)
    info("imported reply "+msgid)
  	val st = prep ("INSERT INTO misc_market_import (post_id, comment_id, msg_id) VALUES(?,?,?)")
	( st << parent.id << result.id << msgid ) executeUpdate ;
  
  }
  
  def logPostCreation(result : Post, src : Message) = {
    val msgid = EmailUtils.id(src)
    info("imported "+msgid)
  	val st = prep ("INSERT INTO misc_market_import (post_id, msg_id) VALUES(?,?)")
	( st << result.id << msgid ) executeUpdate ;
  }
}