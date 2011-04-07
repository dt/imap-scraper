 /**
  * This class is responsible for maintaining all global information about the backend.
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

package imapscraper

import java.io.File

object Config extends log.Logging {
   
  val imapUser = "username"
  val imapPass = new String(utils.Base64 decode(""))

  val POST_BACK_EMAIL = "postback@service.domain"
  val prodHostname = "puffer"
  val pidDefault = "backend.pid"
  val DB = Split( //stage, prod (dev is sqlite)
    "jdbc:postgresql://db.domain.tld:5432/staging",
    "jdbc:postgresql://db.domain.tld:5432/production")
  val localJDBC = "jdbc:sqlite:db/test.sqlite3"
  val dbUser = "backend"
  val dbPass = "password"
  val solrSkipOnFailure = 60
  
  val fs = java.io.File.separatorChar 
  val fsroot = (java.io.File.listRoots(){0}).toString
  val images = Split(
		 fsroot+"www"+fs+"blah"+fs+"attachments"+fs+"stage"+fs,
		 fsroot+"www"+fs+"blah"+fs+"attachments"+fs+"prod"+fs)
  
  val solr = Split( //dev, prod
    "http://solr.domain.tld:8983/stage",
    "http://solr.domain.tld:8983/prod" )

  def jdbcString = { if (local) { info("Using local SQLite"); localJDBC }
    else { 
    	if(prod) { info("Using production Postgres"); DB.prod}
    	else { info("Using development Postgres"); DB.dev }
    }
  }

  def imagesPath = {
	  if(prod) new File(images.prod) else new File(images.dev)
  }
  
  def solrServer = if(prod) solr.prod else solr.dev
  
  var skipSolr = 0
  def addingToSolr : Boolean = {(!local) && (if (skipSolr > 0) {skipSolr -= 1; false} else true) }  
  def cannotReachSolr : Unit = { skipSolr = solrSkipOnFailure }
  
  def prod  = _prod
  def local = _local
  def debug = _debug
  val hostname = java.net.InetAddress.getLocalHost().getHostName()
  var _local =  ! hostname.contains(prodHostname)
  
  var _debug = false
  var _prod = false
  var _daemonize = false
  var _pidFile = ""
  
  /**
   * This method looks through the array of arguments it is passed and sets up
   * the correct fields for every argument.  
   * 
   * @param args This is a list of arguments used to set the necessary fields.
   */
  def setup( args: Array[String]) = {
	_prod = args exists ("production" == _)
	_daemonize = args exists ("daemonize" == _)
	if(args exists ("debug" == _)) turnOnDebug
	if( args exists ("postgres" == _)) _local = false; 
	_pidFile = (args find (_ contains "pid")) match {
		case Some(s) => s
		case None => pidDefault
	}
  }
  
  def turnOnDebug = { _debug = true }
  
  def shouldDaemonize = (_daemonize)
  def pidFileName = _pidFile 
 
  case class Split( dev: String, prod:String )
}
