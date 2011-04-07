 /**
  * This class is responsible for managing all of the backend tasks that need to be run
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

object Service extends log.Logging {
	var mainThread : Thread = null
	var shutdownRequested = false
	
	/**
	 * This method starts the necessary services on the backend
	 *
	 * @param args A list of arguments to config the necessary fields in Config
	 */
	def main(args: Array[String]) = {
	  mainThread = Thread currentThread ;

	  info("ImapScraper starting...")
	  
	  Config setup args
	
	  info("Running on "+Config.hostname)
	  if (Config.prod) info("Using PRODUCTION settings")
	  else info("Using DEV/STAGING settings")
	  if (Config.debug) info("Using DEBUG settings")
	  if (Config.local) info("Using local database (sqlite)")

	  addDaemonShutdownHook
	  
	  if(Config.shouldDaemonize) daemonize
	  
	  if(!Config.local && !Config.prod) Solr clearIndex
	  
	  while(!shutdownRequested ) {
		try {
			Importer importNew ;
			Processor processNew ;
			Solr addNewPosts ;
		    Thread.sleep (60 * 1000)
		} catch {
			case e:InterruptedException => {}
			case e => {
				fatal(e, "Backend Service exiting" )
				shutdown
			}
		}
	  }
	}

	/**
	 * This method closes sysout and syserr so the backend can run as a service
	 */
	def daemonize = {
		new java.io.File(Config.pidFileName).deleteOnExit
		System.out.close() ;
		System.err.close() ;
		info("Daemonized backend")
	}
	
	def addDaemonShutdownHook = {
		Runtime.getRuntime().addShutdownHook(
			new Thread() {
				override def run() = {
					Service.shutdownHook
				}
			});
	}
	
	def shutdown = {
		info("Starting shutdown")
		shutdownRequested = true
		if (Thread.currentThread != mainThread)
			mainThread interrupt ;
	}
	
	def shutdownHook = {
	  shutdown
	  mainThread join ;		
	}	
}
