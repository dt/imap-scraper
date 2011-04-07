 /**
  * This class is responsible for importing posts from misc.market
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

object Importer {
  def importNew = {
	  importers.MiscMarket doImport ;
  }
}