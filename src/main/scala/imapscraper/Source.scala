 /**
  * This class stores information about a Source
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

import utils._
import scala.collection._

case class Source (val id: Int, var name: String)

object Source extends Object with RichSQL  {
    val name_cache = new Cache[String, Option[Source]](1024);
    val id_cache = new Cache[Int, Option[Source]](1024);
    
    def SELF = find("SERVICE NAME") get
    def IMAP = find("Misc Market") get

    def find(s:String) = name_cache.getOrElseUpdate(s, findByName(s))
    def find(i:Int) = id_cache.getOrElseUpdate(i, findById(i))
    
    /**
	 * This method returns the source with a specified id
	 *
	 * @param id The id to search for
	 * @return   A source object with the specified id
	 */
	def findById(id: Int) = {
		val sql = prep ("SELECT id, name FROM sources WHERE id = ? ")
		optionize ((sql << id) executeQuery) ;
	}
	
    /**
	 * This method returns the source with a specified name
	 *
	 * @param name The name to search for
	 * @return     A source object with the specified name
	 */
	def findByName(name:String) = {
	  val sql = prep ("SELECT id, name FROM sources WHERE name = ? ")
	  optionize ((sql << name) executeQuery) match {
		case Some(s) => Some(s)
		case None => {
	      val sql = "INSERT INTO sources (name)  VALUES ( ? )"
		  val ret = insertAndGetId(sql, (_ << name) , "sources.id") ;
		  if( ret next ) {
		    val id = ret getInt 1
			findById(id)
		  } else None
		}
	  }
	}
	
	/**
	 * This method takes a result set and turns it into a source option
	 *
	 * @param rs The result set to convert
	 * @return   None if the set is empty, Some(source) if it is not
	 */
	def optionize(rs : java.sql.ResultSet) :Option[Source] = {
		if (rs next) {
		    val s = new Source(rs getInt 1, rs getString 2)
		    name_cache.put(s.name, Some(s))
		    id_cache.put(s.id, Some(s))
			Some(s)
		} else None
	}
}
