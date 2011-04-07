
 /**
  * This class stores information about a Person
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

import java.util.Date
import imapscraper.utils._
import imapscraper.identities._
import scala.collection.mutable._

class Person (val id: Int, var name: String) extends Object with RichSQL {

	/**
	 * This method updates the name of a person in the database.
	 *
	 * @param newName The new name that will be stored in the database
	 */
	def updateName(newName:Name) {
		name = newName.name
		
		val sql = prep ("UPDATE people SET name = \"?\" WHERE id = ? ")
		val res = sql << name << id executeQuery ;
	}
	
	/**
	 * This method links an email address to a Person.
	 *
	 * @param addr The email address to be linked
	 */
	def addEmail(addr:String) = {
		EmailIdent.create(addr, this)
		this
	}

   /**
	 * This method links an andrew id to a Person.
	 *
	 * @param andrew_id The andrew id to be linked
	 */
	def addAndrew(andrew_id:String) = {
		AndrewIdent.create(andrew_id, this)
		this
	}
   
}

object Person extends Object with RichSQL  {
	val cache = new Cache[Int, Person](1024);
	
    var miss = 0
    var access = 0
    
   /**
	 * This method returns the person with a specified id
	 *
	 * @param id The id to search for
	 * @return   A person object with the specified id
	 */
	def findPerson(id:Int) = {
		val sql = prep ("SELECT (name) FROM people WHERE id = ? ")
		val res = sql << id executeQuery ;
		miss = miss+1
		res next ;
		new Person(id, res getString 1 )
	}
		
	def get(id:Int):Person = {access = access + 1; cache.getOrElseUpdate(id, findPerson(id) ) } 
	
    /**
	 * This method returns the person with a specified email
	 *
	 * @param email The email to search for
	 * @return      A person object with the specified email
	 */
	def getByEmail(email:String, possibleName:Option[Name] ) = {

		var create : ( Person => Person ) = (k:Person) => k;
		var ident : Option[Identity] = None; 
	  
	    if (AndrewIdent.is_andrew(email)) {
	    	ident = AndrewIdent.findByEmail(email)
	    	create =  (_:Person).addAndrew(EmailUtils.username(email))
	    } else {
	    	ident = EmailIdent.find(email)
	    	create =  (_:Person).addEmail(email)
	    }
		ident match {
			case Some(m) => m.owner
			case None =>
			  possibleName match {
			    case Some(name) => create(makeNew(name))
			    case None => throw new IllegalArgumentException("Need name for new person")
			  }
		}
	    
	}
	
    /**
	 * This method creates a new person object in the database
	 *
	 * @param name The name of the person to create
	 */
    def makeNew(name:Name) : Person = {
		val sql = ("INSERT INTO people (name, created_at, confidence) VALUES(?, ?, ?)")
		val result = insertAndGetId(sql, _ << name.name << (new Date()) << name.confidence , "people.id") ;
		result next;
		get(result getInt 1);
	}

    def makeNew(name:String) : Person = makeNew( Name(name, 0) )
 

}
