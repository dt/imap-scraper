package imapscraper.identities

import imapscraper.utils._
import imapscraper.Person

case class AndrewIdent(override val owner: Person, andrew_id: String) extends Identity(owner)

object AndrewIdent extends Object with RichSQL {
    def is_andrew(email : String) = {
      EmailUtils.host(email) == "andrew.cmu.edu"
    }
  
    def findByEmail( email : String) = {
      find( EmailUtils.username(email) )
    }
  
	def find( andrew_id:String) = {
		val sql = prep ("SELECT person_id, andrew_id FROM andrew_ids WHERE andrew_id = ? ")
		optionize ((sql <<  andrew_id) executeQuery) ;
	}
	
	def create( andrew_id: String, person: Person) = {
		val st = prep ("INSERT INTO andrew_ids (person_id, andrew_id) VALUES(?, ?) ")
		st << person.id << andrew_id execute ;
	}
	
	def optionize(rs : java.sql.ResultSet) :Option[AndrewIdent] = {
		if (rs next) {
			val owner = Person.get (rs getInt "person_id" )
			Some(new AndrewIdent(owner, rs getString 2))
		} else None

	}
}
