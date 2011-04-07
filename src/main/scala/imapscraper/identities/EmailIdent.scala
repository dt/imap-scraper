package imapscraper.identities

import imapscraper.utils._
import imapscraper.Person

case class EmailIdent(override val owner: Person, address: String) extends Identity(owner)

object EmailIdent extends Object with RichSQL {
	def find(email:String):Option[EmailIdent] = {
		val sql = prep ("SELECT person_id, address FROM emails WHERE address = ? ")
		optionize ((sql << email) executeQuery) ;
	}
	
	def create(email: String, person: Person) = {
		val st = prep ("INSERT INTO emails (person_id, created_at, address) VALUES(?, ?, ?)")
		val result = st << person.id << new java.util.Date() << email executeUpdate ;
		find( email );
	}
	
	def optionize(rs : java.sql.ResultSet) :Option[EmailIdent] = {
		if (rs next) {
			val owner = Person.get (rs getInt "person_id" )
			Some(new EmailIdent(owner, rs getString 2))
		} else None

	}
}
