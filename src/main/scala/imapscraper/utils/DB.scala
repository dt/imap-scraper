package imapscraper.utils

import imapscraper._
import RichSQL._

object DB extends RichSQL {
  val tables = Array( "posts", "comments", "sources", "people", "andrew_ids", 
		  "emails", "accounts", "misc_market_import", 
		  "attributes", "attribute_types", "schema_migrations", "validation_permissions", "images")	

  def main(args : Array[String]) : Unit = {
	  drop
	  load
	  count
  }
  
  def readFileAsString(filePath : String) : String = {
	import java.io._
	import java.nio._
	
    val stream = new FileInputStream(new File(filePath));
    try {
      val fc = stream.getChannel();
      val bb = fc.map(channels.FileChannel.MapMode.READ_ONLY, 0, fc.size());
      /* Instead of using default, pass in a decoder. */
      return charset.Charset.defaultCharset().decode(bb).toString();
    } finally {
      stream.close();
    }
  }

  
  def load = {
	val cs = readFileAsString("db/struct.sql") split ";"
    cs.map { s => conn.execute( s + ";" ); println(s) }
    
  }
		  
  def drop = {
    val deleter = tables.map { "DROP TABLE "+_}
    for(q <- deleter) {
	  try { (conn.execute(q)) } catch { case e => }
    }
  }
  
  def count = {
  	val counter = tables.map { "SELECT COUNT(*) FROM "+_} map {prep(_)}
	for(q <- counter)
		(q executeQuery).map{print_rs}

  }
  
  def emptyTable (t : String) = {
	 val q = "DELETE FROM "+ t
	 println(q)
	 conn execute q
  }
  
  def empty = {
    for(t <- tables) {
	  try { emptyTable(t) } catch { case e => }
    }
  }
  
  def print_rs(rs : java.sql.ResultSet) = {
	  val rsmd = rs.getMetaData(); 
	  for (i <- 1 until rsmd.getColumnCount + 1) {
	 	  println((rsmd getColumnName(i)) + " :\t" + rs.getInt(i) )
	  }
  }
}