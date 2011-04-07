 /**
  * This class stores information about a Post
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
import utils.RichSQL._
import scala.collection.mutable._

import java.util.Date;

class Post (val id: Int, val poster : Person, 
		val title : String , val body : String, 
		val posted_at : Date, val updated_at : Date,
		val src: Source ) extends RichSQL {
  private var loaded = false
  private val _attributes = HashMap[String, String](); 
  private var _comments : List[Post.Comment] = Nil
  private var _images : List[Post.Image] = Nil
  
  private def load() = {
	loadAttrs
	loadComments
	loadImages
    loaded = true
  }
  
  private def loadAttrs() = {
	val sql = prep ("SELECT t.name, a.value FROM attribute_types t, attributes a WHERE a.attribute_type_id = t.id and a.post_id = ? ")
	val res = sql << id executeQuery ;
	while (res next) {
	  _attributes.put(res getString 1, res getString 2)
	}
  }

    /**
	 * This method deletes an attribute of a post
	 *
	 * @param name The name of the attribute to delete
	 */
  private def delete(name:String) : Unit = {
    val attr = Post attr_type name get
    val delq = prep ("DELETE FROM attributes WHERE post_id = ? AND attribute_type_id = ?")
    delq << id << attr execute
  }

    /**
	 * This method creates an attribute for a post
	 *
	 * @param name The name of the attribute to create
	 */
  private def save(name:String) : Unit = {
    val attr = Post attr_type name get ;
    delete(name)
    val addq = prep ("INSERT INTO attributes (post_id , attribute_type_id , value) VALUES ( ?, ? , ? )")
    val ret = addq << id <<  attr << (get(name) get) execute ;
  }
  
    /**
	 * This method adds an image to a post
	 *
	 * @param name        The name of the image
	 * @param filename    The filename of the image
	 * @param contentType The content type of the image
	 */
  def addImage(name:String, filename:String, contentType:String) = {
	loaded = false
	val sql = ("INSERT INTO images (post_id, name, img_file_name, location, img_content_type) VALUES(?,?,?,?,?)")
	val result = insertAndGetId(sql, _ << id << name << name << filename << contentType, "images.id");
	if( result next)
		Post.Image.get(this,result getInt 1) get
	else
	  error("addImage failed")
  }
  
  private def loadImages = {
	_images = Nil
	val sql = ("SELECT "+Post.Image.fields+" FROM images WHERE post_id = ?")
	val res = sql << id executeQuery ;
	while (res next) {
	  _images = Post.Image.fromSQL(this, res) :: _images
	}
  }
  
  /**
	 * This method adds a comment to a post
	 *
	 * @param poster The person who created the comment
	 * @param title  The title of the post
	 * @param body   The body of the post
	 * @param posted The date that the post was posted
	 * @param src    The source that the comment was created at
	 */
  def addComment(poster: Person, title:String, body:String, posted:Date, src: Source) = {
	loaded = false
	val sql = ("INSERT INTO comments (person_id, post_id, title, body, posted_at, source_id) VALUES(?,?,?,?,?,?)")
	val result = insertAndGetId(sql, _ << poster.id << id << title << body << posted << src.id , "comments.id");
	if( result next)
		Post.Comment.get(this,result getInt 1) get 
	else
	  error("addComment failed")
  }
  
  private def loadComments = {
	 _comments = Nil
	val sql = ("SELECT "+Post.Comment.fields+" FROM comments WHERE post_id = ? ORDER BY posted_at")
	val res = sql << id executeQuery ;
	while (res next) {
	  _comments = Post.Comment.fromSQL(this, res) :: _comments
	}
  }
  
  def attributes = { if (!loaded) load() ; _attributes keysIterator }
  def get = { if (!loaded) load() ; _attributes get _ }
  def set = { if (!loaded) load() ; _attributes put(_, _)} 
  def add = { if (!loaded) load() ;_attributes put(_:String, "")} 
  def has = { if (!loaded) load() ; _attributes contains _ }
  def del(k:String) = { if (!loaded) load() ; if (has(k)) delete(k); _attributes remove k }
  def commit = for( i <-_attributes.keysIterator) { save(i) }
  
  override def toString = {
	  if (!loaded) load() ;
	  title+"\n"+poster.name+"\n"+(_attributes mkString "\n")+"\n----\n"+body+"\n----\n"
  }
}

object Post extends RichSQL  {
    val fields = "p.id, p.person_id, p.title, p.body, p.posted_at, p.updated_at, p.source_id"
    	
    /**
	 * This method returns the post with a specified id
	 *
	 * @param id The id to search for
	 * @return   A post object with the specified id
	 */
	def get(id:Int) = {
		val sql = prep ("SELECT "+fields+" FROM posts p WHERE id = ? ")
		optionize(sql << id executeQuery)
	}
 
    /**
	 * This method returns the posts with a specified attribute
	 *
	 * @param attrib The attribute to search for
	 * @return       All post objects with the specified attribute
	 */
	def findByAttribute(attrib : String) = {
	  val attr = Post attr_type attrib get
	  val sql = prep("SELECT "+fields+" FROM posts p, attributes a " +
		"WHERE a.post_id = p.id AND a.attribute_type_id = ?")
      val rs = (sql << attr executeQuery)
      rs map fromSQL
	}

    /**
	 * This method returns the posts with a specified attribute value
	 *
	 * @param attrib The attribute to search for
	 * @param cond   A conditional statement to filter posts by
	 * @param value  The value of the attribute to search for
	 * @return       All post objects with the specified attribute value and condition
	 */
 	def findByAttributeValue(attrib : String, cond : String, value : String) = {
	  val attr = Post attr_type attrib get
	  val sql = prep ("SELECT "+fields+" FROM posts p, attributes a " +
		"WHERE a.post_id = p.id AND a.attribute_type_id = ? AND "+cond);
	  (sql << attr << value executeQuery) map fromSQL
	}

    /**
	 * This method returns the posts without a specified attribute
	 *
	 * @param attrib The attribute to search for
	 * @return       All post objects without the specified attribute
	 */
	def findWithoutAttribute(attrib : String) = {
	  val attr = Post attr_type attrib get
	  val sql = prep ("SELECT "+fields+" FROM posts p LEFT OUTER JOIN attributes a " +
		"on a.post_id = p.id AND a.attribute_type_id = ? WHERE a.id IS NULL");
	  (sql << attr executeQuery) map fromSQL
	}
 
    /**
	 * This method creates a new post object
	 *
	 * @param poster  The person who created the post
	 * @param title   The title of the post
	 * @param body    The body of the post
	 * @param posted  The date the post was created
	 * @param updated The date the post was updated
	 * @param src     The source of the post
	 */
	def makeNew(poster: Person, title:String, body:String, posted:Date, updated:Date, src: Source) = {
		val sql = ("INSERT INTO posts (person_id, title, body, created_at, posted_at, source_id) VALUES(?,?,?,?,?,?)")
		val result = insertAndGetId(sql, _ << poster.id << title << body << (new Date())<< posted << src.id , "posts.id")  ;
		result next;
		get(result getInt 1) get 
	}
	
	/**
	 * This method returns the id of an attribute type
	 *
	 * @param name The name of the attribute type
	 * @return     An int option containing the id of the attribute type
	 */
	def attr_type( name : String ) : Option[Int] = {
		val sql = prep ("SELECT id FROM attribute_types WHERE name = ? ")
		val res = sql << name executeQuery ;
		if( res next ) { 
			Some(res getInt "id")
		} else {
			val sql = "INSERT INTO attribute_types (name)  VALUES ( ? )"
			val ret = insertAndGetId(sql, (_ << name) , "attribute_types.id") ;
			if( ret next ) { 
			  Some(ret getInt 1)
			} else None
		}
	}
 
    def fromSQL(rs : java.sql.ResultSet) = {
	  val s = Source find as_i(rs,"source_id") get
	  val p = Person get (as_i(rs,"person_id"))
	  new Post( as_i(rs,"id"), p, as_s (rs,"title"), as_s (rs,"body"), as_t (rs,"posted_at"), as_t (rs,"updated_at"), s)
	}
 
 	def optionize(rs : java.sql.ResultSet) :Option[Post] = {
		if (rs next) {
			Some( fromSQL(rs) )
		} else None
	}
 	
	class Comment (val id: Int, val parent : Post, val poster : Person, 
			val title : String , val body : String, 
			val posted_at : Date,
			val src: Source ) extends RichSQL {
	
		override def toString = title+"\n"+poster.name+"\n"+body+"\n"
	}

	object Comment extends RichSQL {
	  val fields = "id, person_id, title, body, posted_at, source_id"
	
	  def get(parent : Post, id : Int) = {
		val sql = ("SELECT "+Comment.fields+" FROM comments WHERE id = ?")
		val res = sql << id executeQuery ;
		if (res next)
		  Some(fromSQL(parent, res))
		else
		  None
	  }
		  
	  def fromSQL(p: Post, rs : java.sql.ResultSet) = {
		  val s = Source find as_i(rs,"source_id") get
		  val owner = Person get (as_i(rs,"person_id"))
		  new Comment( as_i(rs,"id"), p, owner, as_s (rs,"title"), as_s (rs,"body"), as_t (rs,"posted_at"), s)
		}
	}
	class Image (val id: Int, val parent : Post, 
			val name : String , val filename : String,
			val created_at : Date) extends RichSQL {
	}
	
	object Image extends RichSQL {
	  val fields = "id, post_id, name, location, created_at"
	
	  def get(parent : Post, id : Int) = {
		val sql = ("SELECT "+Image.fields+" FROM images WHERE id = ?")
		val res = sql << id executeQuery ;
		if (res next)
		  Some(fromSQL(parent, res))
		else
		  None
	  }
		  
	  def fromSQL(p: Post, rs : java.sql.ResultSet) = {
		new Image( as_i(rs,"id"), p, as_s(rs,"name"), as_s (rs,"location"), as_t(rs,"created_at"))
	  }
	}


}


