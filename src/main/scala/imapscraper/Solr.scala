 /**
  * This class sets up information needed to later search the posts
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

import java.util.ArrayList
import org.apache.solr.common._
import org.apache.solr.client.solrj._
import org.apache.solr.client.solrj.impl._

object Solr extends log.Logging {

  def main( args : Array[String] ) ={
	  Config setup Array("postgres")
	  clearIndex
	  addNewPosts
  }
	
  def clearIndex = {
	doIt( server => {
	  info("Clearing Solr index")
	  val posts = Post findByAttribute "indexed"
	  posts map { p => p del "indexed"; p.commit;}
	  server deleteByQuery( "*:*" )
	  server commit
    })
  }
	
  def addNewPosts = {
    val posts = Post findWithoutAttribute "indexed"
      
    if (posts.size > 0 && Config.addingToSolr  ) {
      doIt( server => {
    	info("Adding "+posts.size+" new posts to Solr")
   	    val docs = new ArrayList[SolrInputDocument]()
	    posts map { k => docs add formatPost(k) }
	    server add docs
	    server commit ;
	    posts map { p => p add "indexed" ; p.commit; }
      })
    } else if (posts.size > 0)
      warn("Skipping Solr update ("+posts.size+") waiting")
  }
  
  def doIt( f : SolrServer => Unit ) {
    try {
	    val server = new CommonsHttpSolrServer( Config.solrServer )
	    f(server)
	} catch { case e => {
        error(e, "Cannot Reach Solr")
	    Config cannotReachSolr
	}}
	  
  }

  def formatPost(p : Post) = {
	  val doc = new SolrInputDocument()
	  doc addField ("type", "Post")
	  doc addField ("id", "Post "+ p.id)
	  doc addField ("title", p.title)
	  doc addField ("poster", p.poster.name)
	  doc addField ("posted_at", p.posted_at)
	  doc addField ("updated_at", p.updated_at)
	  for(i <- p.attributes) { 
	    if( (p get i get).length > 0)
	      doc addField(i+"_attribute", p get i get)
        else
	      doc addField("attribute", i)
      }
	  doc addField ("body", p.body)
	  doc
  }
}
