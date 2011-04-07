package imapscraper
package log

import org.apache.log4j.Logger

trait Logging
{
	val log = Logging.getLogger(this)
	
	def checkFormat(msg:String, refs:Seq[Any]):String =
        if (refs.size > 0) msg + refs toString else msg 

    def debug(msg:String, refs:Any*) = log debug checkFormat(msg, refs)
    def debug(t:Throwable, msg:String, refs:Any*) = log debug (checkFormat(msg, refs), t)
    def info(msg:String, refs:Any*) = log info checkFormat(msg, refs)
    def info(t:Throwable, msg:String, refs:Any*) = log info (checkFormat(msg, refs), t)
    def warn(msg:String, refs:Any*) = log warn checkFormat(msg, refs)
    def warn(t:Throwable, msg:String, refs:Any*) = log warn (checkFormat(msg, refs), t)

	def error(msg:String, refs:Any*) = log error checkFormat(msg, refs)
    def error(t:Throwable, msg:String, refs:Any*) = log error (checkFormat(msg, refs), t)
    def fatal(msg:String, refs:Any*) = log fatal checkFormat(msg, refs)
    def fatal(t:Throwable, msg:String, refs:Any*) = log fatal (checkFormat(msg, refs), t)	
}

object Logging {
    def loggerNameForClass(className: String) = {  
        if (className endsWith "$") className.substring(0, className.length - 1)  
        else className  
    }  

    def getLogger(logging: AnyRef) = Logger.getLogger(loggerNameForClass(logging.getClass.getName))  
}
