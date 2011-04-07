package imapscraper.log;

import org.apache.log4j.net.SMTPAppender;

import imapscraper.Config;

public class CustomSMTPAppender extends SMTPAppender {

	  protected
	  boolean checkEntryConditions() {
		if (!Config.prod())
		{
		  return false;
		}
		  
	    if(this.msg == null) {
	      errorHandler.error("Message object not configured.");
	      return false;
	    }

	    if(this.evaluator == null) {
	      errorHandler.error("No TriggeringEventEvaluator is set for appender ["+
				 name+"].");
	      return false;
	    }


	    if(this.layout == null) {
	      errorHandler.error("No layout set for appender named ["+name+"].");
	      return false;
	    }
	    return true;
	  }
}
