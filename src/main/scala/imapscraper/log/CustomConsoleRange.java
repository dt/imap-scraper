package imapscraper.log;

import org.apache.log4j.Level;
import org.apache.log4j.spi.LoggingEvent;
import imapscraper.Config;

public class CustomConsoleRange extends org.apache.log4j.varia.LevelRangeFilter {
  Level saved = null ;
	
  public int decide(LoggingEvent e) {
	  if(Config.debug() && saved == null) {
		  saved = getLevelMin();
		  setLevelMin(Level.ALL);
	  } else if(saved != null) {
		  setLevelMin(saved);
		  saved = null;
	  }
	  return super.decide(e);
  }
}
