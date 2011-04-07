package imapscraper.utils

import imapscraper.Config
import java.sql.{DriverManager, Connection, ResultSet, PreparedStatement, Statement, Date, Time , Timestamp}
import java.util.{Date => JDate}
import java.text.SimpleDateFormat

trait RichSQL {
	val db = RichSQL.conn
	val prep = RichSQL.str2RichPrepared (_:String) (db)

    def as_s(r:ResultSet, f:String) = r.getString(f)
	def as_i(r:ResultSet, f:String) = r.getInt(f)
	def as_t(r:ResultSet, f:String) = {
		if(RichSQL.usingSQLite) {
			val dt = r.getString(f)
			if(dt != null)
				RichSQL.sdf.parse(dt)
			else
				null
		}
		else r.getTime(f)
	}
	
 	def insertAndGetId( 
					query : String, 
					filler : (imapscraper.utils.RichSQL.RichPreparedStatement => imapscraper.utils.RichSQL.RichPreparedStatement ) , 
					idName : String ) = {
				
 				  if(RichSQL.usingSQLite) {
						filler( prep (query) ) executeUpdate ;
						prep("SELECT last_insert_rowid()") executeQuery
				  } else {
					  filler( prep (query + " RETURNING "+idName) ) executeQuery
          }
				
      }
}

object RichSQL {
	var usingSQLite = false
	
   	implicit val conn = {
		usingSQLite = Config.jdbcString contains "jdbc:sqlite"
		if(usingSQLite)
			Class.forName("org.sqlite.JDBC");
		DriverManager.getConnection(Config.jdbcString, Config.dbUser, Config.dbPass);
	}

      val DATE_FORMAT = "yyyy-MM-dd HH:mm:ss";
      val cal = { val c = java.util.Calendar.getInstance();
		  		c.setTimeZone(java.util.TimeZone.getTimeZone("GMT"));
		  		c}
      val sdf = {val s = new SimpleDateFormat(DATE_FORMAT); s.setCalendar(cal); s}
	
	
   	private def strm[X](f: RichResultSet => X, rs: ResultSet): Stream[X] = 
        if (rs.next) Stream.cons(f(new RichResultSet(rs)), strm(f, rs))
        else { rs.close(); Stream.empty };
 
    implicit def query[X](s: String, f: RichResultSet => X)(implicit stat: Statement) = {
        strm(f,stat.executeQuery(s));
    }
    
    implicit def str2RichPrepared(s: String)(implicit conn: Connection): RichPreparedStatement = ps2Rich ( conn prepareStatement(s) );
   
    implicit def conn2Statement(conn: Connection): Statement = conn.createStatement;
    
    implicit def rrs2Boolean(rs: RichResultSet) = rs.nextBoolean;
    implicit def rrs2Byte(rs: RichResultSet) = rs.nextByte;
    implicit def rrs2Int(rs: RichResultSet) = rs.nextInt;
    implicit def rrs2Long(rs: RichResultSet) = rs.nextLong;
    implicit def rrs2Float(rs: RichResultSet) = rs.nextFloat;
    implicit def rrs2Double(rs: RichResultSet) = rs.nextDouble;
    implicit def rrs2String(rs: RichResultSet) = rs.nextString;
    implicit def rrs2Date(rs: RichResultSet) = rs.nextDate;
 
    implicit def resultSet2Rich(rs: ResultSet) = new RichResultSet(rs);
    implicit def rich2ResultSet(r: RichResultSet) = r.rs;
    class RichResultSet(val rs: ResultSet) {
        
      var pos = 1
      def apply(i: Int) = { pos = i; this }
        
      def nextBoolean: Boolean = { val ret = rs.getBoolean(pos); pos = pos + 1; ret }
      def nextByte: Byte = { val ret = rs.getByte(pos); pos = pos + 1; ret }
      def nextInt: Int = { val ret = rs.getInt(pos); pos = pos + 1; ret }
      def nextLong: Long = { val ret = rs.getLong(pos); pos = pos + 1; ret }
      def nextFloat: Float = { val ret = rs.getFloat(pos); pos = pos + 1; ret }
      def nextDouble: Double = { val ret = rs.getDouble(pos); pos = pos + 1; ret }
      def nextString: String = { val ret = rs.getString(pos); pos = pos + 1; ret }
      def nextDate: Date = { val ret = rs.getDate(pos); pos = pos + 1; ret }
 
      def foldLeft[X](init: X)(f: (ResultSet, X) => X): X = rs.next match {
        case false => init
        case true => foldLeft(f(rs, init))(f)
      }
      def map[X](f: ResultSet => X) = {
        var ret = List[X]()
        while (rs.next())
        ret = f(rs) :: ret
        ret.reverse; // ret should be in the same order as the ResultSet
      }
    }
     
    implicit def ps2Rich(ps: PreparedStatement) = new RichPreparedStatement(ps);
    implicit def rich2PS(r: RichPreparedStatement) = r.ps;
 
    class RichPreparedStatement(val ps: PreparedStatement) {
      var pos = 1;
      private def inc = { pos = pos + 1; this }
 
      def execute[X](f: RichResultSet => X): Stream[X] = {
	pos = 1; strm(f, ps.executeQuery)
      }
      def executeQuery = ps.executeQuery
      def executeUpdate = ps.executeUpdate
      
      def <<![X](f: RichResultSet => X): Stream[X] = execute(f);
 
      def execute = { pos = 1; ps.execute }
      def <<! = execute;
  
      def <<(b: Boolean) = { ps.setBoolean(pos, b); inc }
      def <<(x: Byte) = { ps.setByte(pos, x); inc }
      def <<(i: Int) = {  ps.setInt(pos, i); inc }
      def <<(x: Long) = { ps.setLong(pos, x); inc }
      def <<(f: Float) = { ps.setFloat(pos, f); inc }
      def <<(d: Double) = { ps.setDouble(pos, d); inc }      
      def <<(o: String) = { ps.setString(pos, o); inc }
            
      def <<(x: JDate) = {
		 if(usingSQLite) {
			 val dt = sdf.format(x)
			 println(dt)
		 	 ps.setString(pos, dt)
		 } else
			 ps.setTimestamp(pos, x, cal)
    	 inc }
      
      override def toString = ps toString
    }
    
    implicit def date2ts(dt: JDate) : Timestamp = {  new Timestamp(dt.getTime()); }
    implicit def ts2Date(ts: Timestamp) : JDate = new Date(ts.getTime());
    implicit def jdate2time(jd : JDate) : Time = new Time(jd.getTime());
    implicit def conn2Rich(conn: Connection) = new RichConnection(conn);
    
    class RichConnection(val conn: Connection) {
      def <<(sql: String) = new RichStatement(conn.createStatement) << sql;
      def <<(sql: Seq[String]) = new RichStatement(conn.createStatement) << sql;
    }
 
    implicit def st2Rich(s: Statement) = new RichStatement(s);
    implicit def rich2St(rs: RichStatement) = rs.s;
    
    class RichStatement(val s: Statement) {
      def <<(sql: String) = { s.execute(sql); this }
      def <<(sql: Seq[String]) = { for ( x <- sql) s.execute(x); this }
    }
}
