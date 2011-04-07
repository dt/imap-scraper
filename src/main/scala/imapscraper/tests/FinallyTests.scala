package imapscraper.tests

object FinallyTests {
  def foo( f : Unit => Unit ) = {
	  try {
	 	  f()
	  } finally {
	 	  println("foo")
	  }
  }
  def bar( g : Unit => Unit ) = {
	  try {
	 	  g()
	  } finally {
	 	  println("bar")
	  }
  }
  def main(args : Array[String]) : Unit = {
	foo { _ => bar { _ => throw new RuntimeException(); } }  
  }
}
