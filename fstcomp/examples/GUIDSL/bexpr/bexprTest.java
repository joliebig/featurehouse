class bexprTest {
   public static void main(String args[]) {
     node n = new and( new iff( new bterm("Src"), 
	                             new or( new bterm("_BFS"),
										          new bterm("_DFS") )
							  ),
							  new atmostone( new bterm("_BFS"), new bterm("_DFS") )
							);
	  /*
	  node n = new atmostone( new bterm("X"), new bterm("Y") );
	  */
	  System.out.println(n);
	  node s = n.klone().simplify();
	  System.out.println(s);
	  node c = s.klone().cnf();
	  System.out.println(c);
	}
}
