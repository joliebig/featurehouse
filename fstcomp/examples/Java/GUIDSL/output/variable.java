class variable {
   boolean eqn = false;   		// assume no equation to output for this variable
	                           // makes sense only for productions
   String  out;               // what to output in .equations file
                              // only for primitives
   boolean reverse;           // output ordering of .equations file


   public void print() {
     original();
     System.out.print( " eqn ="  + eqn );
   }

   public static variable define( String name, int type, gObj g, boolean b ) {
      variable v = original(name,type,g, b);
      if (v!=null)
         v.out = name;
      return v;
	}
}

