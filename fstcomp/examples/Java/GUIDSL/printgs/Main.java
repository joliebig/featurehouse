class Main   {

   static void debugActions() {
	    grammar.current.visit( new print() );
		 original();
	}
}
