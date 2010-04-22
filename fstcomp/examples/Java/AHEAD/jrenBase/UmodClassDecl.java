

class UmodClassDecl   {

    public void reduce2java( AstProperties props ) {

	    String depth = (String) props.getProperty("inside");

	    // rename only the outer-most class
	    if ( depth == null ) {
          if (arg[0] instanceof NameId) {
   		    String n = arg[0].tok[0].getTokenName();
   			 arg[0].tok[0].setTokenName(n + kernelConstants.renameId);
   		 }
          props.setProperty("inside", "");
   		 super.reduce2java(props);
			 props.removeProperty("inside");
	    }
		 else {
		    // inside outer-most class; increment length of depth 
		    // which indicates the level of nesting, then decrement
          props.setProperty("inside", depth + " " );
		    super.reduce2java(props);
          props.setProperty("inside", depth);
		 }
	 }
}
