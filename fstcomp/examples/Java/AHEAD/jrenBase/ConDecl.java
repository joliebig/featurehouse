

public class ConDecl     {

    public void reduce2java( AstProperties props ) {

	    // Step 1: get depth of nesting

	    String depth = (String) props.getProperty( "inside" );

       // Step 2: rewrite only at level 0 -- ignore all other levels
       if (arg[1] instanceof NameId && (depth.length() == 0) ) {
		    String n = arg[1].tok[0].getTokenName();
			 arg[1].tok[0].setTokenName(n + kernelConstants.renameId);
		 }
		 super.reduce2java(props);
	 }
}
