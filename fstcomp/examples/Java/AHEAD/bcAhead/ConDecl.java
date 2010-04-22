

public class ConDecl {

    public void reduce2java( AstProperties props ) {
	    props.setProperty( "insideConstructor", "" );
		 super.reduce2java(props);
		 props.removeProperty( "insideConstructor" );
	 }

    public void changeNameTo( String name ) {
        arg[1].tok[0].setTokenName( name );
    }
}
