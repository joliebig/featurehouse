

public class ModOverrides {

    public void reduce2java( AstProperties props ) {
	     // just print the white-space; reduction is simply erasure
        props.print( getComment() );
    }
}
