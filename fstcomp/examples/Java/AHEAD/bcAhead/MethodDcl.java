

public class MethodDcl {
    public void reduce2java( AstProperties props ) {
         
        // Step 1: see if the method is static or not.  Indicate so
        //         by adding (or removing) a property

        boolean isStatic = false;
        if ( arg[0].arg[0] != null ) {
            AstCursor c = new  AstCursor();
            for ( c.FirstElement( arg[0].arg[0] ); c.MoreElement(); c.NextElement() ) {
                if ( c.node instanceof  ModStatic ) {
                    isStatic = true;
                    break;
                }
            }
        }

        props.removeProperty( "isStatic" );
        if ( isStatic )
            props.setProperty( "isStatic", "isStatic" );
        
        // Step 2: reduce normally, and afterward, remove property

        super.reduce2java( props );
        props.removeProperty( "isStatic" );
    }
}
