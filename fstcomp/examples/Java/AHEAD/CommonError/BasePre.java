

public class BasePre {

    public void checkForErrors( int stage, String file ) {

        if ( stage != 0 ) {
            super.checkForErrors( stage, file );
            return;
        }

        MethodDcl.seenSUPER= true;
        if (!Ute.withinRefines)
           AstNode.warning(tok[0],
              "cannot use Super outside of a refines type declaration -- "
              + "use 'super' instead");
    }
}
