

//------------- extracting parameters from parameter list -----
//------------- this really is a common utility and should be
//------------- placed in its own layer for others to use
//------------- originally written for Unpack tool

public class AST_ParList {

    public String onlyParams() {
        AstCursor c = new  AstCursor();
        String result = "";

        for ( c.FirstElement( this ); c.MoreElement(); c.NextElement() ) {
            // here's what I'm taking advantage of -- I know
            // a formal parameter will always be a FormParDecl production
            // and that a VariableDeclaratorId will always be a
            // DecNameDim production.  To generalize, it is necessary
            // to push a set of abstract methods through a set of classes...

            FormParDecl fpd = ( FormParDecl ) c.node;
            DecNameDim  dnd = ( DecNameDim ) fpd.arg[1];

            String varname = dnd.arg[0].tok[0].tokenName();
            if ( result.equals( "" ) )
                result = varname;
            else
                result = result + ", " + varname;
        }
        return result;
    }
}
