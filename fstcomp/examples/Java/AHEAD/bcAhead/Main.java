

public class Main {
    protected void cleanUp() {
        int nerrors =  AstNode.errorCount();
        if ( nerrors == 0 )
            return;
        AstNode.toolReport( "Summary " + nerrors + " error" + ( nerrors==1?"":"s" ) );
    }
}
