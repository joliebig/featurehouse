public class CNFException extends Exception {

    public CNFException() {
        super( "CNF Translation Error" );
    }

    public CNFException( String message ) {
        super( "CNF Translation Error: " + message );
    }

    public CNFException( String message, Throwable cause ) {
        super( "CNF Translation Error: " + message, cause );
    }

    public CNFException( Throwable cause ) {
        super( cause );
    }
}
