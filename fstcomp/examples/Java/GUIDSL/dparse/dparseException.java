import java.lang.Exception;

public class dparseException extends Exception {

    public dparseException() {
        super( "dparse Error" );
    }

    public dparseException( String message ) {
        super( "dparse Error: " + message );
    }

    public dparseException( String message, Throwable cause ) {
        super( "dparse Error: " + message, cause );
    }

    public dparseException( Throwable cause ) {
        super( cause );
    }
}
