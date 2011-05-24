import java.lang.Exception;

public class SemanticException extends Exception {

    public SemanticException() {
        super("Semantic Error");
    }

    public SemanticException(String message) {
        super("Semantic Error: " + message);
    }

    public SemanticException(String message, Throwable cause) {
        super("Semantic Error: " + message, cause);
    }

    public SemanticException(Throwable cause) {
        super(cause);
    }
}
