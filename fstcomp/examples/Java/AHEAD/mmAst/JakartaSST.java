

// the following classes are examples of rewrites that
// note boundaries between quoted text and executable text.
// These rewrites are necessary only if both the basemm layer
// and the ast layer are present; they are absent otherwise.

public class JakartaSST {
    public void execute( int stage ) {
        super.execute( stage+1 );
    }
}
