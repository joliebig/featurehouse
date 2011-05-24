

// the following classes are examples of rewrites that
// note boundaries between quoted text and executable text.
// These rewrites are necessary only if both the unmixinbase layer
// and the ast layer are present; they are absent otherwise.

public class JakartaSST {
    public void unmangleIds( int stage ) {
        super.unmangleIds( stage+1 );
    }
}
