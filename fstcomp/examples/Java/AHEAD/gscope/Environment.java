

import java.io.PrintWriter;

// Start of classes from CodeTemplate.Kernel

//**************************************************
// class Environment
//**************************************************
    
public class Environment {

    protected  Environment next;
    protected String mangle_string;
    protected  Environment parents;
    protected  EnvElem idents;
    protected boolean active;

    public Environment() {
        mangle_string = "_" + String.valueOf( kernelConstants.globals().global_mangle_num++ );
        parents = null;
        idents = null;
        next = null;
        active = true;
    }

    public boolean isActive() {
        return ( active );
    }

    public void setActive( boolean act ) {
        active = act;
    }

    // Locate the specified id on the list if it's there.
    protected  EnvElem localFindId( String id ) {
        EnvElem ptr;

        ptr = idents;
        while ( ptr != null ) {
            if ( id.compareTo( ptr.id ) == 0 )
                return ( ptr );
            ptr = ptr.next;
        }
        return ( null );
    }

    public  EnvElem findId( String id ) {
        EnvElem ptr;
        Environment eptr;

        if ( ! active )
            return ( null );
        ptr = localFindId( id );
        if ( ptr != null )
            return ( ptr );

        eptr = parents;
        while ( eptr != null ) {
            ptr = eptr.findId( id );
            if ( ptr != null )
                return ( ptr );
            eptr = eptr.next;
        }
        return ( null );
    }

    public String mangleNum() {
        return ( mangle_string );
    }

    public boolean addAlias( String id,  AstNode ast ) {
        EnvElem elem;

        elem = ( EnvElem ) findId( id );
        if ( elem == null )
            return ( false );
        elem._alias = ast;
        return ( true );
    }

    public  Environment addId( String id ) {
        idents = new  EnvElem( id, this, idents );
        return ( Environment ) this ;
    }

    public  Environment addEnv( Environment env ) {
        env.next = parents;
        parents = env;
        return ( Environment ) this ;
    }
}
