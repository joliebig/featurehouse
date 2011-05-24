

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class MMOutput extends HashMap {

    protected String name; // name of unit
    protected String type; // "C" for class, "I" for interface,
    protected String defn; // "D" for defined, "E" for extends, "R" for refines
    MMHashMap nested = new  MMHashMap();
    // for hierarchies of abstractions.

    int    firstline; // starting line number
    int    lastline; // ending line number

	 protected int     modifiers;   // see MMGlobals for options

    public MMOutput() {
        name = "";
        type = "";
        defn = "";
        firstline = -1;
        lastline  = -1;
		  modifiers = 0;
    }

    public  MMOutput init( String name, String type, String defn ) {
        this.name = name;
        this.type = type;
        this.defn = defn;
        return ( MMOutput ) this;
    }

    // equals needed by HashMap

    public boolean equals( Object o ) {
        if ( o instanceof  MMOutput )
            return ( ( MMOutput ) o ).name.equals( name );
        return false;
    }

    public void setlines( int start, int end ) {
        firstline = start;
        lastline  = end;
    }
    public int getStartLine() {
        return firstline;
    }
    public int getLastLine() {
        return lastline;
    }

    // set methods for name, type, defn assign values the first time.

    public String getName() {
        return name;
    }
    public void setName( String x ) {
        if ( name.equals( "" ) )
            name = Util2.unmangleId( x );
    }

    public String getType() {
        return type;
    }
    public void setType( String x ) {
        if ( type.equals( "" ) )
            type = x;
    }

    public String getDefn() {
        return defn;
    }
    public void setDefn( String x ) {
        if ( defn.equals( "" ) )
            defn = x;
    }

    public MMHashMap getNested() {
        return nested;
    }

    // get methods for modifiers

	 public int getModifiers() {
	    return modifiers;
	 }

    public void copyModifiers( MMOutput o ) {
	    modifiers = o.modifiers;
	 }

    public void setModifiers( AstOptNode n ) {

	    // Step 1: if there are no modifiers, return

	    if (n.arg[0] == null) return;

		 // Step 2: iterate over list of modifiers and set booleans

       modifiers = 0;
		 AstCursor c = new AstCursor();
		 for (c.FirstElement(n.arg[0]); c.MoreElement(); c.NextElement()) {
		    if (c.node instanceof ModAbstract)  {
			    modifiers |= MMGlobals.ModAbstract;
			 }
		    else if (c.node instanceof ModFinal) {
			    modifiers |= MMGlobals.ModFinal;
			 }
			 else if (c.node instanceof ModPublic) {
			    modifiers |= MMGlobals.ModPublic;
			 }
			 else if (c.node instanceof ModProtected) {
			    modifiers |= MMGlobals.ModProtected;
			 }
			 else if (c.node instanceof ModPrivate) {
			    modifiers |= MMGlobals.ModPrivate;
			 }
			 else if (c.node instanceof ModStatic) {
			    modifiers |= MMGlobals.ModStatic;
			 }
			 else if (c.node instanceof ModTransient) {
			    modifiers |= MMGlobals.ModTransient;
			 }
			 else if (c.node instanceof ModVolatile) {
			    modifiers |= MMGlobals.ModVolatile;
			 }
			 else if (c.node instanceof ModNative) {
			    modifiers |= MMGlobals.ModNative;
			 }
			 else if (c.node instanceof ModSynchronized) {
			    modifiers |= MMGlobals.ModSynchronized;
			 }
		 }
    }

    // ----------------------

    public void print() {
        print( "" );
    }

    protected void print( String indent ) {
        String nametype = "";
        if (type == MMGlobals.Constructor) nametype = "__";

        System.out.println( indent + type + " " + defn + " " + name  + " " + nametype);
        System.out.println( indent + "Line range (" + firstline + ", " 
                             + lastline + ")" );

        System.out.print( indent );
		  if ((modifiers & MMGlobals.ModAbstract) != 0)
		     System.out.print( "abstract " );
		  if ((modifiers & MMGlobals.ModFinal) != 0)
		     System.out.print( "final " );
		  if ((modifiers & MMGlobals.ModPublic) != 0)
		     System.out.print( "public " );
		  if ((modifiers & MMGlobals.ModProtected) != 0)
		     System.out.print( "protected " );
		  if ((modifiers & MMGlobals.ModPrivate) != 0)
		     System.out.print( "private " );
		  if ((modifiers & MMGlobals.ModStatic) != 0)
		     System.out.print( "static " );
		  if ((modifiers & MMGlobals.ModTransient) != 0)
		     System.out.print( "transient " );
		  if ((modifiers & MMGlobals.ModVolatile) != 0)
		     System.out.print( "volatile " );
		  if ((modifiers & MMGlobals.ModNative) != 0)
		     System.out.print( "native " );
		  if ((modifiers & MMGlobals.ModSynchronized) != 0)
		     System.out.print( "synchronized " );
        System.out.println(" endMods ");

        List keys = new ArrayList( keySet() ) ;

        if (keys.size() != 0) {
           Collections.sort( keys ) ;

           // print out all collected named vectors (e.g., super, extends, implements)

           System.out.println(indent + " beginKeys");
           for ( Iterator p = keys.iterator() ; p.hasNext() ; )
               ( ( NamedVector ) get( p.next() ) ) . print( indent ) ;
           System.out.println(indent + " endKeys");
        }

        // now print out all objects nested inside this construct

        if ( nested != null && nested.size() != 0) {
            System.out.println(indent + " beginNest");
            nested.print( indent +  MMGlobals.INDENT ) ;
            System.out.println(indent + " endNest");
	}
    }

    public void union( NamedVector v ) {

        // the idea here is that when a named vector is to be added,
        // it should have a unique name.  If a vector already exists
        // with this name, its contents are added to the existing vector.
        // this wierdness is imposed upon us by mixin-produced files.
        // in particular, interfaces.
 
        if ( v.isEmpty() )
            return ;

        NamedVector previous = ( NamedVector ) get( v.getName() ) ;
        if ( previous == null ) {
            put( v.getName(), v ) ;
            return ;
        }

        previous.addAll( v ) ;
    }

    // has a result been computed?

    public boolean computed() {
        return !type.equals( "" );
    }

	 public void merge( MMOutput o ) {
	    // this method is called to merge method declarations in mixin-produced
		 // files.  The throws clauses are unioned, and so too are the modifiers.

		 for ( Iterator p = o.keySet().iterator() ; p.hasNext() ; ) {
		     String key = (String) p.next();
		     NamedVector nv = (NamedVector) o.get(key);
			  if (nv != null)  {
			     union( nv );
			  }
		 }

		 modifiers |= o.modifiers;
	}
}
