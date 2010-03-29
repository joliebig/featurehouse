import java.util.*;
import Jakarta.util.*;

public class variable {
    public static variable current; // most recently parsed variable
    public static HashMap Vtable; // current set of variables
    public static final int T=1; // true
    public static final int F=0; // false
    public static final int U=-1; // unknown

    public static int vtsize = 0; // size of Vtable

    public static final int Unkn=0; // unknown
    public static final int Sprd=1; // starting production
    public static final int Prod=2; // production
    public static final int Patt=3; // pattern
    public static final int Prim=4; // primitive

    public String getType() {
       if (type==Unkn) return "Unkn";
       if (type==Sprd) return "Sprd";
       if (type==Prod) return "Prod";
       if (type==Patt) return "Patt";
       if (type==Prim) return "Prim";
       Util.fatalError("unknown type for " + name + " is " + type);
       return null;
    } 

    public int     value;
    public String  name;
    public int     type;
    public gObj    gobj;
    public boolean redefine;   // can this be redefined?

    public static variable define( String name, int type, gObj g, boolean redefinable ) {
        if ( vtsize==0 )
            Vtable = new HashMap();
        variable v = ( variable ) Vtable.get( name );
        if (v != null) {
            if (v.redefine) {
               v.redefine = false;
               return v;
            }
            Util.error( "multiple definitions of "+ name );
            return v;
        }
        v = new variable();
        current = v;
        v.name = name;
        v.value = U;
        v.type = type;
        v.gobj = g;
        v.redefine = redefinable;
        Vtable.put( name, v );
        vtsize++;
        return v;
    }

    public void setValue( int v ) {
        value = v;
    }


    public void print() {
        System.out.print( " "+value+" = "+name+ " type=" + getType() + " " );
        if ( gobj == null )
            System.out.print( " gobj not set " );
        else
            if ( !name.equals( gobj.name ) )
                Util.error( "variable name != definition name" );
    }

    public void visit( GVisitor v ) {
        v.action( this );
    }

    public static void dumpVtable() {
        variable v;
        int cnt = 0;
        System.out.println( "-------Begin Vtable Dump----------" );
        Iterator i = Vtable.values().iterator();
        while ( i.hasNext() ) {
            v = ( variable ) i.next();
            v.print();
				System.out.println();
            cnt++;
        }
        System.out.println( cnt + " variables in all." );
        System.out.println( "-------End Vtable Dump----------" );
    }

    public void setgobj( gObj g ) {
        gobj = g;
    }
}
