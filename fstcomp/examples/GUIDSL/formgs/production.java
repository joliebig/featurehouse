import java.util.*;
import Jakarta.util.*;

class production {
    node formula;

    static void makeFormula() {
        Iterator i = Ptable.values().iterator();
        while ( i.hasNext() ) {
            production p = ( production ) i.next();
            p.formula = p.makef();
        }
    }

    public node makef() {
        node n = null;
        node o = null;
		  int cnt = 0;
        Iterator i = pat.iterator();
        while ( i.hasNext() ) {
            pattern t = ( pattern ) i.next();
				cnt++;
            switch ( type )
            {
                case production.norm: /* choose 1 */
                case production.opt: /* treat like choose 1 */
                if ( o == null )
                    o = new bterm( t.name );
                else
                    o = new or( new bterm( t.name ), o );

                if ( n == null )
                    n = new bterm( t.name );
                else
                    n = new atmostone( new bterm( t.name ), n );
                break;
                case production.star: /* choose any number - treat like star */
                case production.plus:
                if ( n == null )
                    n = new bterm( t.name );
                else
                    n = new or( new bterm( t.name ), n );
                break;
                default:
                Util.error( "production " + name + " is not referenced in grammar" );
                return new bterm( t.name ); // return some dummy 
         }
        }
        switch ( type )
        {
            case production.norm:
            case production.opt:
				if (cnt>1) {
               // production iff (or of options) AND atmostone( list of options)
               return new and( new iff( new bterm( name ), o ), n ); 
				}
				else {
				   // production (prodname iff onlyPattern)
					// the atmostone(x) is always true
               return new iff( new bterm( name ), o );
				}
            case production.star:
            case production.plus:
            //  production iff (predicate associated with patterns)
            return new iff( new bterm( name ), n );
            default:
            Util.fatalError( "unrecognizable type: " + type +
                             " for production " + name );
        }
        // should never get here
        Util.fatalError( "should never get here" );
        return null;
    }

    public void print() {
        original();
        System.out.print( " formula = " + formula );
    }
}
