//created on: Thu Nov 03 08:55:43 CST 2005

class variable {

    public static String findVar( int num ) {
        Iterator i = Vtable.values().iterator();
        while ( i.hasNext() ) {
            variable v = ( variable ) i.next();
            if ( v.number == num )
                return v.name;
        }
        return null;
    }

}