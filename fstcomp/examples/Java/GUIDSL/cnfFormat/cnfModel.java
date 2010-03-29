import Jakarta.util.*;

class cnfModel {
    static cnfModel thisModel = null;

    String model;
    int    nvars;
    int    nclause;

    static cnfModel init() {
        //if ( thisModel == null )
            thisModel = new cnfModel();
        return thisModel;
    }

    cnfModel() {
        // Step 1: output formulas for productions, patterns, additional formulas
        //         and root=true

        cnfout out = new cnfout();
        try {
            production.toCnfFormat( out );
            pattern.toCnfFormat( out );
            ESList.toCnfFormat( out );
            grammar.toCnfFormat( out );
            out.close();
        }
        catch ( CNFException e ) {
            Util.fatalError( e.getMessage() );
        }

        // Step 2: now fill in the attributes of a cnfModel
        model = out.toString();
        nvars = variable.vtsize;
        nclause = out.getCnt();
    }

    // for debugging

    void print() {
        System.out.println( "# of variables " + nvars );
        System.out.println( "# of clauses   " + nclause );
        System.out.println( model );
    }
}
