import java.io.*;

class cnfout {
    static boolean debug = true;

    StringWriter sw; // the string buffer in which data is written
    PrintWriter pw; // output "file", which hides the StringWriter
    int         cnt; // # of clauses -- this is an incrementing counter
    boolean     andSeen; // special case needed for counting

    cnfout() {
        sw = new StringWriter();
        pw = new PrintWriter( sw );
        cnt = 0;
        andSeen = false;
    }

    void print( String x ) {
        pw.print( x );
    }
    void println( String x ) {
        pw.println( x );
    }
    void println() {
        pw.println();
    }

    void inc() {
        cnt++;
    }

    // invoke only after endFormula() is invoked

    int getCnt() {
        return cnt;
    }

    void andSeen() {
        andSeen = true;
    }

    void beginFormula( node n ) {
        comment( n );
    }

    void beginFormula( String s ) {
        comment( s );
    }

    void cnfBeginFormula( String s ) {
       cnfcomment(s);
    }

    void endFormula() {
        if ( !andSeen ) {
            pw.println( " 0" );
            inc();
        }
        andSeen = false;
    }

    void newLine( String s ) {
        pw.println( s );
    }

    void comment( String s ) {
        if ( debug )
            pw.println( "c " + s );
    }

    void comment( node n ) {
        comment( ""+n );
    }

    void cnfcomment( node n ) {
         cnfcomment(""+n);
    }

    void cnfcomment( String x ) {
       if (debug) 
         pw.println("c x " + x);
    }


    void append( String s ) {
        pw.print( s );
    }

    void close() {
        pw.close();
    }

    public String toString() {
        return sw.toString();
    }
}
