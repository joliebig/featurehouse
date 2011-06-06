


package org.gjt.sp.jedit.bsh;

import java.io.*;


public interface ConsoleInterface {
    public Reader getIn();
    public PrintStream getOut();
    public PrintStream getErr();
    public void println( Object o );
    public void print( Object o );
    public void error( Object o );
}

