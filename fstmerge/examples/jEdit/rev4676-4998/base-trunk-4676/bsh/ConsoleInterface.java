


package bsh;

import java.io.*;


public interface ConsoleInterface {
	public Reader getIn();
	public PrintStream getOut();
	public PrintStream getErr();
	public void println( String s );
	public void print( String s );
	public void error( String s );
}

