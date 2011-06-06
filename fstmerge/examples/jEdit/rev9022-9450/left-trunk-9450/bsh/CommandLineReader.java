


package bsh;

import java.io.*;


class CommandLineReader extends FilterReader {

    public CommandLineReader( Reader in ) {
		super(in);
    }

	static final int 
		normal = 0,
		lastCharNL = 1,
		sentSemi = 2;

	int state = lastCharNL;

    public int read() throws IOException {
		int b;

		if ( state == sentSemi ) {
			state = lastCharNL;
			return '\n';
		}

		
        while ( (b = in.read()) == '\r' );

		if ( b == '\n' )
			if ( state == lastCharNL ) {
				b = ';';
				state = sentSemi;
			} else
				state = lastCharNL;
		else
			state = normal;

		return b;
    }

	
    public int read(char buff[], int off, int len) throws IOException 
	{
		int b = read();
		if ( b == -1 )
			return -1;  
		else {
			buff[off]=(char)b;
			return 1;
		}
    }

	
	public static void main( String [] args ) throws Exception {
		Reader in = new CommandLineReader( new InputStreamReader(System.in) );
		while ( true )
			System.out.println( in.read() );
		
	}
}

