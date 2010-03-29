// this is the parser of a model debugging file
// the format of a debugging file is
// debugFile  : (action)* ;
// action : test id-list #END 
//        | #FILE <filename>
//        ;
// test   : #TRUE
//        | #FALSE
//        | #INCOMPLETE
//        | #COMPLETE
//        ;
// id-list: (IDENTIFIER)+ ;

import java.io.*;
import java.util.*;
import Jakarta.util.*;

class dparser {
    static final String fileToken;
    static final int fileTokenLength;

    static final String falseToken;
    static final int falseTokenLength;

    static final String trueToken;
    static final int trueTokenLength;

    static final String endToken;
    static final int endTokenLength;

    static final String incompleteToken;
    static final int incompleteTokenLength;

    static final String completeToken;
    static final int completeTokenLength;

    static final String comment;

    static {
       fileToken = "#file";
       fileTokenLength = fileToken.length();

       falseToken = "#false";
       falseTokenLength = falseToken.length();

       trueToken = "#true";
       trueTokenLength = trueToken.length();

       endToken = "#end";
       endTokenLength = endToken.length();

       incompleteToken = "#incomplete";
       incompleteTokenLength = incompleteToken.length();

       completeToken = "#complete";
       completeTokenLength = completeToken.length();

       comment = "#";
    }

    Stack  stack; // stack of filenames to process
    String filename; // name current file being processed
    int    lineno; // line# in current file
    LineNumberReader l; // handle to file being processed

    dparser( String fname ) throws dparseException {
        stack    = new Stack();
        filename = fname;
        lineno   = 0;
        try {
            l = new LineNumberReader( new FileReader( filename ) );
        }
        catch ( Exception e ) {
            throw new dparseException( e.getMessage() );
        }
    }

    // returns the next SATtest, else null (when at end of debug files)
    //
    public SATtest getNextTest() throws dparseException {
        String tmpstr;
        String s;
        SATtest current = null;
        try {
            while ( true ) {
                // Step 1: read in line
                        //
                lineno = l.getLineNumber();
                s      = l.readLine();

                // Step 2: if at end of file, then process the next file on stack
                //         else, return null as processing is finished.
                if ( s == null ) {
                    l.close();
                    if ( stack.empty() )
                        return null;
                    filename = ( String ) stack.pop();
                    l = new LineNumberReader( new FileReader( filename ) );
                    continue;
                }
                // In the following, we'll define actions per line of
                // a debug file.
                //
                // Step 3: process fileToken if present
                //
                if ( s.startsWith( fileToken ) ) {
                    // fileToken cannot appear inside a test-end pair
                    //
                    if ( current != null ) {
                        throw new dparseException( "Parse error at " + filename + ":" + 
								          lineno + " " + current.name + " not ended properly. " +
                                  fileToken + " appears inside its definition" );
                    }
                    // else push name on file stack for later processing
                    //
                    stack.push( ( s.substring( fileTokenLength ) ).trim() );
                    continue;
                }

                // Step 4: process true/false/complete/incomplete statement if present
                //
                if ( s.startsWith( trueToken )     || s.startsWith( falseToken ) || 
					      s.startsWith( completeToken ) || s.startsWith( incompleteToken ) ) {
                    // if we are inside a t/f/c/i statement (i.e. a SAT test)
                    // then this is an error
                    //
                    if ( current == null ) {
                        if ( s.startsWith( trueToken ) ) {
                            tmpstr = ( s.substring( trueTokenLength ) ).trim();
                            current = new SATtest( tmpstr, true, false );
                        }
                        if ( s.startsWith( falseToken ) ) {
                            tmpstr = ( s.substring( falseTokenLength ) ).trim();
                            current = new SATtest( tmpstr, false, false );
                        }
								if ( s.startsWith( completeToken ) ) {
                            tmpstr = ( s.substring( completeTokenLength ) ).trim();
                            current = new SATtest( tmpstr, true, true );
                        }
								if ( s.startsWith( incompleteToken ) ) {
                            tmpstr = ( s.substring( incompleteTokenLength ) ).trim();
                            current = new SATtest( tmpstr, false, true );
                        }
                    }
                    else {
                        throw new dparseException( "Parse error at " + filename + ":" + 
								      lineno + " " + current.name + " not ended properly." );
                    }
                    continue;
                }
                // Step 5: process end token
                //
                if ( s.startsWith( endToken ) ) {
                    // if an end token appears without a prior true/false
                    // token, this is an error
                    //
                    if ( current == null ) {
                        throw new dparseException( "Parse error at " + filename + ":" + 
								      lineno + " unmatched " + endToken );
                    }
                    // if not, then we've finished reading a SAT test
                    //
                    else {
                        return current;
                    }
                }
                // Step 6: ignore comments (lines beginning with #)
                //
                if ( s.startsWith( comment ) )
                    continue;
                // Step 7: if we're reading a SAT test, parse identifiers
                //         and add to test 
                //
                if ( current != null ) {
                    tokenizeLine( s, current );
                    continue;
                }
                // Step 8: otherwise, only blank lines are ignored. We'll
                //         complain otherwise
                //
                if ( ! ( s.trim().equals( "" ) ) )
                      throw new dparseException( "Ignoring " + filename + 
							 ":" + lineno + " " + s );
            }
        }
        catch ( Exception e ) {
            throw new dparseException( e.getMessage() );
        }
    }

    // method takes a line and adds its tokens (feature names) to the
    // provided SATtest
    //
    void tokenizeLine( String line, SATtest t ) throws dparseException {
        StringTokenizer st = new StringTokenizer( line );
        while ( st.hasMoreTokens() ) {
            String tok = st.nextToken();
            if ( tok.equals( "-" ) ) {
                throw new dparseException( "minus (-) cannot be a " +
                     " separate token at " + filename + ":" + lineno );
            }
				if (t.isComplete() && tok.startsWith("-"))
               throw new dparseException( "minus (-) cannot be used " +
                     " in a complete test at " + filename + ":" + lineno );
            t.add( tok );
        }
    }
}
