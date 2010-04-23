

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.StringTokenizer;
import java.util.regex.Pattern;


import java.lang.String;

public class Arguments {

    public void setOutputMain(boolean t) {
	outputMain = t;
    }

    public void setFileInput(boolean t) {
	fileInput = t;
    }

    public void setStdInput(boolean t) {
	stdInput = t;
    }

    public void setStart (String s) {
	Start = s;
    }

    public void setClassFile (String s) {
  	classFile = s;
    }

    public void setLayer (String s) {
	Layer = s;
    }

    public void setBaliFile (String s) {
	baliFile = s;
    }

    public void print() {
	//print out all collected classes
	for(Iterator p = classes.iterator(); p.hasNext(); ) {
	    String name = (String) p.next() ;
	    System.out.println(name);
     	}
	System.out.println();
	System.out.println("outputMain = " + outputMain);
	System.out.println("fileInput = " + fileInput);
	System.out.println("stdInput = " + stdInput);
	
	System.out.println("Start = " + Start);
	System.out.println("Layer = " + Layer);
	System.out.println("classFile = " + classFile);
     	System.out.println("baliFile = " + baliFile);

	System.out.println();
    }

     /**
      * Given a method signature return the reference form, parameter type,
      * return type.
      */ 

    public void addMethod (String s) {

	MethodBuilder method = new MethodBuilder () ;
	String[] m = decompose(s, method);
	String returnType = m[0].trim();
	String name = m[1];
	String type = m[2];
	String parm = m[3];
	method.addModifier(Modifier.PUBLIC);
	method.setReturn (name, returnType) ;
	method.endLine();
	if(!returnType.equals("void"))
	    method.append("return ");
	method.append ("Super(" + type + ")." + name + "(" + parm + ");");
	Methods.add (method) ;

    }

     /**
      * Given a method signature and optional method object, return string array with:
		* [0] method type,  
		* [1] method name,
		* [2] formal parameter list,
		* [3] parameter call list
		* if method object present, parameters are added
      */ 
    public String[] decompose(String s, MethodBuilder method ) {
	    String[] result = new String[4];

	Matcher methodMatcher = methodPattern.matcher (s) ;
	if (! methodMatcher.matches ())
	    throw new IllegalArgumentException (
		"invalid method signature: "
		+ s
	    ) ;

	result[1] = methodMatcher.group (2);
	result[0] = methodMatcher.group (1);
	if(result[0]== null) result[0]="void";
   
	String arg = methodMatcher.group (3);
	String type = "";
	String parm = "";

	if(arg.length() > 0) {

	    Matcher argMatcher = argPattern.matcher (arg) ;
	    while (argMatcher.find ()) {
	        String t = argMatcher.group (1) ;
	        String p = argMatcher.group (2) ;
	        type += t + ",";
	        parm += p + ",";
	        if (method!=null)
			     method.addParameter (p, t) ;
	    }
	    type = type.substring(0, type.length()-1);
	    parm = parm.substring(0, parm.length()-1);
	}
	result[2] = type;
	result[3] = parm;

	return result;

    }

    /**
     * Collects class names from a class list file or a bali
     * grammar file
     */
    public void collectClass() throws Throwable {
	StringBuffer className = new StringBuffer();

	if(classFile != null) {
	    readFile(className,classFile);
	}else if (baliFile != null) {
	    File argSourceFile = parseSourceFile( baliFile );
	    Collector collector = collectSource( argSourceFile ) ;
            collector.collectClasses( className );
	    if( !grammarRefinement( baliFile ) ) {
                Start = collector.baliRules.getStartName();
            }
	}

        String s = className.toString();
	StringTokenizer st = new StringTokenizer(s);
	while ( st.hasMoreTokens() ) {
	    classes.add(st.nextToken());
	}           
    }

    /**
     * Read in class list file
     */
    private void readFile( StringBuffer buffer, String fileName ) {
        try {
            FileReader fr = new FileReader( fileName ) ;
            BufferedReader reader = new BufferedReader( fr ) ;
            char[] readBuffer = new char[1024] ;
            int count = reader.read( readBuffer, 0, readBuffer.length ) ;

            while ( count > -1 ) {
                if ( count > 0 )
                    buffer.append( readBuffer, 0, count ) ;
                count = reader.read( readBuffer, 0, readBuffer.length ) ;
            }
            fr.close();
        }
        catch ( IOException e ) {
            System.err.println( e.getMessage() );
        }
    }


    /**
     * Checks if a Bali file is a base grammar or a grammar extension
     * @return true if it's an extension
     *
     */
    private boolean grammarRefinement( String fileName ) {

        String line;
        try {
            BufferedReader infile = new BufferedReader( new FileReader( fileName ) );
            while( ( line = infile.readLine() ) != null ) {
                line = line.trim();
                if( line.startsWith( "/*" ) ) {
                    while( !line.endsWith( "*/" ) && line != null )
                        line = infile.readLine();
                    line = infile.readLine();
                    line = line.trim();
                }
                if( line.startsWith( "require" ) )
                    return true;
            }
        }
        catch ( IOException e ) {
            System.err.println( e.getMessage() );
        }
        return false;
    }

    private File parseSourceFile( String fileName ) {

        File file = new File( fileName ) ;

        if ( ! file.exists() )
            throw new IllegalArgumentException( "file doesn't exist: "
                            + fileName ) ;

        if ( ! file.canRead() )
            throw new IllegalArgumentException( "file can't be read: "
                            + fileName ) ;

        return file ;
    }

    /**
     * Processes an input {@link File} (with Bali source code) into a parse
     * tree, then running a {@link Collector} over the tree to gather
     * all necessary data for later code generation.
     *
     * @return {@link Collector} with collected data from parse tree.
     *
     */
    private  Collector collectSource( File inpFile )
    throws IOException, ParseException {
        Reader reader = new BufferedReader( new InputStreamReader( new FileInputStream( inpFile ),
                    "ISO-8859-1" ) ) ;

        Parser parser =  Parser.getInstance( reader ) ;
        BaliParse tree = ( BaliParse ) parser.parseAll () ;
        reader.close() ;

        Collector collector = new  Collector() ;
        collector.dispatch( tree ) ;
           
        return collector ;
    }

    public List getClasses() {
	return classes;
    }

    public boolean doesOutputMain() {
	return outputMain;
    }

    public boolean doesFileInput() {
	return fileInput;
    }

    public boolean doesStdInput () {
	return stdInput;
    }

    public String getStart() {
	return Start;
    }

    public String getLayer() {
	return Layer;
    }

    public String getClassFile() {
	return classFile;
    }

    public String getBaliFile() {
	return baliFile;
    }

    public List getMethods() {
	return Methods;
    }
    
    private List classes = new ArrayList();
    
    private boolean outputMain     = false;  //output a Main class
    private boolean fileInput      = false;  //Main takes a file as input
    private boolean stdInput       = false;  //Main gets input from stdin 

    private String Start = null ;
    private String Layer = null ;
    private String classFile = null ;
    private String baliFile = null ;
    	
    private List Methods = new ArrayList();    

    final private static Pattern argPattern ;
    final private static Pattern methodPattern ;
    static {
	String nameRex = "[$_a-zA-Z][$_a-zA-Z0-9]*" ;
	String typeRex = nameRex + "(?:[.]" + nameRex + ")*" ;
	String argRex = typeRex + "\\s+" + nameRex ;
	String argsRex = "(?:" + argRex + "(?:\\s*[,]\\s*" + argRex + ")*)?" ;

	argPattern = Pattern.compile (
	    "\\s*"
	    + "(" + typeRex + ")"
	    + "\\s*"
	    + "(" + nameRex + ")"
	    + "\\s*(?:[,])?\\s*"
	) ;

	methodPattern = Pattern.compile (
	    "\\s*"
	    + "(" + typeRex + "\\s+)?"
	    + "\\s*"
	    + "(" + nameRex + ")"
	    + "\\s*[(]\\s*"
	    + "(" + argsRex + ")"
	    + "\\s*[)]\\s*"
	) ;

    }
}
