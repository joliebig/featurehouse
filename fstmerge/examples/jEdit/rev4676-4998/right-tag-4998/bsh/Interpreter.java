

package bsh;

import java.util.Vector;
import java.io.*;


public class Interpreter 
	implements Runnable, ConsoleInterface,Serializable
{
	

	public static final String VERSION = "1.2.7";
	
    public static boolean DEBUG, TRACE;

	
    transient static PrintStream debug;
	static { 
		staticInit();
	}

	
	static This systemObject;

	
	public static boolean strictJava = false;

	

	

	transient Parser parser;
    NameSpace globalNameSpace;
    transient Reader in;
    transient PrintStream out;
    transient PrintStream err;
    ConsoleInterface console; 

	
	Interpreter parent;

	
	String sourceFileInfo;

	
	public boolean noExitOnEOF;

    private boolean 
		evalOnly, 		
		interactive;	

	

	
    public Interpreter(
		Reader in, PrintStream out, PrintStream err, 
		boolean interactive, NameSpace namespace,
		Interpreter parent, String sourceFileInfo )
    {
		parser = new Parser( in );
		long t1=System.currentTimeMillis();
        this.in = in;
        this.out = out;
        this.err = err;
        this.interactive = interactive;
		debug = err;
		this.parent = parent;
		this.sourceFileInfo = sourceFileInfo;

		if ( namespace == null )
        	this.globalNameSpace = new NameSpace("global");
		else
			this.globalNameSpace = namespace;

		
		
		

		
		if ( ! ( getu("bsh") instanceof bsh.This ) )
			initRootSystemObject();

		if ( interactive )
			loadRCFiles();

		long t2=System.currentTimeMillis();
		if ( Interpreter.DEBUG ) 
			Interpreter.debug("Time to initialize interpreter: "+(t2-t1));
    }

    public Interpreter(
		Reader in, PrintStream out, PrintStream err, 
		boolean interactive, NameSpace namespace)
    {
		this( in, out, err, interactive, namespace, null, null );
	}

    public Interpreter(
		Reader in, PrintStream out, PrintStream err, boolean interactive)
    {
        this(in, out, err, interactive, null);
    }

	
    public Interpreter(ConsoleInterface console, NameSpace globalNameSpace) {

        this( console.getIn(), console.getOut(), console.getErr(), 
			true, globalNameSpace );

		setConsole( console );
    }

	
    public Interpreter(ConsoleInterface console) {
        this(console, null);
    }

	
    public Interpreter()
    {
		this( new StringReader(""), 
			System.out, System.err, false, null );
        evalOnly = true;
		setu( "bsh.evalOnly", new Primitive(true) );
    }

	

	
	public void setConsole( ConsoleInterface console ) {
		this.console = console;
		setu( "bsh.console", console );
		
		setOut( console.getOut() );
		setErr( console.getErr() );
		
	}

	private void initRootSystemObject() 
	{
		
		setu("bsh", new NameSpace( "Bsh Object" ).getThis( this ) );

		
		if ( systemObject == null )
			systemObject = new NameSpace( 
				"Bsh System Object" ).getThis( this );
		
		setu( "bsh.system", systemObject );

		
		This helpText = new NameSpace( 
			"Bsh Command Help Text" ).getThis( this );
		setu( "bsh.help", helpText );

		
		try {
			setu( "bsh.cwd", System.getProperty("user.dir") );
		} catch ( SecurityException e ) { 
			
			setu( "bsh.cwd", "." );
		}

		
		setu( "bsh.interactive", new Primitive(interactive) );
		
		setu( "bsh.evalOnly", new Primitive(evalOnly) );
	}

	
	public void setNameSpace( NameSpace globalNameSpace ) {
		this.globalNameSpace = globalNameSpace;
	}

	
	public NameSpace getNameSpace() {
		return globalNameSpace;
	}

	
    public static void main( String [] args ) 
	{
        if ( args.length > 0 ) {
			String filename = args[0];

			String [] bshArgs;
			if ( args.length > 1 ) {
				bshArgs = new String [ args.length -1 ];
				System.arraycopy( args, 1, bshArgs, 0, args.length-1 );
			} else
				bshArgs = new String [0];

            Interpreter interpreter = new Interpreter();
			interpreter.setu( "bsh.args", bshArgs );
			try {
				interpreter.source( filename, interpreter.globalNameSpace );
			} catch ( FileNotFoundException e ) {
				System.out.println("File not found: "+e);
			} catch ( TargetError e ) {
				System.out.println("Script threw exception: "+e);
				if ( e.inNativeCode() )
					e.printStackTrace( DEBUG, System.err );
			} catch ( EvalError e ) {
				System.out.println("Evaluation Error: "+e);
			} catch ( IOException e ) {
				System.out.println("I/O Error: "+e);
			}
        } else {
			
			
			InputStream src;
			if ( System.getProperty("os.name").startsWith("Windows") 
				&& System.getProperty("java.version").startsWith("1.1."))
			{
				src = new FilterInputStream(System.in) {
					public int available() throws IOException {
						return 0;
					}
				};
			}
			else
				src = System.in;

            Reader in = new CommandLineReader( new InputStreamReader(src));
            Interpreter interpreter = 
				new Interpreter( in, System.out, System.err, true );
        	interpreter.run();
        }
    }

	
    public void run() {
        if(evalOnly)
            throw new RuntimeException("bsh Interpreter: No stream");

        
        if ( interactive )
			try { 
				eval("printBanner();"); 
			} catch ( EvalError e ) {
				println(
					"BeanShell "+VERSION+" - by Pat Niemeyer (pat@pat.net)");
			}

        boolean eof = false;

		
		CallStack callstack = new CallStack();
		callstack.push( globalNameSpace );

        while(!eof)
        {
            try
            {
                
                System.out.flush();
                System.err.flush();
                Thread.yield();  
                if(interactive)
                    print("bsh % ");

                eof = Line();

                if(get_jjtree().nodeArity() > 0)  
                {
                    SimpleNode node = (SimpleNode)(get_jjtree().rootNode());

                    if(DEBUG)
                        node.dump(">");

                    Object ret = node.eval( callstack, this );
				
					
					if ( callstack.depth() > 1 )
						throw new InterpreterError(
							"Callstack growing: "+callstack);

                    if(ret instanceof ReturnControl)
                        ret = ((ReturnControl)ret).value;
                    if(ret != Primitive.VOID)
                    {
                        setVariable("$_", ret);
                        Object show = getu("bsh.show");
                        if(show instanceof Boolean &&
                            ((Boolean)show).booleanValue() == true)
                            println("<" + ret + ">");
                    }
                }
            }
            catch(ParseException e)
            {
                error("Parser Error: " + e.getMessage(DEBUG));
				if ( DEBUG )
                	e.printStackTrace();
                if(!interactive)
                    eof = true;

                parser.reInitInput(in);
            }
            catch(InterpreterError e)
            {
                error("Internal Error: " + e.getMessage());
                e.printStackTrace();
                if(!interactive)
                    eof = true;
            }
            catch(TargetError e)
            {
                error("// Uncaught Exception: " + e );
				if ( e.inNativeCode() )
					e.printStackTrace( DEBUG, err );
                if(!interactive)
                    eof = true;
				setVariable("$_e", e.getTarget());
            }
            catch (EvalError e)
            {
				if ( interactive )
					error( e.toString() );
				else
					error( e.getMessage() );
                if(DEBUG)
                    e.printStackTrace();
                if(!interactive)
                    eof = true;
            }
            catch(Exception e)
            {
                error("Unknown error: " + e);
                e.printStackTrace();
                if(!interactive)
                    eof = true;
            }
            catch(TokenMgrError e)
            {
				error("Error parsing input: " + e);

				
				parser.reInitTokenInput( in );

                if(!interactive)
                    eof = true;
            }
            finally
            {
                get_jjtree().reset();
				
				if ( callstack.depth() > 1 ) {
					callstack.clear();
					callstack.push( globalNameSpace );
				}
            }
        }

		if ( interactive && !noExitOnEOF )
			System.exit(0);
    }

	

	
    public Object source( String filename, NameSpace nameSpace ) 
		throws FileNotFoundException, IOException, EvalError 
	{
		File file = pathToFile( filename );
		if ( Interpreter.DEBUG ) debug("Sourcing file: "+file);
		Reader sourceIn = new BufferedReader( new FileReader(file) );
		try {
			return eval( sourceIn, nameSpace, filename );
		} finally {
			sourceIn.close();
		}
	}

	
    public Object source( String filename ) 
		throws FileNotFoundException, IOException, EvalError 
	{
		return source( filename, globalNameSpace );
	}

    
	
	

    public Object eval( 
		Reader in, NameSpace nameSpace, String sourceFileInfo ) 
		throws EvalError 
	{
		Object retVal = null;
		if ( Interpreter.DEBUG ) debug("eval: nameSpace = "+nameSpace);

		
        Interpreter localInterpreter = 
			new Interpreter( 
				in, out, err, false, nameSpace, this, sourceFileInfo  );

		CallStack callstack = new CallStack();
		callstack.push( nameSpace );

        boolean eof = false;
        while(!eof)
        {
			SimpleNode node = null;
            try
            {
                eof = localInterpreter.Line();
                if (localInterpreter.get_jjtree().nodeArity() > 0)
                {
                    node = (SimpleNode)localInterpreter.get_jjtree().rootNode();
					
					node.setSourceFile( sourceFileInfo );

					if ( TRACE )
						println( "// " +node.getText() );

                    retVal = node.eval( callstack, localInterpreter );

					
					if ( callstack.depth() > 1 )
						throw new InterpreterError(
							"Callstack growing: "+callstack);

                    if ( retVal instanceof ReturnControl ) {
                        retVal = ((ReturnControl)retVal).value;
						break; 
					}
                }
            } catch(ParseException e) {
				
				if ( DEBUG )
					
					error( e.getMessage(DEBUG) );

				
				e.setErrorSourceFile( sourceFileInfo );
				throw e;

            } catch(InterpreterError e) {
                e.printStackTrace();
                throw new EvalError(
					"Sourced file: "+sourceFileInfo+" internal Error: " 
					+ e.getMessage(), node);
            } catch( TargetError e ) {
				
				if ( e.getNode()==null )
					e.setNode( node );
				e.reThrow("Sourced file: "+sourceFileInfo);
            } catch(EvalError e) {
                if(DEBUG)
                    e.printStackTrace();
				
				if ( e.getNode()==null )
					e.setNode( node );
				e.reThrow( "Sourced file: "+sourceFileInfo );
            } catch(Exception e) {
                e.printStackTrace();
                throw new EvalError(
					"Sourced file: "+sourceFileInfo+" unknown error: " 
					+ e.getMessage(), node);
            } catch(TokenMgrError e) {
                throw new EvalError(
					"Sourced file: "+sourceFileInfo+" Token Parsing Error: " 
					+ e.getMessage(), node );
            } finally {
                localInterpreter.get_jjtree().reset();

				
				if ( callstack.depth() > 1 ) {
					callstack.clear();
					callstack.push( nameSpace );
				}
            }
        }
		return Primitive.unwrap( retVal );
    }

	
    public Object eval( Reader in ) throws EvalError 
	{
		return eval( in, globalNameSpace, "eval stream" );
	}

	
    public Object eval( String statement ) throws EvalError {
		if ( Interpreter.DEBUG ) debug("eval(String): "+statement);
		return eval(statement, globalNameSpace);
	}

	
    public Object eval( String statement, NameSpace nameSpace ) 
		throws EvalError {

		String s = ( statement.endsWith(";") ? statement : statement+";" );
        return eval( 
			new StringReader(s), nameSpace, "<Inline eval of: "+s+" >" );
    }

	

	
    public final void error(String s) {
		if ( console != null )
				console.error( "// Error: " + s +"\n" );
		else {
			err.println("// Error: " + s);
			err.flush();
		}
    }

	
	
	
	

	
	public Reader getIn() { return in; }

	
	public PrintStream getOut() { return out; }

	
	public PrintStream getErr() { return err; }

    public final void println(String s)
    {
        print(s + "\n");
    }

    public final void print(String s)
    {
		if (console != null) {
            console.print(s);
        } else {
            out.print(s);
            out.flush();
        }
    }

	

	
    public final static void debug(String s)
    {
        if ( DEBUG )
            debug.println("// Debug: " + s);
    }

	

	
    public Object get( String name ) throws EvalError {
		Object ret = globalNameSpace.get( name, this );
		return Primitive.unwrap( ret );
	}

	
    Object getu( String name ) {
		try { 
			return get( name );
		} catch ( EvalError e ) { 
			throw new InterpreterError("set: "+e);
		}
	}

	
    public void set(String name, Object value) 
		throws EvalError 
	{
		
		if ( value == null )
			value = Primitive.NULL;

		CallStack callstack = new CallStack();
		LHS lhs = globalNameSpace.getNameResolver( name ).toLHS( 
			callstack, this );
		lhs.assign( value );
	}

	
    void setu(String name, Object value) {
		try { 
			set(name, value);
		} catch ( EvalError e ) { 
			throw new InterpreterError("set: "+e);
		}
	}

    public void set(String name, long value) throws EvalError {
        set(name, new Primitive(value));
	}
    public void set(String name, int value) throws EvalError {
        set(name, new Primitive(value));
	}
    public void set(String name, double value) throws EvalError {
        set(name, new Primitive(value));
	}
    public void set(String name, float value) throws EvalError {
        set(name, new Primitive(value));
	}
    public void set(String name, boolean value) throws EvalError {
        set(name, new Primitive(value));
	}

	
    public void unset( String name ) 
		throws EvalError 
	{
		CallStack callstack = new CallStack();
		LHS lhs = globalNameSpace.getNameResolver( name ).toLHS( 
			callstack, this );

		if ( lhs.type != LHS.VARIABLE )
			throw new EvalError("Can't unset, not a variable: "+name);

		
		lhs.assign( null );
	}


	
    public Object getVariable(String name)
    {
        Object obj = globalNameSpace.getVariable(name);
		return Primitive.unwrap( obj );
    }

	
    public void setVariable(String name, Object value)
    {
        try { globalNameSpace.setVariable(name, value); }
        catch(EvalError e) { error(e.toString()); }
    }

	
    public void setVariable(String name, int value)
    {
        try { globalNameSpace.setVariable(name, new Primitive(value)); }
        catch(EvalError e) { error(e.toString()); }
    }

	
    public void setVariable(String name, float value)
    {
        try { globalNameSpace.setVariable(name, new Primitive(value)); }
        catch(EvalError e) { error(e.toString()); }
    }

	
    public void setVariable(String name, boolean value)
    {
        try { globalNameSpace.setVariable(name, new Primitive(value)); }
        catch(EvalError e) { error(e.toString()); }
    }

	

	
	public Object getInterface( Class interf ) throws EvalError
	{
		return globalNameSpace.getThis( this ).getInterface( interf );
	}

	

	private JJTParserState get_jjtree() {
		return parser.jjtree;
	}

  	private ASCII_UCodeESC_CharStream get_jj_input_stream() {
		return parser.jj_input_stream;
	}

  	private boolean Line() throws ParseException {
		return parser.Line();
	}

	

	void loadRCFiles() {
		try {
			String rcfile = 
				
				System.getProperty("user.home") + File.separator + ".bshrc";
			source( rcfile, globalNameSpace );
		} catch ( Exception e ) { 
			
			if ( Interpreter.DEBUG ) debug("Could not find rc file: "+e);
		}
	}

	
    public File pathToFile( String fileName ) 
		throws IOException
	{
		File file = new File( fileName );

		
		if ( !file.isAbsolute() ) {
			String cwd = (String)getu("bsh.cwd");
			file = new File( cwd + File.separator + fileName );
		}

		return new File( file.getCanonicalPath() );
	}

	public static void redirectOutputToFile( String filename ) 
	{
		try {
			PrintStream pout = new PrintStream( 
				new FileOutputStream( filename ) );
			System.setOut( pout );
			System.setErr( pout );
		} catch ( IOException e ) {
			System.err.println("Can't redirect output to file: "+filename );
		}
	}

	
	public void setClassLoader( ClassLoader externalCL ) {
		BshClassManager.setClassLoader( externalCL );
	}

	static void staticInit() {
	
		try {
    		debug = System.err;
    		DEBUG = Boolean.getBoolean("debug");
    		TRACE = Boolean.getBoolean("trace");
			String outfilename = System.getProperty("outfile");
			if ( outfilename != null )
				redirectOutputToFile( outfilename );
		} catch ( SecurityException e ) { 
			System.err.println("Could not init static:"+e);
		} catch ( Exception e ) {
			System.err.println("Could not init static(2):"+e);
		} catch ( Throwable e ) { 
			System.err.println("Could not init static(3):"+e);
		}
	}

	
	public String getSourceFileInfo() { 
		if ( sourceFileInfo != null )
			return sourceFileInfo;
		else
			return "<unknown source>";
	}

	public Interpreter getParent() {
		return parent;
	}
	
	public void setOut( PrintStream out ) {
		this.out = out;
	}
	public void setErr( PrintStream out ) {
		this.err = err;
	}

	
	private void readObject(ObjectInputStream stream) 
		throws java.io.IOException, ClassNotFoundException
	{
		stream.defaultReadObject();

		
		if ( console != null ) {
			setOut( console.getOut() );
			setErr( console.getErr() );
		} else {
			setOut( System.out );
			setErr( System.err );
		}
	}

}

