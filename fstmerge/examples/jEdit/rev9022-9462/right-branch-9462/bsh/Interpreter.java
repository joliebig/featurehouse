

package bsh;

import java.util.Vector;
import java.io.*;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;


public class Interpreter 
	implements Runnable, ConsoleInterface,Serializable
{
	

	public static final String VERSION = "2.0b4-jedit";
	
    public static boolean DEBUG, TRACE, LOCALSCOPING;

	
    transient static PrintStream debug;
	static String systemLineSeparator = "\n"; 

	static {
		staticInit();
	}

	
	static This sharedObject;

	
	private boolean strictJava = false;

	

	

	transient Parser parser;
    NameSpace globalNameSpace;
    transient Reader in;
    transient PrintStream out;
    transient PrintStream err;
    ConsoleInterface console;

	
	Interpreter parent;

	
	String sourceFileInfo;

	
	private boolean exitOnEOF = true;

    protected boolean
		evalOnly, 		
		interactive;	

	
	private boolean showResults;

	

	
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
		if ( parent != null )
			setStrictJava( parent.getStrictJava() );
		this.sourceFileInfo = sourceFileInfo;

		BshClassManager bcm = BshClassManager.createClassManager( this );
		if ( namespace == null )
        	this.globalNameSpace = new NameSpace( bcm, "global");
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
		BshClassManager bcm = getClassManager();
		
		setu("bsh", new NameSpace( bcm, "Bsh Object" ).getThis( this ) );

		
		if ( sharedObject == null )
			sharedObject = new NameSpace(
				bcm, "Bsh Shared System Object" ).getThis( this );
		
		setu( "bsh.system", sharedObject );
		setu( "bsh.shared", sharedObject ); 

		
		This helpText = new NameSpace(
			bcm, "Bsh Command Help Text" ).getThis( this );
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
				Object result =
					interpreter.source( filename, interpreter.globalNameSpace );
				if ( result instanceof Class )
					try {
						invokeMain( (Class)result, bshArgs );
					} catch ( Exception e )
					{
						Object o = e;
						if ( e instanceof InvocationTargetException )
							o = ((InvocationTargetException)e)
								.getTargetException();
						System.err.println(
							"Class: "+result+" main method threw exception:"+o);
					}
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
        } else
		{
			
			
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

	public static void invokeMain( Class clas, String [] args )
		throws Exception
	{
    	Method main = Reflect.resolveJavaMethod(
			null, clas, "main",
			new Class [] { String [].class }, true );
		if ( main != null )
			main.invoke( null, new Object [] { args } );
	}

	
    public void run()
	{
        if(evalOnly)
            throw new RuntimeException("bsh Interpreter: No stream");

        
        if ( interactive )
			try {
				eval("printBanner();");
			} catch ( EvalError e ) {
				println(
					"BeanShell "+VERSION+" - by Pat Niemeyer (pat@pat.net)");
			}

		
		CallStack callstack = new CallStack( globalNameSpace );

        boolean eof = false;
        while( !eof )
        {
            try
            {
                
                System.out.flush();
                System.err.flush();
                Thread.yield();  

                if ( interactive )
                    print( getBshPrompt() );

                eof = Line();

                if( get_jjtree().nodeArity() > 0 )  
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

                    if( ret != Primitive.VOID )
                    {
                        setu("$_", ret);
						if ( showResults )
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
				setu("$_e", e.getTarget());
            }
            catch (EvalError e)
            {
				if ( interactive )
					error( "EvalError: "+e.toString() );
				else
					error( "EvalError: "+e.getMessage() );

                if(DEBUG)
                    e.printStackTrace();

                if(!interactive)
                    eof = true;
            }
            catch(Exception e)
            {
                error("Unknown error: " + e);
				if ( DEBUG )
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

		if ( interactive && exitOnEOF )
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
		Reader in, NameSpace nameSpace, String sourceFileInfo
			 )
		throws EvalError
	{
		Object retVal = null;
		if ( Interpreter.DEBUG ) debug("eval: nameSpace = "+nameSpace);

		
        Interpreter localInterpreter =
			new Interpreter(
				in, out, err, false, nameSpace, this, sourceFileInfo  );

		CallStack callstack = new CallStack( nameSpace );

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

					if ( localInterpreter.showResults
						&& retVal != Primitive.VOID )
						println("<" + retVal + ">");
                }
            } catch(ParseException e) {
				
				if ( DEBUG )
					
					error( e.getMessage(DEBUG) );

				
				e.setErrorSourceFile( sourceFileInfo );
				throw e;

            } catch ( InterpreterError e ) {
                e.printStackTrace();
                throw new EvalError(
					"Sourced file: "+sourceFileInfo+" internal Error: "
					+ e.getMessage(), node, callstack);
            } catch ( TargetError e ) {
				
				if ( e.getNode()==null )
					e.setNode( node );
				e.reThrow("Sourced file: "+sourceFileInfo);
            } catch ( EvalError e) {
                if ( DEBUG)
                    e.printStackTrace();
				
				if ( e.getNode()==null )
					e.setNode( node );
				e.reThrow( "Sourced file: "+sourceFileInfo );
            } catch ( Exception e) {
                if ( DEBUG)
                	e.printStackTrace();
                throw new EvalError(
					"Sourced file: "+sourceFileInfo+" unknown error: "
					+ e.getMessage(), node, callstack);
            } catch(TokenMgrError e) {
                throw new EvalError(
					"Sourced file: "+sourceFileInfo+" Token Parsing Error: "
					+ e.getMessage(), node, callstack );
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

	
    public Object eval( String statements ) throws EvalError {
		if ( Interpreter.DEBUG ) debug("eval(String): "+statements);
		return eval(statements, globalNameSpace);
	}

	
    public Object eval( String statements, NameSpace nameSpace )
		throws EvalError
	{

		String s = ( statements.endsWith(";") ? statements : statements+";" );
        return eval(
			new StringReader(s), nameSpace,
			"inline evaluation of: ``"+ showEvalString(s)+"''" );
    }

	private String showEvalString( String s ) {
		s = s.replace('\n', ' ');
		s = s.replace('\r', ' ');
		if ( s.length() > 80 )
			s = s.substring( 0, 80 ) + " . . . ";
		return s;
	}

	

	
    public final void error( Object o ) {
		if ( console != null )
				console.error( "// Error: " + o +"\n" );
		else {
			err.println("// Error: " + o );
			err.flush();
		}
    }

	
	
	
	

	
	public Reader getIn() { return in; }

	
	public PrintStream getOut() { return out; }

	
	public PrintStream getErr() { return err; }

    public final void println( Object o )
    {
        print( String.valueOf(o) + systemLineSeparator );
    }

    public final void print( Object o )
    {
		if (console != null) {
            console.print(o);
        } else {
            out.print(o);
            out.flush();
        }
    }

	

	
    public final static void debug(String s)
    {
        if ( DEBUG )
            debug.println("// Debug: " + s);
    }

	

	
    public Object get( String name ) throws EvalError {
		try {
			Object ret = globalNameSpace.get( name, this );
			return Primitive.unwrap( ret );
		} catch ( UtilEvalError e ) {
			throw e.toEvalError( SimpleNode.JAVACODE, new CallStack() );
		}
	}

	
    Object getu( String name ) {
		try {
			return get( name );
		} catch ( EvalError e ) {
			throw new InterpreterError("set: "+e);
		}
	}

	
    public void set( String name, Object value )
		throws EvalError
	{
		
		if ( value == null )
			value = Primitive.NULL;

		CallStack callstack = new CallStack();
		try {
			if ( Name.isCompound( name ) )
			{
				LHS lhs = globalNameSpace.getNameResolver( name ).toLHS(
					callstack, this );
				lhs.assign( value, false );
			} else 
				globalNameSpace.setVariable( name, value, false );
		} catch ( UtilEvalError e ) {
			throw e.toEvalError( SimpleNode.JAVACODE, callstack );
		}
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
		try {
			LHS lhs = globalNameSpace.getNameResolver( name ).toLHS(
				callstack, this );

			if ( lhs.type != LHS.VARIABLE )
				throw new EvalError("Can't unset, not a variable: "+name,
					SimpleNode.JAVACODE, new CallStack() );

			
			lhs.nameSpace.unsetVariable( name );
		} catch ( UtilEvalError e ) {
			throw new EvalError( e.getMessage(),
				SimpleNode.JAVACODE, new CallStack() );
		}
	}

	

	
	public Object getInterface( Class interf ) throws EvalError
	{
		try {
			return globalNameSpace.getThis( this ).getInterface( interf );
		} catch ( UtilEvalError e ) {
			throw e.toEvalError( SimpleNode.JAVACODE, new CallStack() );
		}
	}

	

	private JJTParserState get_jjtree() {
		return parser.jjtree;
	}

  	private JavaCharStream get_jj_input_stream() {
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
		getClassManager().setClassLoader( externalCL );
	}

	
	public BshClassManager getClassManager()
	{
		return getNameSpace().getClassManager();
	}

	
	public void setStrictJava( boolean b ) {
		this.strictJava = b;
	}

	
	public boolean getStrictJava() {
		return this.strictJava;
	}

	static void staticInit()
	{
	
		try {
			systemLineSeparator = System.getProperty("line.separator");
    		debug = System.err;
    		DEBUG = Boolean.getBoolean("debug");
    		TRACE = Boolean.getBoolean("trace");
    		LOCALSCOPING = Boolean.getBoolean("localscoping");
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
	public void setErr( PrintStream err ) {
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

	
	private String getBshPrompt()
	{
		try {
			return (String)eval("getBshPrompt()");
		} catch ( Exception e ) {
			return "bsh % ";
		}
	}

	
	public void setExitOnEOF( boolean value ) {
		exitOnEOF = value; 
	}

	
	public void setShowResults( boolean showResults ) {
		this.showResults = showResults;
	}
	
	public boolean getShowResults()  {
		return showResults;
	}
}

