

package bsh;

import bsh.org.objectweb.asm.*;
import bsh.org.objectweb.asm.Type;

import java.lang.reflect.*;
import java.util.ArrayList;
import java.util.List;



public class ClassGeneratorUtil implements Constants 
{
	
	static final String BSHSTATIC="_bshStatic";

	
	static final String BSHTHIS="_bshThis";

	
	static final String BSHSUPER="_bshSuper";

	
	static final String BSHINIT="_bshInstanceInitializer";

	
	static final String BSHCONSTRUCTORS="_bshConstructors";

	
	static final int DEFAULTCONSTRUCTOR = -1;

	static final String OBJECT= "Ljava/lang/Object;";

	String className;
	
	String fqClassName;
	Class superClass; 
	String superClassName;
	Class [] interfaces;
	Variable [] vars;
	Constructor [] superConstructors;
	DelayedEvalBshMethod [] constructors;
	DelayedEvalBshMethod [] methods;
	NameSpace classStaticNameSpace;
	Modifiers classModifiers;
	boolean isInterface;

	
	public ClassGeneratorUtil(
		Modifiers classModifiers, String className, String packageName, 
		Class superClass, Class [] interfaces, Variable [] vars, 
		DelayedEvalBshMethod [] bshmethods, NameSpace classStaticNameSpace,
		boolean isInterface
	) 
	{
		this.classModifiers = classModifiers;
		this.className = className;
		if ( packageName != null ) 
			this.fqClassName = packageName.replace('.','/') + "/" + className;
		else
			this.fqClassName = className;
		if ( superClass == null )
			superClass = Object.class;
		this.superClass = superClass;
		this.superClassName = Type.getInternalName( superClass );
		if ( interfaces == null )
			interfaces = new Class[0];
		this.interfaces = interfaces;
		this.vars = vars;
		this.classStaticNameSpace = classStaticNameSpace;
		this.superConstructors = superClass.getDeclaredConstructors();

		
		List consl = new ArrayList();
		List methodsl = new ArrayList();
		String classBaseName = getBaseName( className ); 
		for( int i=0; i< bshmethods.length; i++ )
			if ( bshmethods[i].getName().equals( classBaseName ) )
				consl.add( bshmethods[i] );
			else
				methodsl.add( bshmethods[i] );

		this.constructors = (DelayedEvalBshMethod [])consl.toArray( 
			new DelayedEvalBshMethod[0] );
		this.methods = (DelayedEvalBshMethod [])methodsl.toArray( 
			new DelayedEvalBshMethod[0] );

		try {
			classStaticNameSpace.setLocalVariable( 
				BSHCONSTRUCTORS, constructors, false );
		} catch ( UtilEvalError e ) {
			throw new InterpreterError("can't set cons var");
		}

		this.isInterface = isInterface;
	}

	
	public byte [] generateClass() 
	{
		
		int classMods = getASMModifiers( classModifiers ) | ACC_PUBLIC;
		if ( isInterface )
			classMods |= ACC_INTERFACE;

		String [] interfaceNames = new String [interfaces.length];
		for(int i=0; i<interfaces.length; i++)
			interfaceNames[i]=Type.getInternalName( interfaces[i] );

		String sourceFile = "BeanShell Generated via ASM (www.objectweb.org)";
		ClassWriter cw = new ClassWriter(false);
		cw.visit( classMods, fqClassName, superClassName, 
			interfaceNames, sourceFile );

		if ( !isInterface )
		{
			
			generateField( 
				BSHTHIS+className, "Lbsh/This;", ACC_PUBLIC, cw);

			
			generateField( 
				BSHSTATIC+className, "Lbsh/This;", ACC_PUBLIC+ACC_STATIC, cw);
		}

		
		for( int i=0; i<vars.length; i++)
		{
			String type = vars[i].getTypeDescriptor();

			
			
			if ( vars[i].hasModifier("private") || type == null )
				continue;
		
			int modifiers;
			if ( isInterface )
				modifiers = ACC_PUBLIC | ACC_STATIC | ACC_FINAL;
			else
				modifiers = getASMModifiers( vars[i].getModifiers() );

			generateField( vars[i].getName(), type, modifiers , cw );
		}

		
		boolean hasConstructor = false;
		for( int i=0; i<constructors.length; i++)
		{
			
			if ( constructors[i].hasModifier("private") )
				continue;

			int modifiers = getASMModifiers( constructors[i].getModifiers() );
			generateConstructor( 
				i, constructors[i].getParamTypeDescriptors(), modifiers, cw );
			hasConstructor = true;
		}

		
		if ( !isInterface && !hasConstructor )
			generateConstructor( 
				DEFAULTCONSTRUCTOR, new String [0], ACC_PUBLIC, cw );

		
		for( int i=0; i<methods.length; i++)
		{
			String returnType = methods[i].getReturnTypeDescriptor();

			
			if ( methods[i].hasModifier("private")  )
				continue;

			int modifiers = getASMModifiers( methods[i].getModifiers() );
			if ( isInterface )
				modifiers |= ( ACC_PUBLIC | ACC_ABSTRACT );

			generateMethod( className, fqClassName, 
				methods[i].getName(), returnType,
				methods[i].getParamTypeDescriptors(), modifiers, cw );

			boolean isStatic = (modifiers & ACC_STATIC) > 0 ;
			boolean overridden = classContainsMethod( 
				superClass, methods[i].getName(), 
				methods[i].getParamTypeDescriptors() ) ;
			if ( !isStatic && overridden )
				generateSuperDelegateMethod( superClassName,
					methods[i].getName(), returnType,
					methods[i].getParamTypeDescriptors(), modifiers, cw );
		}

		return cw.toByteArray();
	}

	
	static int getASMModifiers( Modifiers modifiers ) 
	{
		int mods = 0;
		if ( modifiers == null )
			return mods;

		if ( modifiers.hasModifier("public") )
			mods += ACC_PUBLIC;
		if ( modifiers.hasModifier("protected") )
			mods += ACC_PROTECTED;
		if ( modifiers.hasModifier("static") )
			mods += ACC_STATIC;
		if ( modifiers.hasModifier("synchronized") )
			mods += ACC_SYNCHRONIZED;
		if ( modifiers.hasModifier("abstract") )
			mods += ACC_ABSTRACT;

		return mods;
	}

	
	static void generateField( 
		String fieldName, String type, int modifiers, ClassWriter cw ) 
	{
		cw.visitField( modifiers, fieldName, type, null );
	}
	
	
	static void generateMethod( 
		String className, String fqClassName, String methodName, 
		String returnType, String[] paramTypes, int modifiers, ClassWriter cw ) 
	{
		String [] exceptions = null;
		boolean isStatic = (modifiers & ACC_STATIC) != 0 ;

		if ( returnType == null ) 
			returnType = OBJECT;

		String methodDescriptor = getMethodDescriptor( returnType, paramTypes );

		
		CodeVisitor cv = cw.visitMethod( 
			modifiers, methodName, methodDescriptor, exceptions );

		if ( (modifiers & ACC_ABSTRACT) != 0 )
			return;

		
		if ( isStatic )
		{
			cv.visitFieldInsn( 
				GETSTATIC, fqClassName, BSHSTATIC+className, "Lbsh/This;" );
		}else
		{
			
			cv.visitVarInsn( ALOAD, 0 );

			
			cv.visitFieldInsn( 
				GETFIELD, fqClassName, BSHTHIS+className, "Lbsh/This;" );
		}

		
    	cv.visitLdcInsn( methodName );

		
		generateParameterReifierCode( paramTypes, isStatic, cv );

		
		cv.visitInsn(ACONST_NULL); 
		cv.visitInsn(ACONST_NULL); 
		cv.visitInsn(ACONST_NULL); 

		
		cv.visitInsn(ICONST_1);

		
		cv.visitMethodInsn(
			INVOKEVIRTUAL, "bsh/This", "invokeMethod", 
			Type.getMethodDescriptor( 
				Type.getType(Object.class),
				new Type [] { 
					Type.getType(String.class), 
					Type.getType(Object [].class),
					Type.getType(Interpreter.class),
					Type.getType(CallStack.class),
					Type.getType(SimpleNode.class),
					Type.getType(Boolean.TYPE) 
				} 
			)
		);

		
		cv.visitMethodInsn(
		  INVOKESTATIC, "bsh/Primitive", "unwrap", 
		  "(Ljava/lang/Object;)Ljava/lang/Object;" );

		
		generateReturnCode( returnType, cv );

		
		cv.visitMaxs( 20, 20 );
	}

	
	void generateConstructor( 
		int index, String [] paramTypes, int modifiers, ClassWriter cw ) 
	{
		
		final int argsVar = paramTypes.length+1;
		
		final int consArgsVar = paramTypes.length+2;

		String [] exceptions = null;
		String methodDescriptor = getMethodDescriptor( "V", paramTypes );

		
		CodeVisitor cv = 
			cw.visitMethod( modifiers, "<init>", methodDescriptor, exceptions );

		
		generateParameterReifierCode( paramTypes, false, cv );
		cv.visitVarInsn( ASTORE, argsVar );

		
		generateConstructorSwitch( index, argsVar, consArgsVar, cv );

		

		
		cv.visitVarInsn( ALOAD, 0 );

		
    	cv.visitLdcInsn( className );

		
		cv.visitVarInsn( ALOAD, argsVar );

		
		cv.visitMethodInsn(
			INVOKESTATIC, "bsh/ClassGeneratorUtil", "initInstance",
			"(Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;)V");

		cv.visitInsn( RETURN );

		
		cv.visitMaxs( 20, 20 );
	}

	
	void generateConstructorSwitch( 
		int consIndex, int argsVar, int consArgsVar, CodeVisitor cv )
	{
		Label defaultLabel = new Label();
		Label endLabel = new Label();
		int cases = superConstructors.length + constructors.length ;

		Label [] labels = new Label[ cases ];
		for(int i=0; i<cases; i++)
			labels[i]=new Label();

		
		

		
    	cv.visitLdcInsn( superClass.getName() ); 

		
		cv.visitFieldInsn( 
			GETSTATIC, fqClassName, BSHSTATIC+className, "Lbsh/This;" );

		
		cv.visitVarInsn( ALOAD, argsVar );

		
		cv.visitIntInsn( BIPUSH, consIndex );

		
		cv.visitMethodInsn(
			INVOKESTATIC, "bsh/ClassGeneratorUtil", "getConstructorArgs",
			"(Ljava/lang/String;Lbsh/This;[Ljava/lang/Object;I)"
			+"Lbsh/ClassGeneratorUtil$ConstructorArgs;"
		);

		
		cv.visitVarInsn( ASTORE, consArgsVar );

		

		
		cv.visitVarInsn( ALOAD, consArgsVar );
		cv.visitFieldInsn( 
			GETFIELD, "bsh/ClassGeneratorUtil$ConstructorArgs", "selector", "I" );

		
		cv.visitTableSwitchInsn( 
			0, cases-1, defaultLabel, labels );

		
		int index = 0;
		for( int i=0; i< superConstructors.length; i++, index++)
			doSwitchBranch( index, superClassName, 
				getTypeDescriptors( superConstructors[i].getParameterTypes() ), 
				endLabel, labels, consArgsVar, cv );
		for( int i=0; i< constructors.length; i++, index++)
			doSwitchBranch( index, fqClassName, 
				constructors[i].getParamTypeDescriptors(), 
				endLabel, labels, consArgsVar, cv );
	
		
		cv.visitLabel( defaultLabel );
		
		cv.visitVarInsn( ALOAD, 0 ); 
		cv.visitMethodInsn( INVOKESPECIAL, superClassName, "<init>", "()V" );

		
		cv.visitLabel( endLabel );
	}

	
	static void doSwitchBranch( 
		int index, String targetClassName, String [] paramTypes,
		Label endLabel, Label [] labels, int consArgsVar, CodeVisitor cv
	)
	{
		cv.visitLabel( labels[index] );
		
		cv.visitVarInsn( ALOAD, 0 ); 

		
		for (int i=0; i<paramTypes.length; i++)
		{
			String type = paramTypes[i];
			String method = null;
			if      ( type.equals("Z") )
				method = "getBoolean";
			else if ( type.equals("B") )
				method = "getByte";
			else if ( type.equals("C") )
				method = "getChar";
			else if ( type.equals("S") )
				method = "getShort";
			else if ( type.equals("I") )
				method = "getInt";
			else if ( type.equals("J") )
				method = "getLong";
			else if ( type.equals("D") )
				method = "getDouble";
			else if ( type.equals("F") )
				method = "getFloat";
			else 
				method = "getObject";

			
			cv.visitVarInsn( ALOAD, consArgsVar ); 
			String className = "bsh/ClassGeneratorUtil$ConstructorArgs";
			String retType;
			if ( method.equals("getObject") )
				retType = OBJECT;
			else
				retType = type; 
			cv.visitMethodInsn(INVOKEVIRTUAL, className, method, "()"+retType);
			
			if ( method.equals("getObject") )
				cv.visitTypeInsn( CHECKCAST, descriptorToClassName(type) ); 
		}

		
		String descriptor = getMethodDescriptor( "V", paramTypes );
		cv.visitMethodInsn( 
			INVOKESPECIAL, targetClassName, "<init>", descriptor );
		cv.visitJumpInsn( GOTO, endLabel );
	}
	
	static String getMethodDescriptor( String returnType, String [] paramTypes )
	{
		StringBuffer sb = new StringBuffer("(");
		for(int i=0; i<paramTypes.length; i++)
			sb.append(paramTypes[i]);
		sb.append(")"+returnType);
		return sb.toString();
	}

	
	
	static void generateSuperDelegateMethod( String superClassName, String methodName,
											 String returnType, String[] paramTypes, int modifiers, ClassWriter cw )
	{
		String [] exceptions = null;

		if ( returnType == null ) 
			returnType = OBJECT;

		String methodDescriptor = getMethodDescriptor( returnType, paramTypes );

		
		CodeVisitor cv = cw.visitMethod( 
			modifiers, "_bshSuper"+methodName, methodDescriptor, exceptions );

		cv.visitVarInsn(ALOAD, 0);
		
		int localVarIndex = 1;
		for (int i = 0; i < paramTypes.length; ++i) 
		{
			if ( isPrimitive( paramTypes[i]) )
				cv.visitVarInsn(ILOAD, localVarIndex);
			else
				cv.visitVarInsn(ALOAD, localVarIndex);
			localVarIndex += 
				( (paramTypes[i].equals("D") || paramTypes[i].equals("J")) 
					? 2 : 1 );
		}

		cv.visitMethodInsn( INVOKESPECIAL, 
			superClassName, methodName, methodDescriptor );

		generatePlainReturnCode( returnType, cv );

		
		cv.visitMaxs( 20, 20 );
	}

	boolean classContainsMethod(
		Class clas, String methodName, String [] paramTypes )
	{
		while( clas != null )
		{
			Method [] methods = clas.getDeclaredMethods();
			for( int i =0; i<methods.length; i++ )
			{
				if ( methods[i].getName().equals(methodName) )
				{
					String [] methodParamTypes = 
						getTypeDescriptors( methods[i].getParameterTypes() );
					boolean found = true;
					for( int j=0; j<methodParamTypes.length; j++)
					{
						if ( ! paramTypes[j].equals( methodParamTypes[j] ) ) {
							found = false;
							break;
						}
					}
					if ( found )
						return true;
				}
			}

			clas = clas.getSuperclass();
		}

		return false;
	}

	
	static void generatePlainReturnCode( String returnType, CodeVisitor cv )
	{
		if ( returnType.equals("V") )
			cv.visitInsn( RETURN );
		else 
		if ( isPrimitive( returnType ) )
		{
			int opcode = IRETURN;
			if ( returnType.equals("D") )
				opcode = DRETURN;
			else if ( returnType.equals("F") )
				opcode = FRETURN;
			else if ( returnType.equals("J") )  
				opcode = LRETURN;

			cv.visitInsn(opcode);
		}
		else {
			cv.visitTypeInsn( CHECKCAST, descriptorToClassName(returnType) );
			cv.visitInsn( ARETURN );
		}
	}

	
	public static void generateParameterReifierCode (
		String [] paramTypes, boolean isStatic, final CodeVisitor cv )
	{
		cv.visitIntInsn(SIPUSH, paramTypes.length);
		cv.visitTypeInsn(ANEWARRAY, "java/lang/Object");
		int localVarIndex = isStatic ? 0 : 1;
		for (int i = 0; i < paramTypes.length; ++i) 
		{
			String param = paramTypes[i];
			cv.visitInsn(DUP);
			cv.visitIntInsn(SIPUSH, i);
			if ( isPrimitive( param ) ) 
			{
                int opcode;
                if (param.equals("F")) {
                    opcode = FLOAD;
                } else if (param.equals("D")) {
                    opcode = DLOAD;
                } else if (param.equals("J")) {
                    opcode = LLOAD;
                } else {
                    opcode = ILOAD;
                }

				String type = "bsh/Primitive";
				cv.visitTypeInsn( NEW, type );
				cv.visitInsn(DUP);
				cv.visitVarInsn(opcode, localVarIndex);
				String desc = param; 
				cv.visitMethodInsn(
					INVOKESPECIAL, type, "<init>", "(" + desc + ")V");
			} else {
				
				
				
				
				
				cv.visitVarInsn( ALOAD, localVarIndex );
			}
			cv.visitInsn(AASTORE);
			localVarIndex += 
				( (param.equals("D") || param.equals("J")) ? 2 : 1 );
		}
  }

  
	public static void generateReturnCode (
		String returnType, CodeVisitor cv ) 
	{
		if ( returnType.equals("V") ) 
		{
			cv.visitInsn(POP);
			cv.visitInsn(RETURN);
		} 
		else if ( isPrimitive( returnType ) ) 
		{
			int opcode = IRETURN;
			String type;
			String meth;
			if ( returnType.equals("B") ) {
				type = "java/lang/Byte";
				meth = "byteValue";
		 	} else if (returnType.equals("I") ) {
				type = "java/lang/Integer";
				meth = "intValue";
			} else if (returnType.equals("Z") ) {
				type = "java/lang/Boolean";
				meth = "booleanValue";
			} else if (returnType.equals("D") ) {
				opcode = DRETURN;
				type = "java/lang/Double";
				meth = "doubleValue";
		 	} else if (returnType.equals("F") ) {
				opcode = FRETURN;
				type = "java/lang/Float";
				meth = "floatValue";
			} else if (returnType.equals("J") ) {
				opcode = LRETURN;
				type = "java/lang/Long";
				meth = "longValue";
			} else if (returnType.equals("C") ) {
				type = "java/lang/Character";
				meth = "charValue";
			} else  {
				type = "java/lang/Short";
				meth = "shortValue";
			}

			String desc = returnType;
			cv.visitTypeInsn( CHECKCAST, type ); 
			cv.visitMethodInsn( INVOKEVIRTUAL, type, meth, "()" + desc );
			cv.visitInsn(opcode);
		} else 
		{
			cv.visitTypeInsn( CHECKCAST, descriptorToClassName(returnType) );
			cv.visitInsn(ARETURN);
		}
  }

	
	public static ConstructorArgs getConstructorArgs( 
		String superClassName, This classStaticThis, 
		Object [] consArgs, int index )
	{
		DelayedEvalBshMethod [] constructors;
		try {
			constructors = 
				(DelayedEvalBshMethod [])classStaticThis.getNameSpace()
				.getVariable( BSHCONSTRUCTORS );
		} catch ( Exception e ) {
			throw new InterpreterError(
				"unable to get instance initializer: "+e );
		}

		if ( index == DEFAULTCONSTRUCTOR ) 
			return ConstructorArgs.DEFAULT; 

		DelayedEvalBshMethod constructor = constructors[index];

		if ( constructor.methodBody.jjtGetNumChildren() == 0 )
			return ConstructorArgs.DEFAULT; 

		
		String altConstructor = null;
		BSHArguments argsNode = null;
		SimpleNode firstStatement = 
			(SimpleNode)constructor.methodBody.jjtGetChild(0);
		if ( firstStatement instanceof BSHPrimaryExpression )
			firstStatement = (SimpleNode)firstStatement.jjtGetChild(0);
		if ( firstStatement instanceof BSHMethodInvocation )
		{
			BSHMethodInvocation methodNode = 
				(BSHMethodInvocation)firstStatement;
			BSHAmbiguousName methodName = methodNode.getNameNode();
			if ( methodName.text.equals("super") 
				|| methodName.text.equals("this") 
			) {
				altConstructor = methodName.text;
				argsNode = methodNode.getArgsNode();
			}
		}

		if ( altConstructor == null )
			return ConstructorArgs.DEFAULT; 

		
		
		NameSpace consArgsNameSpace = 
			new NameSpace( classStaticThis.getNameSpace(), "consArgs" );
		String [] consArgNames = constructor.getParameterNames();
		Class [] consArgTypes = constructor.getParameterTypes();
		for( int i=0; i<consArgs.length; i++ )
		{
			try {
				consArgsNameSpace.setTypedVariable( 
					consArgNames[i], consArgTypes[i], consArgs[i], 
					null);
			} catch ( UtilEvalError e ) {
				throw new InterpreterError("err setting local cons arg:"+e);
			}
		}

		

		CallStack callstack = new CallStack();
		callstack.push( consArgsNameSpace);
		Object [] args = null;
		Interpreter interpreter = classStaticThis.declaringInterpreter;

		try {
			args = argsNode.getArguments( callstack, interpreter );
		} catch ( EvalError e ) {
			throw new InterpreterError(
				"Error evaluating constructor args: "+e );
		}

		Class [] argTypes  = Types.getTypes( args );
		args = Primitive.unwrap( args );
		Class superClass = 
			interpreter.getClassManager().classForName( superClassName );
		if ( superClass == null )
			throw new InterpreterError(
				"can't find superclass: "+superClassName );
		Constructor [] superCons = superClass.getDeclaredConstructors();

		
		if ( altConstructor.equals("super") )
		{
			int i = Reflect.findMostSpecificConstructorIndex( 
				argTypes , superCons );
			if ( i == -1 )
				throw new InterpreterError("can't find constructor for args!");
			return new ConstructorArgs( i, args );
		}

		
		Class [][] candidates = new Class [ constructors.length ] [];
		for(int i=0; i< candidates.length; i++ )
			candidates[i] = constructors[i].getParameterTypes();
		int i = Reflect.findMostSpecificSignature( argTypes, candidates );
		if ( i == -1 )
			throw new InterpreterError("can't find constructor for args 2!");
		

		int selector = i+superCons.length;
		int ourSelector = index+superCons.length;

		
		if ( selector == ourSelector )
			throw new InterpreterError( "Recusive constructor call.");

		return new ConstructorArgs( selector, args );
	}
	
	
	public static void initInstance( 
		Object instance, String className, Object [] args )
	{
		Class [] sig = Types.getTypes( args );
		CallStack callstack = new CallStack();
		Interpreter interpreter;
		NameSpace instanceNameSpace;

		
		
		This instanceThis = getClassInstanceThis( instance, className );


		if ( instanceThis == null )
		{
			
			

			
			This classStaticThis = 
				getClassStaticThis( instance.getClass(), className );
			interpreter = classStaticThis.declaringInterpreter;

			
			BSHBlock instanceInitBlock;
			try {
				instanceInitBlock = (BSHBlock)classStaticThis.getNameSpace()
					.getVariable( BSHINIT );
			} catch ( Exception e ) {
				throw new InterpreterError(
					"unable to get instance initializer: "+e );
			}

			
			instanceNameSpace = 
				new NameSpace( classStaticThis.getNameSpace(), className );
			instanceNameSpace.isClass = true;

			
			instanceThis = instanceNameSpace.getThis( interpreter );
			try {
				LHS lhs = 
					Reflect.getLHSObjectField( instance, BSHTHIS+className );
				lhs.assign( instanceThis, false );
			} catch ( Exception e ) {
				throw new InterpreterError("Error in class gen setup: "+e );
			}

			
			instanceNameSpace.setClassInstance( instance );

			
			callstack.push( instanceNameSpace );

			
			try { 
				instanceInitBlock.evalBlock( 
					callstack, interpreter, true, 
					ClassGeneratorImpl.ClassNodeFilter.CLASSINSTANCE );
			} catch ( Exception e ) {
				throw new InterpreterError("Error in class initialization: "+e);
			}

			callstack.pop();

		} else
		{
			
			
			interpreter = instanceThis.declaringInterpreter;
			instanceNameSpace = instanceThis.getNameSpace();
		}

		

		String constructorName = getBaseName( className );
		try {
			
			BshMethod constructor = instanceNameSpace.getMethod( 
				constructorName, sig, true );

			
			if ( args.length > 0 && constructor == null )
				throw new InterpreterError(
					"Can't find constructor: "+ className );

			
			if ( constructor != null )
				constructor.invoke( args, interpreter, callstack,
					null, false ) ;
		} catch ( Exception e ) {
			if ( e instanceof TargetError )
				e =(Exception)((TargetError)e).getTarget();
			if ( e instanceof InvocationTargetException )
				e = (Exception)((InvocationTargetException)e)
					.getTargetException();
			e.printStackTrace( System.err );
			throw new InterpreterError("Error in class initialization: "+e );
		} 
	}

	
	static This getClassStaticThis( Class clas, String className )
	{
		try {
			return (This)Reflect.getStaticFieldValue(
				clas, BSHSTATIC + className );
		} catch ( Exception e ) {
			throw new InterpreterError("Unable to get class static space: "+e);
		}
	}

	
	static This getClassInstanceThis( Object instance, String className )
	{
		try {
			Object o = Reflect.getObjectFieldValue( instance, BSHTHIS+className );
			return (This)Primitive.unwrap(o); 
		} catch ( Exception e ) {
			throw new InterpreterError(
				"Generated class: Error getting This"+e );
		}
	}

	
	private static boolean isPrimitive( String typeDescriptor )
	{
		return typeDescriptor.length() == 1; 
	}

	static String[] getTypeDescriptors( Class [] cparams )
	{
		String [] sa = new String [cparams.length];
		for(int i=0; i<sa.length; i++)
			sa[i] = BSHType.getTypeDescriptor( cparams[i] );
		return sa;
	}

	
	
	
	private static String descriptorToClassName( String s ) 
	{
		if ( s.startsWith("[") || !s.startsWith("L") )
			return s;
		return s.substring( 1, s.length()-1 );
	}

	private static String getBaseName( String className ) 
	{
		int i = className.indexOf("$");
		if ( i == -1 )
			return className;

		return className.substring(i+1);
	}

	
	public static class ConstructorArgs
	{
		
		public static ConstructorArgs DEFAULT = new ConstructorArgs();

		public int selector = DEFAULTCONSTRUCTOR;
		Object [] args;
		int arg = 0;
		

		ConstructorArgs() {  }

		ConstructorArgs( int selector, Object [] args ) { 
			this.selector = selector;
			this.args = args; 
		}

		Object next() { return args[arg++]; }

		public boolean getBoolean() { return ((Boolean)next()).booleanValue(); }
		public byte getByte() { return ((Byte)next()).byteValue(); }
		public char getChar() { return ((Character)next()).charValue(); }
		public short getShort() { return ((Short)next()).shortValue(); }
		public int getInt() { return ((Integer)next()).intValue(); }
		public long getLong() { return ((Long)next()).longValue(); }
		public double getDouble() { return ((Double)next()).doubleValue(); }
		public float getFloat() { return ((Float)next()).floatValue(); }
		public Object getObject() { return next(); }
	}
}
