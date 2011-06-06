

package bsh;

import java.util.Hashtable;


public class Capabilities 
{
	private static boolean accessibility = false;

	public static boolean haveSwing() {
		
		return classExists( "javax.swing.JButton" );
	}

	public static boolean canGenerateInterfaces() {
		
		return classExists( "java.lang.reflect.Proxy" );
	}

	
	public static boolean haveAccessibility() 
	{
		
		return ( accessibility 
			&& classExists( "java.lang.reflect.AccessibleObject" )
			&& classExists("bsh.reflect.ReflectManagerImpl") 
		);
	}

	public static void setAccessibility( boolean b ) { accessibility = b; }

	private static Hashtable classes = new Hashtable();
	
	public static boolean classExists( String name ) 
	{
		Object c = classes.get( name );

		if ( c == null ) {
			try {
				
				c = Class.forName( name );
			} catch ( ClassNotFoundException e ) { }

			if ( c != null )
				classes.put(c,"unused");
		}

		return c != null;
	}

	
	public static class Unavailable extends RuntimeException 
	{
		public Unavailable(String s ){ super(s); }
	}
}


