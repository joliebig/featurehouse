

package org.gjt.sp.jedit.bsh;

import org.gjt.sp.jedit.bsh.Capabilities.Unavailable;


public abstract class ReflectManager
{
	private static ReflectManager rfm;

	
	public static ReflectManager getReflectManager() 
		throws Unavailable
	{
		if ( rfm == null ) 
		{
			Class clas;
			try {
				clas = Class.forName( "org.gjt.sp.jedit.bsh.reflect.ReflectManagerImpl" );
				rfm = (ReflectManager)clas.newInstance();
			} catch ( Exception e ) {
				throw new Unavailable("Reflect Manager unavailable: "+e);
			}
		}
	
		return rfm;
	}

	
	public static boolean RMSetAccessible( Object obj ) 
		throws Unavailable
	{
		return getReflectManager().setAccessible( obj );
	}

	
	public abstract boolean setAccessible( Object o );
}

