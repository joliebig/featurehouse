

package bsh.classpath;

import java.io.*;
import java.io.File;
import java.util.*;
import java.awt.*;
import bsh.BshClassManager;
import bsh.classpath.BshClassPath.ClassSource;
import bsh.classpath.BshClassPath.DirClassSource;
import bsh.classpath.BshClassPath.GeneratedClassSource;


public class DiscreteFilesClassLoader extends BshClassLoader 
{
	
	ClassSourceMap map;

	public static class ClassSourceMap extends HashMap 
	{
		public void put( String name, ClassSource source ) {
			super.put( name, source );
		}
		public ClassSource get( String name ) {
			return (ClassSource)super.get( name );
		}
	}
	
	public DiscreteFilesClassLoader( 
		BshClassManager classManager, ClassSourceMap map ) 
	{
		super( classManager );
		this.map = map;
	}

	
	public Class findClass( String name ) throws ClassNotFoundException 
	{
		
		ClassSource source = map.get( name );

		if ( source != null )
		{
			byte [] code = source.getCode( name );
			return defineClass( name, code, 0, code.length );
		} else
			
			
			return super.findClass( name );
	}

	public String toString() {
		return super.toString() + "for files: "+map;
	}

}
