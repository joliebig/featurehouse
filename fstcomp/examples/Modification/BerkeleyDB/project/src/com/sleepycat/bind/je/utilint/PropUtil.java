package com.sleepycat.je.utilint; 
import java.util.Enumeration; 
import java.util.Properties; 
import java.util.Set; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
public  class  PropUtil {
	 public static boolean getBoolean__wrappee__base( Properties props, String propName){ String value=props.getProperty(propName); if ((value != null) && (value.equalsIgnoreCase("true"))) { return true; } else { return false; } }

	 public static boolean getBoolean( Properties props, String propName){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBoolean__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static Properties validateProps__wrappee__base( Properties props, Set allowedProps, String apiMethod) throws DatabaseException { if (props == null) { return new Properties(); } else { if (props.size() > 0) { Enumeration e=props.propertyNames(); while (e.hasMoreElements()) { String propName=(String)e.nextElement(); validateProp(propName,allowedProps,apiMethod); } } return props; } }

	 public static Properties validateProps( Properties props, Set allowedProps, String apiMethod) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validateProps__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static void validateProp__wrappee__base( String propName, Set allowedProps, String apiMethod) throws DatabaseException { if (!allowedProps.contains(propName)) { throw new DatabaseException(propName + " is not a valid property for " + apiMethod); } }

	 public static void validateProp( String propName, Set allowedProps, String apiMethod) throws DatabaseException { t.in(Thread.currentThread().getStackTrace()[1].toString());	validateProp__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public static long microsToMillis__wrappee__base( long micros){ return (micros + 999) / 1000; }

	 public static long microsToMillis( long micros){ t.in(Thread.currentThread().getStackTrace()[1].toString());	microsToMillis__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
