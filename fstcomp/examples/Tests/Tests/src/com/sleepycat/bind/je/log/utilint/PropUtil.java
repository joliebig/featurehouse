package com.sleepycat.je.utilint; 
import java.util.Enumeration; 
import java.util.Properties; 
import java.util.Set; 
import com.sleepycat.je.DatabaseException; 
import de.ovgu.cide.jakutil.*; 
public  class  PropUtil {
	 public static boolean getBoolean( Properties props, String propName){ String value=props.getProperty(propName); if ((value != null) && (value.equalsIgnoreCase("true"))) { return true; } else { return false; } }

	 public static Properties validateProps( Properties props, Set allowedProps, String apiMethod) throws DatabaseException { if (props == null) { return new Properties(); } else { if (props.size() > 0) { Enumeration e=props.propertyNames(); while (e.hasMoreElements()) { String propName=(String)e.nextElement(); validateProp(propName,allowedProps,apiMethod); } } return props; } }

	 public static void validateProp( String propName, Set allowedProps, String apiMethod) throws DatabaseException { if (!allowedProps.contains(propName)) { throw new DatabaseException(propName + " is not a valid property for " + apiMethod); } }

	 public static long microsToMillis( long micros){ return (micros + 999) / 1000; }


}
