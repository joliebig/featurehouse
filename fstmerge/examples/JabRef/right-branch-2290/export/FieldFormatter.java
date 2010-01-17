
package net.sf.jabref.export;


public interface FieldFormatter {
	public String format(String s, String fieldName) throws IllegalArgumentException;
}
