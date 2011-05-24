package cide.astgen.nparser.ast;

import java.util.HashMap;

/**
 * annotations are used for choices. they are the first term in a choice.
 * 
 * alternatively an annotation can be specified for a production and is thus
 * valid for all choices therein.
 * 
 * @author ckaestne
 * 
 */
public class NAnnotation {

	private String name;
	public final HashMap<String, String> values = new HashMap<String, String>();

	public NAnnotation(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public void addValue(String name, String value) {
		values.put(name, value);
	}

	@Override
	public String toString() {
		String r = "@" + name + "(";
		for (String k : values.keySet())
			r += k + "=" + values.get(k);
		r += ")";
		return r;
	}
}
