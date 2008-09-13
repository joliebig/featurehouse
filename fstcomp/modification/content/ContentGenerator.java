/**
 * 
 */
package modification.content;

import java.io.File;

import modification.content.Parseables.CFile;
import modification.content.Parseables.CFunction;
import modification.content.Parseables.HaskellDefinition;
import modification.content.Parseables.HaskellFile;
import modification.content.Parseables.JavaFile;
import modification.content.Parseables.JavaMethod;

/**
 * generates an FST out of a given root file
 * 
 * @author Boxleitner Stefan
 */
public class ContentGenerator {
    /*
     * returns all files in a given directory (recursive)
     */
    public static Content createContent(File file)
	    throws UnknownFileTypeParseException {
	if (file.getName().endsWith("java")) {
	    return new JavaFile(file);
	} else if (file.getName().endsWith("c")) {
	    return new CFile(file);
	} else if (file.getName().endsWith("hs")) {
	    return new HaskellFile(file);
	    // } else if (type.equals("")) {
	    // return null;
	} else {
	    System.out.println("> " + file.getName());
	    throw new UnknownFileTypeParseException();
	}
    }

    public static Content createContent(String type, String content)
	    throws UnknownContentTypeParseException {
	if (type.equals("java.method")) {
	    return new JavaMethod(type, content);
	} else if (type.equals("c.function")) {
	    return new CFunction(type, content);
	} else if (type.equals("haskell.definition")) {
	    return new HaskellDefinition(type, content);
	    // } else if (type.equals("")) {
	    // return null;
	} else
	    throw new UnknownContentTypeParseException();

    }
}
