/**
 * 
 */
package modification.content;

import java.io.File;

import modification.content.Parseables.C.CFile;
import modification.content.Parseables.C.CFunction;
import modification.content.Parseables.CSharp.CSharpFile;
import modification.content.Parseables.CSharp.CSharpInterfaceMemberDeclaration;
import modification.content.Parseables.CSharp.CSharpMethod;
import modification.content.Parseables.haskell.HaskellDefinition;
import modification.content.Parseables.haskell.HaskellFile;
import modification.content.Parseables.java.JavaFile;
import modification.content.Parseables.java.JavaMethod;
import modification.content.Parseables.java.JavaMethodBody;

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
	} else if (file.getName().endsWith("cs")) {
	    return new CSharpFile(file);
	    // } else if (file.getName().endsWith("")) {
	    // return null;
	    // } else if (file.getName().endsWith("")) {
	    // return null;
	} else {
	    System.out.println("> " + file.getName());
	    throw new UnknownFileTypeParseException();
	}
    }

    public static Content createContent(String type, String content)
	    throws UnknownContentTypeParseException {
	if (type.equals("java.method")) {
	    return new JavaMethod(content);
	} else if (type.equals("c.function")) {
	    return new CFunction(content);
	} else if (type.equals("haskell.definition")) {
	    return new HaskellDefinition(content);
	} else if (type.equals("cSharp.method")) {
	    return new CSharpMethod(content);
	} else if (type.equals("java.methodBody")) {
	    return new JavaMethodBody(content);
	} else if (type.equals("cSharp.interfaceMemberDeclaration")) {
	    return new CSharpInterfaceMemberDeclaration(content);
	    // } else if (type.equals("")) {
	    // return null;
	} else
	    throw new UnknownContentTypeParseException();

    }
}
