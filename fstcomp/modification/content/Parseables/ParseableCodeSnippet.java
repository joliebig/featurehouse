package modification.content.Parseables;

import java.io.StringReader;

import modification.content.Content;
import cide.gparser.CharStream;
import cide.gparser.OffsetCharStream;

/**
 * 
 * @author boxleitner
 * 
 */
public abstract class ParseableCodeSnippet implements Content {
    

    /**
     * 
     */
    protected String content;

    /**
     * @param content
     */
    public ParseableCodeSnippet(String content) {
	this.content = content;	
    }

    /**
     * 
     * @return
     */
    public CharStream getCharStream() {	
	return new OffsetCharStream(new StringReader(content));
    }
}
