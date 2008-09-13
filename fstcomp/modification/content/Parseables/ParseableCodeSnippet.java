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
    protected String type;

    /**
     * 
     */
    protected String content;

    /**
     * @param content
     * @param type
     */
    public ParseableCodeSnippet(String type, String content) {
	this.content = content;
	this.type = type;
    }

    /**
     * 
     * @return
     */
    public CharStream getCharStream() {
	return new OffsetCharStream(new StringReader(content));
    }
}
