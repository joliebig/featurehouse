package modification.content.Parseables;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import modification.content.Content;
import cide.gparser.CharStream;
import cide.gparser.OffsetCharStream;

/**
 * 
 * @author boxleitner
 * 
 */
public abstract class ParseableFile implements Content {
    
    /**
     * @param file
     */
    public ParseableFile(File file) {
	super();
	this.file = file;
    }

    /**
     * 
     */
    protected File file;

    /**
     * 
     * @return
     * @throws FileNotFoundException
     */
    public CharStream getCharStream() throws FileNotFoundException {
	return new OffsetCharStream(new FileInputStream(file));
    }
}
