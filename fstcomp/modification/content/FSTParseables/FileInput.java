package modification.content.FSTParseables;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import cide.gparser.CharStream;
import cide.gparser.OffsetCharStream;

public class FileInput implements Input {

    /**
     * @param file
     */
    public FileInput(File file) {
	super();
	this.file = file;
    }

    private File file;

    @Override
    public CharStream getCharStream() throws FileNotFoundException {
	return new OffsetCharStream(new FileInputStream(file));
    }

    @Override
    public String getType() {
	if (file.getName().split("\\.").length > 0)
	    return file.getName().split("\\.")[file.getName().split("\\.").length - 1];
	else
	    return "";
    }

}
