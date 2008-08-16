package modification.content.FSTParseables;

import java.io.StringReader;

import cide.gparser.CharStream;
import cide.gparser.OffsetCharStream;

public class StringInput implements Input {

    private String type;

    private String string;

    /**
     * @param string
     * @param type
     */
    public StringInput(String string, String type) {
	super();
	this.string = string;
	this.type = type;
    }

    @Override
    public CharStream getCharStream() {
	return new OffsetCharStream(new StringReader(string));
    }

    @Override
    public String getType() {
	return type;
    }

}
