package modification.FSTParseables;

import java.io.StringReader;

import cide.gparser.CharStream;
import cide.gparser.OffsetCharStream;

public class StringInput implements Input {

    /**
     * @param input
     */
    public StringInput(String string) {
	super();
	this.string = string;
    }

    private String string;

    @Override
    public CharStream getCharStream() {
	return new OffsetCharStream(new StringReader(string));
    }

}
