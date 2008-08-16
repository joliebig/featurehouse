package modification.content.FSTParseables;

import java.io.FileNotFoundException;

import cide.gparser.CharStream;

public interface Input {

    public CharStream getCharStream() throws FileNotFoundException;

    public String getType();

}
