package modification.content.FSTParseables;

import java.io.FileNotFoundException;

import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class UnknownType extends FSTParseable {

    public UnknownType(Input input) {
	super(input);
    }

    @Override
    public FSTNode getFST() throws FileNotFoundException, ParseException {
	return new FSTTerminal(input.getType(), "", "", "");
    }

}
