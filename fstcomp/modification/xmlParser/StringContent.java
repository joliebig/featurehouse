package modification.xmlParser;

import de.ovgu.cide.fstgen.ast.FSTNode;

public class StringContent implements Content {

    String s = null;

    public StringContent(String s) {
	this.s = s;
    }

    @Override
    public FSTNode getContent() {
	return null;
    }

}
