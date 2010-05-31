package processor.capprox;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;
import de.ovgu.cide.fstgen.ast.FSTVisitor;

public class CIncludeGuardGenerator extends FSTVisitor {

    final static String[] REGEX_TO_REPLACE = { "\\.", "-" };

    final static String REGEX_REPLACEMENT = "_";

    private static final String HEADER_GUARD_IFNDEF_LINE_NAME = "";

    private static final String HEADER_GUARD_IFNDEF_LINE_TYPE = "Define";

    private static final String HEADER_GUARD_IFNDEF_LINE_CONTENT = "#ifndef";

    private static final String HEADER_GUARD_DEFINE_LINE_NAME = "";

    private static final String HEADER_GUARD_DEFINE_LINE_TYPE = "Define";

    private static final String HEADER_GUARD_DEFINE_LINE_CONTENT = "#define";

    private static final String HEADER_GUARD_ENDIF_LINE_NAME = "";

    private static final String HEADER_GUARD_ENDIF_LINE_TYPE = "Define";

    private static final String HEADER_GUARD_ENDIF_LINE_CONTENT = "#endif";

    private static final String FILE_TYPE = "H-File";

    private static final String GUARD_PARENT = "Sequence_CodeUnit_TopLevel";

    public boolean visit(FSTTerminal terminal) {

	return true;
    }

    public boolean visit(FSTNonTerminal nonTerminal) {
	if (nonTerminal.getType().equals(FILE_TYPE)) {
	    FSTNonTerminal parent = findNonTerminal(nonTerminal,
		    GUARD_PARENT);
	    String identifier = fitString(nonTerminal.getName());
	    parent.getChildren()
		    .add(
			    0,
			    new FSTTerminal(HEADER_GUARD_DEFINE_LINE_TYPE,
				    HEADER_GUARD_DEFINE_LINE_NAME,
				    HEADER_GUARD_DEFINE_LINE_CONTENT + " "
					    + identifier, ""));
	    parent.getChildren()
		    .add(
			    0,
			    new FSTTerminal(HEADER_GUARD_IFNDEF_LINE_TYPE,
				    HEADER_GUARD_IFNDEF_LINE_NAME,
				    HEADER_GUARD_IFNDEF_LINE_CONTENT + " "
					    + identifier, ""));
	    parent.getChildren().add(
		    new FSTTerminal(HEADER_GUARD_ENDIF_LINE_TYPE,
			    HEADER_GUARD_ENDIF_LINE_NAME,
			    HEADER_GUARD_ENDIF_LINE_CONTENT, ""));
	    //System.out.println(nonTerminal);
	}

	return true;
    }

    private String fitString(String name) {
	for (String regex : REGEX_TO_REPLACE) {
	    name = name.replaceAll(regex, REGEX_REPLACEMENT);
	}
	return name.toUpperCase();
    }

    private FSTNonTerminal findNonTerminal(FSTNonTerminal nonTerminal,
	    String type) {
	if (nonTerminal.getType().equals(type)) {
	    return nonTerminal;
	} else {
	    for (FSTNode child : nonTerminal.getChildren()) {
		if (child instanceof FSTNonTerminal) {
		    return findNonTerminal((FSTNonTerminal) child, type);
		}
	    }
	    return null;
	}
    }

    public void postVisit(FSTTerminal terminal) {
    }

    public void postVisit(FSTNonTerminal nonTerminal) {
    }
}
