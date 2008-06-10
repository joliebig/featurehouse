package modification.Test;
import java.util.LinkedList;
import java.util.List;

import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

import modification.queryLanguageParser.ParseException;
import modification.queryLanguageParser.QueryLanguageParser;


public class ParserTest {

    private static int NUMBER_OF_CHILDREN = 5;

    private static int TREE_DEPTH = 3;

    class NameTypePatternVector {

	public final static int POSITION_NAME = 0;

	public final static int POSITION_TYPE = 1;

	String[] nameType = new String[2];

	public NameTypePatternVector(String namePattern, String typePattern) {
	    nameType[POSITION_NAME] = namePattern;
	    nameType[POSITION_TYPE] = typePattern;
	}

	public String getPattern(int i) {
	    return nameType[i];
	}
    }

    /**
     * @param args
     */
    public static void main(String[] args) {	
	
	// TODO Auto-generated method stub
	FSTNode root = new FSTNonTerminal("package", "root", generateChildren(
		"foo", NUMBER_OF_CHILDREN, TREE_DEPTH - 1));

	System.out.println(root);
	
	List<FSTNode> list = new LinkedList<FSTNode>();
	
	String query = "..*:a*";
	System.out.println(query);
	
	QueryLanguageParser parser = new QueryLanguageParser(query, root);
	try {
	    list = parser.parse();
	} catch (ParseException e) {
	    // TODO Auto-generated catch block
	    e.printStackTrace();
	}

	for (FSTNode node : list) {
	    System.out.println(node.getName() + ":" + node.getType());
	}
    }

    private static List<FSTNode> generateChildren(String name, int number,
	    int depth) {
	List<FSTNode> list = new LinkedList<FSTNode>();
	if (depth > 2) {
	    for (int i = 0; i < number; i++) {
		list.add(new FSTNonTerminal("package",
			name + String.valueOf(i), generateChildren(name,
				number, depth - 1)));
	    }
	} else if (depth == 2) {
	    for (int i = 0; i < number; i++) {
		list.add(new FSTNonTerminal("class", name + String.valueOf(i),
			generateChildren(name, number, depth - 1)));
	    }

	} else {
	    list.addAll(generateTerminals("attribute", name, number));
	    list.addAll(generateTerminals("method", name, number));
	}

	return list;

    }

    private static List<FSTNode> generateTerminals(String type, String name,
	    int number) {
	List<FSTNode> list = new LinkedList<FSTNode>();
	for (int i = 0; i < number; i++) {
	    list.add(new FSTTerminal(type, name + String.valueOf(i), "body",
		    "prefix"));
	}
	return list;
    }
}
