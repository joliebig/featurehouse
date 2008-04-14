package generated;

import cide.gast.*;
import cide.gparser.*;
import cide.greferences.*;
import java.util.*;

public class NameList extends GenASTNode {
  public NameList(ArrayList<Name> name, Token firstToken, Token lastToken) {
    super(new Property[] {
      new PropertyList<Name>("name", name)
    }, firstToken, lastToken);
  }
  public NameList(Property[] properties, IToken firstToken, IToken lastToken) {
    super(properties,firstToken,lastToken);
  }
  public ASTNode deepCopy() {
    return new NameList(cloneProperties(),firstToken,lastToken);
  }
  public ArrayList<Name> getName() {
    return ((PropertyList<Name>)getProperty("name")).getValue();
  }
}
