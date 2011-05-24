package generated;

import cide.gast.*;
import cide.gparser.*;
import cide.greferences.*;
import java.util.*;

public class Units extends GenASTNode {
  public Units(ArrayList<Unit> unit, Token firstToken, Token lastToken) {
    super(new Property[] {
      new PropertyZeroOrMore<Unit>("unit", unit)
    }, firstToken, lastToken);
  }
  public Units(Property[] properties, IToken firstToken, IToken lastToken) {
    super(properties,firstToken,lastToken);
  }
  public ASTNode deepCopy() {
    return new Units(cloneProperties(),firstToken,lastToken);
  }
  public ArrayList<Unit> getUnit() {
    return ((PropertyZeroOrMore<Unit>)getProperty("unit")).getValue();
  }
}
