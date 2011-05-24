package generated;

import cide.gast.*;
import cide.gparser.*;
import cide.greferences.*;
import java.util.*;

public class ParameterList extends GenASTNode {
  public ParameterList(ArrayList<Parameter> parameter, Token firstToken, Token lastToken) {
    super(new Property[] {
      new PropertyList<Parameter>("parameter", parameter)
    }, firstToken, lastToken);
  }
  public ParameterList(Property[] properties, IToken firstToken, IToken lastToken) {
    super(properties,firstToken,lastToken);
  }
  public ASTNode deepCopy() {
    return new ParameterList(cloneProperties(),firstToken,lastToken);
  }
  public ArrayList<Parameter> getParameter() {
    return ((PropertyList<Parameter>)getProperty("parameter")).getValue();
  }
}
