package generated;

import cide.gast.*;
import cide.gparser.*;
import cide.greferences.*;
import java.util.*;

public class IfStatement extends GenASTNode {
  public IfStatement(ASTStringNode expression, Statement statement, Statement statement1, Token firstToken, Token lastToken) {
    super(new Property[] {
      new PropertyOne<ASTStringNode>("expression", expression),
      new PropertyOne<Statement>("statement", statement),
      new PropertyZeroOrOne<Statement>("statement1", statement1)
    }, firstToken, lastToken);
  }
  public IfStatement(Property[] properties, IToken firstToken, IToken lastToken) {
    super(properties,firstToken,lastToken);
  }
  public ASTNode deepCopy() {
    return new IfStatement(cloneProperties(),firstToken,lastToken);
  }
  public ASTStringNode getExpression() {
    return ((PropertyOne<ASTStringNode>)getProperty("expression")).getValue();
  }
  public Statement getStatement() {
    return ((PropertyOne<Statement>)getProperty("statement")).getValue();
  }
  public Statement getStatement1() {
    return ((PropertyZeroOrOne<Statement>)getProperty("statement1")).getValue();
  }
}
