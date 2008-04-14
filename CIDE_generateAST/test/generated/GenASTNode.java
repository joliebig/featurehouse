package generated;

import cide.gast.*;
import cide.gparser.*;
import cide.greferences.*;
import java.util.*;

public abstract class GenASTNode extends ASTNode {
  public GenASTNode(Property[] p, Token firstToken, Token lastToken) {
    super(p, new WToken(firstToken), new WToken(lastToken));
  }
  public GenASTNode(Property[] p, IToken firstToken, IToken lastToken) {
    super(p, firstToken, lastToken);
  }
  public String toString() {
    return this.getClass().getSimpleName() + " " + this.getStartPosition()
        + "-" + (this.getStartPosition() + this.getLength());
  }
  public String render() {
    SimplePrintVisitor v=new SimplePrintVisitor();
    accept(v);
    return v.getResult();
  }
}
