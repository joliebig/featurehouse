package generated;

import cide.gast.*;
import cide.gparser.*;
import cide.greferences.*;
import java.util.*;

public class Unit2 extends Unit {
  public Unit2(NonTerminal nonTerminal, Token firstToken, Token lastToken) {
    super(new Property[] {
      new PropertyOne<NonTerminal>("nonTerminal", nonTerminal)
    }, firstToken, lastToken);
  }
  public Unit2(Property[] properties, IToken firstToken, IToken lastToken) {
    super(properties,firstToken,lastToken);
  }
  public ASTNode deepCopy() {
    return new Unit2(cloneProperties(),firstToken,lastToken);
  }
  public NonTerminal getNonTerminal() {
    return ((PropertyOne<NonTerminal>)getProperty("nonTerminal")).getValue();
  }
}
