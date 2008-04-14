package generated;

import cide.gast.*;
import cide.gparser.*;
import cide.greferences.*;
import java.util.*;

abstract class Unit extends GenASTNode {
  protected Unit(Property[] p, Token firstToken, Token lastToken) { super(p, firstToken, lastToken); }
  protected Unit(Property[] p, IToken firstToken, IToken lastToken) { super(p, firstToken, lastToken); }
}
