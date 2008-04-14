package generated;

import cide.gast.*;
import cide.gparser.*;
import cide.greferences.*;
import java.util.*;

abstract class Statement extends GenASTNode {
  protected Statement(Property[] p, Token firstToken, Token lastToken) { super(p, firstToken, lastToken); }
  protected Statement(Property[] p, IToken firstToken, IToken lastToken) { super(p, firstToken, lastToken); }
}
