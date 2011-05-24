package generated;

import cide.gast.*;
import cide.gparser.*;
import cide.greferences.*;
import java.util.*;

abstract class Type extends GenASTNode {
  protected Type(Property[] p, Token firstToken, Token lastToken) { super(p, firstToken, lastToken); }
  protected Type(Property[] p, IToken firstToken, IToken lastToken) { super(p, firstToken, lastToken); }
}
