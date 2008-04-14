package generated;

import cide.gast.*;
import cide.gparser.*;
import cide.greferences.*;
import java.util.*;

public class MethodDeclaration extends GenASTNode implements ISourceFile {
  public MethodDeclaration(Type type, ASTTextNode text1, ASTStringNode identifier, ParameterList parameterList, NameList nameList, Token firstToken, Token lastToken) {
    super(new Property[] {
      new PropertyOne<Type>("type", type),
      new PropertyZeroOrOne<ASTTextNode>("text1", text1),
      new PropertyOne<ASTStringNode>("identifier", identifier),
      new PropertyZeroOrOne<ParameterList>("parameterList", parameterList),
      new PropertyZeroOrOne<NameList>("nameList", nameList)
    }, firstToken, lastToken);
  }
  public MethodDeclaration(Property[] properties, IToken firstToken, IToken lastToken) {
    super(properties,firstToken,lastToken);
  }
  public ASTNode deepCopy() {
    return new MethodDeclaration(cloneProperties(),firstToken,lastToken);
  }
  public Type getType() {
    return ((PropertyOne<Type>)getProperty("type")).getValue();
  }
  public ASTTextNode getText1() {
    return ((PropertyZeroOrOne<ASTTextNode>)getProperty("text1")).getValue();
  }
  public ASTStringNode getIdentifier() {
    return ((PropertyOne<ASTStringNode>)getProperty("identifier")).getValue();
  }
  public ParameterList getParameterList() {
    return ((PropertyZeroOrOne<ParameterList>)getProperty("parameterList")).getValue();
  }
  public NameList getNameList() {
    return ((PropertyZeroOrOne<NameList>)getProperty("nameList")).getValue();
  }
}
