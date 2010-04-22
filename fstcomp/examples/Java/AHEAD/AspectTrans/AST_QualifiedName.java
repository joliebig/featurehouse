

public class AST_QualifiedName        {

    // wherever we find the string $pack as the prefix of 
    // an ast_qualified name, we replace $pack with the package name
    // otherwise take the default reduction action
    
    public void reduce2java( AstProperties props ) {

       if (arg[0].arg[0] instanceof NameId) {
          // should always be true -- but there are some odd AST
          // manipulations where a generated AST is not correct

          String pn = getPrefixName();
          if (pn.equals(AspectStm.packID)) {
              String newName = (String) props.getProperty(AspectStm.packID);
              setPrefixName(newName);
          }
       }
       original(props);
    }
}
