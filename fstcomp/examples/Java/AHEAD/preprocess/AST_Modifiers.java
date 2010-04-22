

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

//-------------------- MODIFIER LIST COMPOSITION ------------------

   /** production:

   AST_Modifiers<br>
  : ( Modifier )+<br>
  ;<br>

   Modifier<br>
      : ABSTRACT::ModAbstract<br>
      | FINAL::ModFinal<br>
      | PUBLIC::ModPublic<br>
      | PROTECTED::ModProtected<br>
      ...<br>
      ;<br>
   *
   * @layer<preprocess>
   */

public class AST_Modifiers {

    // search modifier list of base for modifier x.
    // return true if present
 
    public boolean findModifier( Modifier x ) {
        return findModifier( x.GetName() );
    }

    public boolean findModifier( String mod ) {
        AstCursor     c = new  AstCursor();
        AST_Modifiers b = ( AST_Modifiers ) this;

        for ( c.FirstElement( b ); c.MoreElement(); c.NextElement() ) {
            if ( ( ( Modifier ) ( c.node ) ).GetName().equals( mod ) )
                return true;
        }
        return false;
    }

    public void remModifier( Modifier x ) {
        AstCursor     c = new  AstCursor();
        AST_Modifiers b = ( AST_Modifiers ) this;
        String             xName = x.GetName();

        for ( c.FirstElement( b ); c.MoreElement(); c.NextElement() ) {
            if ( ( ( Modifier ) ( c.node ) ).GetName().equals( xName ) )
                c.Delete();
        }
    }

    /** compose base AST_Modifier tree with extension AST_Modifier tree 
     * @layer<preprocess>
     */

    public void compose( AstNode etree ) {
        // Note: I'm cloning the extension tree, as I will be modifying
        //       it.  If I didn't do this, all sorts of wierd problems
        //       result

        AST_Modifiers x = ( AST_Modifiers ) etree.clone();
        AST_Modifiers b = ( AST_Modifiers ) this;

        // Step 1: foreach element e of extension modifiers list
        //         see if e is already present on base modifiers list.
        //         if so, delete it from the extension list

        AstCursor c = new  AstCursor();
        for ( c.FirstElement( x ); c.MoreElement(); c.NextElement() ) {
            if ( findModifier( ( Modifier ) c.node ) )
                c.Delete();
        }

        // Step 2: now add the truncated modifier list to the base list

        b.add( x );
    }

    public void addModifier( Modifier m ) {

        // Step 1: if modifier already is present, do nothing

        if ( findModifier( m ) )
            return;

        // Step 2: create a list of length one with m on it

        AST_Modifiers l = new  AST_Modifiers();
        l.add( new  AST_ModifiersElem().setParms( m ) );

        // Step 3: add the modifier to the list

        this.add( l );
    }
}
