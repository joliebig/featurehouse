

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import java.io.*;

public class UmInterDecl {

    // this method is called on parse tress of the SoUrCe file
    // the idea is for this method to propagate the contents of
    // this UmInterDecl AST to the file referenced in the SoUrCe file.

    public void propagateChanges() {
        String myName = null;
        String referencedName = null;

        // Step 1: fetch the referenced file, parse it, and locate
        //         the lone ModTypeDecl declaration

        UnMixinUtil u = new  UnMixinUtil();

        // Step 2: do some error checking.  The ModTypeDecl must reference
        //         a UmInterDecl or UmodIntExt with the same QName as 
        //         our parse tree

        //         note u.location is of type ModTypeDecl, whose first
        //         argument is an UnmodifiedTypeDeclaration -- in our
        //         case, it should be a UmInterDecl

        if ( kernelConstants.globals().unmixin.base ) {
            // testing for UmInterDecl

            if ( u.location.arg[1] instanceof  UmInterDecl ) {
                referencedName = u.location.arg[1].arg[0].tok[0].tokenName();
                myName = arg[0].tok[0].tokenName();
                if ( !referencedName.equals( myName ) )
                    AstNode.fatalError( tok[0],
                                             "expecting interface " + myName + " but got "
                     + referencedName +
                     " file " + 
                                              kernelConstants.globals().unmixin.fileName + 
                                             " not updated" );
            }
            else
                AstNode.fatalError( tok[0],
                           "expecting UmInterDecl but got " +
                   u.location.arg[1].getClass().getName() + 
                   " file " +  kernelConstants.globals().unmixin.fileName +
                                   " not updated" );
        }
        else {
            // testing for UmodIntExt

            if ( u.location.arg[1] instanceof  Ute &&
                u.location.arg[1].arg[0] instanceof  UmodIntExt ) {
                referencedName = u.location.arg[1].arg[0].arg[0].tok[0].tokenName();
                myName = arg[0].tok[0].tokenName();
                if ( !referencedName.equals( myName ) )
                    AstNode.fatalError( tok[0],
                                                 "expecting interface extension" + myName + 
                         " but got " + referencedName +
                         " file " +  kernelConstants.globals().unmixin.fileName + 
                                                             " not updated" );
            }
            else
                AstNode.fatalError( tok[0],
                             "expecting Ute (or rather UmodIntExt) but got " +
                     u.location.arg[1].getClass().getName() + 
                     " file " +  kernelConstants.globals().unmixin.fileName + 
                                     " not updated" );
        }
  
        // Step 3: we're ready to make the changes!
        //         first, remove the name of the interface from the
        //         extends list.  Remember that it might be there because
        //         this declaration may have refined (extended) an interface

        AST_TypeNameList l = null;
        if ( arg[1].arg[0] !=null )
            l = ( ( AST_TypeNameList ) arg[1].arg[0].arg[0] ).removeName( myName );
        boolean updated = false;
        if ( kernelConstants.globals().unmixin.base )
            updated = ( ( UmInterDecl ) u.location.arg[1] ).
               propagateChanges( l, ( InterfaceMemberDeclarations ) arg[2].arg[0] );
        else
            updated = ( ( UmodIntExt ) u.location.arg[1].arg[0] ).
               propagateChanges( l, ( InterfaceMemberDeclarations ) arg[2].arg[0] );

        // Step 4: last thing we do -- propagate the changes back to the file

        if ( updated )
            u.output();
    }

    // this method is called on the original AST, not the one with
    // SoUrCe declarations

    public boolean propagateChanges( AST_TypeNameList l, 
                                     InterfaceMemberDeclarations i ) {

        // Just replace corresponding extension and body declarations

        boolean updated;
        updated = pc( arg[1], l, i );
        if ( i!=null && ! ( arg[2].toString().trim().equals( i.toString().trim() ) ) ) {
            arg[2].Replace( i );
            updated = true;
        }
        return updated;
    }

    public static boolean pc( AstNode n,  // points to [IntExtClauseC]
                              AST_TypeNameList l,
                              InterfaceMemberDeclarations i ) {

        boolean updated = false;
        IntExtClauseC c = null;
        if ( l != null ) {
            c = new  IntExtClauseC().setParms( new  AstToken().setParms( " ","extends", 0 ), l );
            String com = n.getComment();
            if ( com != null )
                c.setComment( com );
            if ( ! ( c.toString().trim().equals( n.toString().trim() ) ) ) {
                n.Replace( c );
                updated = true;
            }
        }
        else {
            if ( !n.toString().trim().equals( "" ) ) {
                n.Delete();
                updated = true;
            }
        }
        return updated;
    }
}
