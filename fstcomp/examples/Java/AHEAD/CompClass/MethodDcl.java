

import java.util.*;
import Jakarta.util.FixDosOutputStream;
import Jakarta.util.Util2;
import java.io.*;

public class MethodDcl {

    public void mangleBaseMethod() {
        AST_Modifiers baseModifiers = ( AST_Modifiers ) arg[0].arg[0];

        setName( mangledName );

        // remember comment in front of method declaration
        // and nullify it for now

        String savedComment = this.getComment();
        this.setComment( " " );

        addModifier( new  ModFinal().setParms( new  AstToken().setParms( " ", "final", 0 ) ) );

        // remove "new" or "overrides" modifier from base

        if ( baseModifiers!=null ) {
            baseModifiers.remModifier( MethodDeclaration.mn );
            baseModifiers.remModifier( MethodDeclaration.mo );
        }

        // restore comment associated with method -- place at front
        // of method declaration.
        this.setComment( savedComment );
    }

    public String GetName() {
        return ( ( MethodDeclarator ) this.arg[2] ).GetName();
    }

    public void cleanUpBase( AstCursor k, Hashtable he ) {

        if ( isReferenced && isOverridden ) {
            // propagate the modifiers and throws clause of the base
            // to the extension.

            ( overriddenBy.arg[0] ).compose( ( AstNode ) arg[0].clone() );
            ( overriddenBy.arg[3] ).compose( ( AstNode ) arg[3].clone() );

            // mangle the base name, and add final to the declaration

            mangleBaseMethod();
        }
        else
            if ( isReferenced && !isOverridden ) {
                // create a call to self and add it before the original base method
            
                String call = call2Self( GetName(), mangledName );
                AstList l =  AST_FieldDecl.MakeAST( call );

                // we need to identify this code with some layer
                // but since we are manufacturing this method and it
                // doesn't really belong to a layer, we'll invent a layer name

                l.setSource( kernelConstants.globals().compclass.ManufacturedName++ + "" );

                k.AddBefore( l );

                // mangle original base method

                mangleBaseMethod();
            }
            else
                if ( !isReferenced && isOverridden ) {
                    // just delete the base method -- no one references it
                    // and it is overridden
                    k.Delete();
                }
                else
                    if ( !isReferenced && !isOverridden ) {
                    // do nothing --leave as is
                    }
    }

    public String call2Self( String unmangled, String mangled ) {
        String modifiers = arg[0].toString();
        String typeName  = arg[1].toString();
        String methdecl  = ( ( MethodDeclarator ) arg[2] )
                            .selfDeclarator( unmangled );
        String selfcall  = ( ( MethodDeclarator ) arg[2] )
                            .selfCall( mangled );
        String throwsc   = arg[3].toString();
        boolean isVoid = false;

        if ( arg[1] instanceof  PrimType && 
             arg[1].arg[0] instanceof VoidTyp )
            isVoid = true;
        
        return "\n" + modifiers + typeName + methdecl + throwsc +
              "{ " + ( isVoid?"":"return " ) + selfcall + "; }";
    }

    public String signature() {
        return ( ( MethodDeclarator ) arg[2] ).signature();
    }
}
