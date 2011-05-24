

import java.util.*;
import Jakarta.util.*;
import java.io.*;

//******************************* AST_CLASS *************************/

   // root of series of class and interface declarations,
   // in general, we assume only one class or one interface defined.

public class AST_Class {

    // has tree already been prepared?

    public boolean alreadyPrepared( JTSParseTree t ) {
        AstCursor k = new  AstCursor();

        // Step 1: walk through list of Type declarations.  If any of them
        //         are source, return true.  Else false.

        for ( k.FirstElement( this ); k.MoreElement(); k.NextElement() ) {
            if ( k.node instanceof  SourceDecl )
                return true;
        }
        return false;
    }

    // compose AST_Class base tree with extension AST_Class tree 
    // @param etree - extension tree of type AST_Class

    public void compose( AstNode etree,  JTSParseTree base,
                   JTSParseTree ext ) {

        // Step 1: if there is no base file Type Declaration, stop

        if ( base.lastType == null )
            AstNode.fatalError( "base file missing TypeDeclaration -- nothing to extend" );

        // Step 2: make sure there is an extension

        if ( ext.firstType == null )
            AstNode.fatalError( "extension file missing TypeDeclaration -- nothing to extend" );

        // Step 3: it is ok to compose if (1) the types are compatible
        //         and (2) the names of the artifacts are the same.

        if ( base.lastType.getClass() != ext.firstType.getClass() )
            AstNode.fatalError( "attempting to compose files of different types " +
                       base.firstType.getClass().getName() + " " +
                       ext.firstType.getClass().getName() );
        String baseName  = Util2.unmangleId( base.lastType.getName() );
        String extName   = Util2.unmangleId( ext.firstType.getName() );

        if ( !baseName.equals( extName ) )
            AstNode.fatalError( "attempting to compose files with different names " +
                       base.firstType.getName() + " " +
                       ext.firstType.getName() );

        // Step 4: compose them by concatenating the extension declarations
        //         to the base declarations

        add( ( AST_Class ) etree );

        // Step 5: finally, have the firstType in the extension
        //         extend the lastType in the base.  This emulates the
        //         parameter instantion of mixins.
        //         Also, propagate the modifiers of base.lastType
        //         to ext.lastType, and set all other modifiers to be
        //         abstract

        ext.firstType.extensionOf( base.lastType.getAndMangleName() );

        AST_Modifiers m = base.lastType.getModifier();
        base.lastType.setAbstractModifier();
        if ( ext.firstType != ext.lastType )
            ext.firstType.setAbstractModifier();
        ext.lastType.addModifiers( m );
        base.lastType = ext.lastType;
    }

    // can remove into its own layer!

    public boolean isExtension() {
        AstCursor c = new  AstCursor();

        for ( c.FirstElement( this ); c.MoreElement(); c.NextElement() ) {

            if ( c.node instanceof  SourceDecl )
                return ( ( SourceDecl ) c.node ).isExtension();

            if ( c.node instanceof  ModTypeDecl )
                return ( ( ModTypeDecl ) c.node ).isExtension();
        }

        AstNode.fatalError( "file missing Modified Type declaration" );
        return false; // pacify whiney compiler
    }
}
