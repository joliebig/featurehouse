

import java.util.*;
import Jakarta.util.*;
import java.io.*;

//---------- code for locating file, parsing it, and locating 
//---------- declaration to update

public class UnMixinUtil {
    public  AST_Program root = null;
    public  ModTypeDecl location = null;

    public UnMixinUtil() {
        Parser parser;

        // Step 1: open file 

        FileInputStream     inputFile = null;
        try {
            inputFile = new FileInputStream( kernelConstants.globals().unmixin.fileName );
        }
        catch ( Exception e ) {
            AstNode.fatalError( e.getMessage() );
        }

        // Step 2: parse the file, and close it afterwards

        parser =  Parser.getInstance( inputFile ) ;

        try {
            root = (AST_Program) parser.parseAll () ;
        }
        catch ( Exception e ) {
            AstNode.parseError2( "Parsing Exception Thrown in " + 
                      kernelConstants.globals().unmixin.fileName + ": " + e.getMessage() );
        }
        try {
            inputFile.close();
        }
        catch ( Exception e ) {
            AstNode.fatalError( "can't close " + kernelConstants.globals().unmixin.fileName );
        }

        // Step 3: find first ModTypeDecl -- that should be the only
        //         such declaration in the file itself

        AstCursor c = new  AstCursor();
        for ( c.First( root ); c.More(); c.PlusPlus() ) {
            if ( c.node instanceof  ModTypeDecl ) {
                if ( location == null )
                    location = ( ModTypeDecl ) c.node;
                else
                    AstNode.fatalError( "Too many ModTypeDecl statements in " + 
                             kernelConstants.globals().unmixin.fileName );
            }
            if ( c.node instanceof  TypeDeclaration )
                c.Sibling();
        }
        if ( location == null )
            AstNode.fatalError( "No ModTypeDecl statements in " + kernelConstants.globals().unmixin.fileName );
    }

    // propagate output to file itself

    public void output() {
        String fileName;
        String dirName;

        // Step 1: create AstProperties for outputting AST

        int i = kernelConstants.globals().unmixin.fileName.lastIndexOf( File.separatorChar );
        if ( i>=0 ) {
            fileName = kernelConstants.globals().unmixin.fileName.substring( i+1 );
            dirName =  kernelConstants.globals().unmixin.fileName.substring( 0,i );
        }
        else {
            fileName = kernelConstants.globals().unmixin.fileName;
            dirName  = ".";
        }

        AstProperties props =  AstProperties.open( dirName, fileName );
        // Step 3: output to file

        root.print( props );
        PrintWriter pw = ( PrintWriter ) props.getProperty( "output" );
        pw.println( "" ); // finish the file with a complete line
        pw.close();

        // Step 4: now if verbose is selected, announce file has been
        //         updated.
                        
        if ( Main.verbose )
            System.err.println( "changes propagated to file " + 
                                                       dirName + File.separator + fileName );
    }
}
