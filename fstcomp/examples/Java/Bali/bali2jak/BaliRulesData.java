

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Level;

//-----------------------------------------------------------------------//
// Composer-specific collections from the parse tree:
// (adds composer-specific {@link Object#toString()} methods)
//-----------------------------------------------------------------------//

public class BaliRulesData  {

    final public static int COMMENT_COLUMN = 32 ;

    public void generateClasses( File directory, String layerName ) {

        initializeGeneratedSet() ;
        this.layerName = layerName ;

        for ( Iterator p = entrySet().iterator() ; p.hasNext() ; ) {

            Map.Entry entry = ( Map.Entry ) p.next() ;
            String ruleName = ( String ) entry.getKey() ;

            List productions = ( List ) entry.getValue() ;
            for ( Iterator q = productions.iterator() ; q.hasNext() ; ) {
                Production production = ( Production ) q.next() ;
                productionClass( directory, ruleName, production ) ;
            }
        }

        for ( Iterator p = entrySet().iterator() ; p.hasNext() ; ) {
            Map.Entry entry = ( Map.Entry ) p.next() ;
            String ruleName = ( String ) entry.getKey() ;
            ruleClass( directory, ruleName ) ;
        }

        generatedSet.clear() ;

    }

    protected void initializeGeneratedSet() {
        generatedSet.clear() ;
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
    // Methods to generate rule classes:
    // (all use variable "generatedSet" to avoid redundant generation)
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

    final protected Set generatedSet = new HashSet() ;

    private void abstractSubClass( File dir, String base, String sub ) {

        if ( ! generatedSet.contains( sub ) ) {
            ClassBuilder builder = new ClassBuilder() ;
            builder.addModifier( Modifier.ABSTRACT ) ;
            builder.addModifier( Modifier.PUBLIC ) ;
            builder.setLayerName( layerName ) ;
            builder.setClassName( sub ) ;
            builder.setSuperName( base ) ;
            Files.toFile( builder.toString(), new File( dir, sub+".jak" ) ) ;
            generatedSet.add( sub ) ;
        }
    }

    /**
     * Generates a class for productions with named classes.  These are
     * concrete classes with inner data that corresponds to a sequence of
     * non-terminals and terminals.  The productions in these cases are
     * guaranteed to derive as follows:
     * <blockquote>
     * Production
     * -> Rewrite
     * -> PrimitiveRewrite
     * -> PatternNode
     * -> Pattern
     * -> Primitive
     * </blockquote>
     *
     * @layer<bali2jak>
     */
    private void concreteSubClass( File dir, String base, String sub, Production prod ) {

        if ( ! generatedSet.contains( sub ) ) {

            ClassBuilder builder = new ClassBuilder() ;
            builder.addModifier( Modifier.PUBLIC ) ;
            builder.setLayerName( layerName ) ;
            builder.setClassName( sub ) ;
            builder.setSuperName( base ) ;

            List primitives = new ArrayList() ;
            AstNode node = ( PrimitiveRewriteNode ) prod.arg [1] ;
            primitives.add( node.arg [0] ) ;

            AstList list = ( Pattern ) node.arg[1].arg[0].arg [0] ;
            if ( list != null )
                primitives.addAll( list.toList() ) ;

            MethodBuilder setParms = new MethodBuilder() ;
            setParms.addModifier( Modifier.PUBLIC ) ;
            setParms.setReturn( "setParms", sub ) ;

            String code = "arg = new AstNode [ARG_LENGTH] ;" ;
            setParms.endLine().append( code ) ;
            code = "tok = new AstTokenInterface [TOK_LENGTH] ;" ;
            setParms.endLine().append( code ) ;
            setParms.endLine() ;

            MethodBuilder method = new MethodBuilder() ;

            int arg = 0 ;
            int tok = 0 ;
            List interlace = new ArrayList() ;
            for ( Iterator p = primitives.iterator() ; p.hasNext() ; ) {

                Primitive prim = ( Primitive ) p.next() ;

                if ( prim instanceof BaliTokenNode ) {

                    String name = prim.tok[0].getTokenName() ;
                    setParms.addParameter( "tok" + tok, "AstToken" ) ;
                    interlace.add( Boolean.TRUE ) ;

                    code = "tok [" + tok + "] = tok" + tok + " ;" ;
                    setParms.endLine().append( code ) ;
                    setParms.spaceToColumn( COMMENT_COLUMN ) ;

                    setParms.append( "/* " + prim.toString().trim() + " */" ) ;

                    code = "return (AstToken) tok [" + tok + "] ;" ;
                    method.clear() ;
                    method.endLine().append( code ) ;
                    method.addModifier( Modifier.PUBLIC ) ;
                    method.setReturn( "get" + name, "AstToken" ) ;
                    builder.addMethod( method ) ;

                    ++ tok ;
                    continue ;
                }

                if ( prim instanceof IdentifierNode ) {

                    String name = prim.tok[0].getTokenName() ;
                    String type = getType( name ) ;

                    setParms.addParameter( "arg" + arg, type ) ;
                    interlace.add( Boolean.FALSE ) ;

                    code = "arg [" + arg + "] = arg" + arg + " ;" ;
                    setParms.endLine().append( code ) ;
                    setParms.spaceToColumn( COMMENT_COLUMN ) ;
                    setParms.append( "/* " + prim.toString().trim() + " */" ) ;

                    code = "return (" + type + ") arg [" + arg + "] ;" ;
                    method.clear() ;
                    method.endLine().append( code ) ;
                    method.addModifier( Modifier.PUBLIC ) ;
                    method.setReturn( "get" + name, type ) ;
                    builder.addMethod( method ) ;

                    ++ arg ;
                    continue ;
                }

                if ( prim instanceof OptionalNode ) {

                    Terminal term = ( Terminal ) prim.arg [1] ;

                    if ( term instanceof IdentifierNode ) {

                        setParms.addParameter( "arg"+arg, "AstOptNode" ) ;
                        interlace.add( Boolean.FALSE ) ;
                        code = "arg [" + arg + "] = arg" + arg + " ;" ;

                        String name = term.tok[0].getTokenName() ;
                        String type = getType( name ) ;

                        method.clear() ;
                        method.appendLines( "\nAstNode node = arg[" + arg + "].arg [0] ;"
                                                                                                + "\nreturn (node != null) ? (" + type + ") "
                                                                                                + "node : null ;" ) ;

                        method.addModifier( Modifier.PUBLIC ) ;
                        method.setReturn( "get" + name, type ) ;
                        builder.addMethod( method ) ;
                        ++ arg ;

                    }
                    else
                        if ( term instanceof BaliTokenNode ) {

                            setParms.addParameter( "tok"+tok, "AstOptToken" ) ;
                            interlace.add( Boolean.TRUE ) ;
                            code = "tok [" + tok + "] = tok" + tok + " ;" ;

                            String name = term.tok[0].getTokenName() ;
                            method.clear() ;
                            method.endLine() ;
                            method.append( "return (AstToken)"
                                                                                                                + " ((AstNode) tok ["
                                                                                                                + tok
                                                                                                                + "]) . tok [0] ;" ) ;

                            method.addModifier( Modifier.PUBLIC ) ;
                            method.setReturn( "get" + name, "AstToken" ) ;
                            builder.addMethod( method ) ;
                            ++ tok ;

                        }
                        else {
                            setParms.addParameter( "tok"+tok, "AstOptToken" ) ;
                            interlace.add( Boolean.TRUE ) ;
                            code = "tok [" + tok + "] = tok" + arg + " ;" ;
                            ++ tok ;
                        }

                    setParms.endLine().append( code ) ;
                    setParms.spaceToColumn( COMMENT_COLUMN ) ;
                    setParms.append( "/* " + prim.toString().trim() + " */" ) ;

                    continue ;
                }

                if ( prim instanceof StringNode ) {

                    setParms.addParameter( "tok" + tok, "AstToken" ) ;
                    interlace.add( Boolean.TRUE ) ;

                    code = "tok [" + tok + "] = tok" + tok + " ;" ;
                    setParms.endLine().append( code ) ;
                    setParms.spaceToColumn( COMMENT_COLUMN ) ;
                    setParms.append( "/* " + prim.toString().trim() + " */" ) ;

                    ++ tok ;
                    continue ;
                }

                String msg = "class " + prim.getClass().getName() ;
                throw new IllegalStateException( msg ) ;
            }

            // Apparently, some previously written code assumes that
            // "arg[0]" is always a valid reference.  So, the array size of
            // "arg" is bumped up to a minimum of "1".  A note about this
            // is included in the generated source code.
            //
            code =
                            "final public static int ARG_LENGTH = "
                            + ( arg < 1 ? "1 /* Kludge! */" : String.valueOf( arg ) )
                            + " ;" ;
            builder.endLine().append( code ) ;

            code =
                            "final public static int TOK_LENGTH = "
                            + ( tok < 1 ? "1 /* Kludge! */" : String.valueOf( tok ) )
                            + " ;" ;
            builder.endLine().append( code ) ;

            setParms.endLine().endLine() ;
            setParms.append( "InitChildren () ;" ) ;
            setParms.endLine() ;
            setParms.append( "return (" + sub + ") this ;" ) ;
            builder.addMethod( setParms ) ;

            MethodBuilder printOrder = new MethodBuilder() ;
            printOrder.addModifier( Modifier.PUBLIC ) ;
            printOrder.setReturn( "printorder", "boolean[]" ) ;
            code =
                            "return new boolean[] {"
                            + listToString( interlace )
                            + "} ;" ;
            printOrder.endLine().append( code ) ;
            builder.addMethod( printOrder ) ;

            Files.toFile( builder.toString(), new File( dir, sub+".jak" ) ) ;
            generatedSet.add( sub ) ;
        }
    }

    /**
     * Generates a class inheriting from <code>AstList</code>, then returns
     * a {@link ClassBuilder} with the basic setup for an element class
     * inheriting from <code>AstListNode</code>.  If the return value is
     * <code>null</code>, then the element class has previously been
     * generated.
     *
     * @layer<bali2jak>
     */
    private ClassBuilder listClass( File dir, String base ) {

        ClassBuilder build = new ClassBuilder() ;
        build.addModifier( Modifier.PUBLIC ) ;
        build.setLayerName( layerName ) ;

        if ( ! generatedSet.contains( base ) ) {
            build.setClassName( base ) ;
            build.setSuperName( "AstList" ) ;
            Files.toFile( build.toString(), new File( dir, base+".jak" ) ) ;
            generatedSet.add( base ) ;
        }

        base += "Elem" ;
        if ( generatedSet.contains( base ) )
            return null ;

        build.setClassName( base ) ;
        build.setSuperName( "AstListNode" ) ;
        generatedSet.add( base ) ;

        return build ;
    }

    private ClassBuilder listClass( File dir, String base, Primitive prim ) {

        ClassBuilder build = listClass( dir, base ) ;
        if ( build == null )
            return null ;

        if ( ! ( prim instanceof IdentifierNode ) )
            throw new IllegalStateException( "invalid primitive in list" ) ;

        String argType = prim.tok[0].getTokenName() ;
        String elem = base + "Elem" ;

        MethodBuilder method = new MethodBuilder() ;
        method.addModifier( Modifier.PUBLIC ) ;
        method.setReturn( "setParms", elem ) ;
        method.addParameter( "arg0", argType ) ;
        method.endLine().append( "super.setParms (arg0) ;" ) ;
        method.spaceToColumn( COMMENT_COLUMN ) ;
        method.append( "/* " + prim.toString().trim() + " */" ) ;
        method.endLine() ;
        method.append( "return (" + elem + ") this ;" ) ;
        build.addMethod( method ) ;

        method.clear() ;
        method.addModifier( Modifier.PUBLIC ) ;
        method.setReturn( "get" + argType, argType ) ;
        method.endLine().append( "return (" + argType + ") arg [0] ;" ) ;
        build.addMethod( method ) ;

        return build ;
    }

    private void listClasses( File dir, String base, Primitive prim ) {

        ClassBuilder build = listClass( dir, base, prim ) ;
        if ( build == null )
            return ;

        Files.toFile( build.toString(), new File( dir, base+"Elem.jak" ) ) ;
    }

    private void listClasses( File dir, String base, Primitive one, Primitive two ) {

        ClassBuilder build = listClass( dir, base, two ) ;
        if ( build == null )
            return ;

        if ( one.tok.length < 1 )
            throw new IllegalStateException( "missing token in list" ) ;

        String twoType = two.tok[0].getTokenName() ;
        String elem = base + "Elem" ;

        MethodBuilder method = new MethodBuilder() ;
        method.addModifier( Modifier.PUBLIC ) ;
        method.setReturn( "setParms", elem ) ;
        method.addParameter( "tok0", "AstToken" ) ;
        method.addParameter( "arg0", twoType ) ;

        method.endLine().append( "tok = new AstToken [1] ;" ) ;
        method.endLine() ;
        method.append( "tok [0] = tok0 ;" ) ;
        method.spaceToColumn( COMMENT_COLUMN ) ;
        method.append( "/* " + one.toString().trim() + " */" ) ;

        method.endLine().append( "return setParms (arg0) ;" ) ;
        method.spaceToColumn( COMMENT_COLUMN ) ;
        method.append( "/* " + two.toString().trim() + " */" ) ;
        build.addMethod( method ) ;

        if ( one instanceof BaliTokenNode ) {
            String name = one.tok[0].getTokenName() ;
            method.clear() ;
            method.addModifier( Modifier.PUBLIC ) ;
            method.setReturn( "get" + name, "AstToken" ) ;
            method.append( "return (AstToken) tok [0] ;" ) ;
            build.addMethod( method ) ;
        }

        Files.toFile( build.toString(), new File( dir, base+"Elem.jak" ) ) ;
    }

    /**
     * Returns a comma-separated list of strings representing the
     * {@link Object} instances held in a given {@link List}.
     *
     * @layer<bali2jak>
     */
    private String listToString( List objects ) {

        if ( objects.size() < 1 )
            return "" ;

        StringBuffer buffer = new StringBuffer() ;
        buffer.append( objects.get( 0 ) ) ;
        for ( int n = 1 ; n < objects.size() ; ++n )
            buffer.append( ", " ).append( objects.get( n ) ) ;

        return buffer.toString() ;
    }

    private void productionClass( File dir, String base, Production prod ) {

        // The production is a (item)+ list:
        // (generate a List class and a ListElem class)
        //
        ProductionNode prodNode = ( ProductionNode ) prod ;
        Rewrite rewrite = ( Rewrite ) prodNode.arg [1] ;
        if ( rewrite instanceof SimpleListNode ) {
            Primitive prim = ( Primitive ) rewrite.arg [1] ;
            listClasses( dir, base, prim ) ;
            return ;
        }

        // The production is a "item1 (item2)*" list:
        // (generate a List class and a ListElem class)
        //
        PrimitiveRewriteNode prn = ( PrimitiveRewriteNode ) rewrite ;
        PrimitiveRewrite pr = ( PrimitiveRewrite ) prn.arg [1] ;
        if ( pr instanceof ComplexListNode ) {
            Primitive one = ( Primitive ) pr.arg [1] ;
            Primitive two = ( Primitive ) pr.arg [2] ;
            listClasses( dir, base, one, two ) ;
            return ;
        }

        // Named pattern (i.e., ":: NodeName" at production's end):
        // (generate a concrete subclass of base rule class)
        //
        PatternNode patternNode = ( PatternNode ) pr ;
        AstNode classNameNode = patternNode.arg[1].arg [0] ;
        if ( classNameNode != null ) {
            ClassNameNode node = ( ClassNameNode ) classNameNode ;
            String name = node.tok[1].getTokenName() ;
            concreteSubClass( dir, base, name, prod ) ;
            return ;
        }

        // Don't generate a class for unnamed pattern *sequences*:
        // (are these errors?)
        //
        if ( patternNode.arg[0].arg [0] != null )
            throw new IllegalStateException( "unnamed rule with right-hand sequence: " + base ) ;

        // What remains are productions with a singleton fields.
        // If the field is a rule name, it's an unnamed rule:
        //
        Primitive prim = ( Primitive ) prn.arg [0] ;
        if ( prim instanceof IdentifierNode ) {
            String name = prim.tok[0].getTokenName() ;
            abstractSubClass( dir, base, name ) ;
            return ;
        }

        // No classes generated for any other alternative:
        //
        return ;
    }

    private void ruleClass( File dir, String base ) {
        abstractSubClass( dir, "AstNode", base ) ;
    }

    protected String layerName = null ;

}
