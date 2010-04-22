layer bali2javacc;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Reader;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.Properties;
import java.util.Random;
import java.util.Set;
import java.util.logging.Level;

//-----------------------------------------------------------------------//
// Composer-specific collections from the parse tree:
// (adds composer-specific {@link Object#toString()} methods)
//-----------------------------------------------------------------------//

public refines class BaliRulesData {

    /**
     * Generates JavaCC productions for each Bali rule:
     *
     * @layer<bali2javacc>
     */
    public String toString() {

        List keys = new ArrayList( keySet() ) ;

        StringBuffer buffer = new StringBuffer() ;
        buffer.append( rule2string( getStartName() ) ) ;
        keys.remove( getStartName() ) ;
            
        Collections.sort( keys ) ;
        for ( Iterator p = keys.iterator() ; p.hasNext() ; ) {
            buffer.append( Main.LINE_SEPARATOR ) ;
            buffer.append( Main.LINE_SEPARATOR ) ;
            buffer.append( rule2string( ( String ) p.next() ) ) ;
        }

        return buffer.toString() ;
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

    private String assignmentCode( CodeBuffer code, Primitive prim, Variables variables ) {

        if ( prim instanceof BaliTokenNode ) {
            String var = variables.getFree( "Token" ) ;
            code.append( var + "=<" + prim.tok[0].tokenName() + '>' ) ;
            return "t2at(" + var + ')' ;
        }

        if ( prim instanceof IdentifierNode ) {
            String name = prim.tok[0].tokenName() ;
            String type = getType( name ) ;
            String var = variables.getFree( type ) ;
            code.append( var + '=' + name + "()" ) ;
            return var ;
        }

        if ( prim instanceof OptionalNode ) {

            code.append( '[' ) ;

            if ( prim.arg[0].arg [0] != null ) {
                LookaheadNode look = ( LookaheadNode ) prim.arg[0].arg [0] ;
                code.append( "LOOKAHEAD(" + look.arg [0] + ") " ) ;
            }

            String var = assignmentCode( code, ( Primitive ) prim.arg [1], variables ) ;

            code.append( ']' ) ;
            return "opt(" + var + ')' ;
        }

        if ( prim instanceof StringNode ) {
            String var = variables.getFree( "Token" ) ;
            code.append( var + '=' + prim.tok[0].tokenName() ) ;
            return "t2at(" + var + ')' ;
        }

        throw new IllegalStateException( "unexpected Primitive: " + prim ) ;
    }

    private String compact( String text ) {
        return text.trim().replaceAll( "\\s+", " " ) ;
    }

    private String complexList( String rule, Primitive item, ComplexListNode rest, Variables vars ) {

        CodeBuffer code = new CodeBuffer() ;
        String result = vars.declare( rule, "list", "new " + rule + " ()" );

        String var = assignmentCode( code, item, vars ) ;
        code.endLine().append( "{" + result + ".add (new " ) ;
        code.append( rule + "Elem().setParms (" + var + ")) ;}" ) ;

        code.endLine().append( '(' ) ;

        // Handle LOOKAHEAD:
        //
        code.endLine().indent() ;
        if ( rest.arg[0].arg [0] != null ) {
            LookaheadNode node = ( LookaheadNode ) rest.arg[0].arg [0] ;
            code.append( "LOOKAHEAD(" + node.arg [0] + ") " ) ;
            code.endLine() ;
        }

        vars.reset() ;

        Primitive one = ( Primitive ) rest.arg [1] ;
        String varOne = assignmentCode( code, one, vars ) ;
        code.endLine() ;

        Primitive two = ( Primitive ) rest.arg [2] ;
        String varTwo = assignmentCode( code, two, vars ) ;

        code.endLine().append( "{" + result + ".add (new " ) ;
        code.append( rule + "Elem().setParms (" + varOne + ", " + varTwo + ")) ;}" ) ;

        code.endLine().outdent().append( ")*" ) ;

        code.endLine().append( "{return " + result + " ;}" ) ;
        return code.toString() ;
    }

    private String merger2string( Merger merger, Variables variables ) {

        ProductionNode namedNode = merger.getNamed() ;
        IdentifierNode nonTerminal = merger.getNonterminal() ;
        String rule = merger.getRule() ;
        ProductionNode unnamedNode = merger.getUnnamed() ;

        CodeBuffer code = new CodeBuffer() ;

        code.append( "// Merged productions from rule " + rule ) ;
        code.endLine() ;
        code.append( "// (*) " + compact( unnamedNode.toString() ) ) ;
        code.endLine() ;
        code.append( "// (*) " + compact( namedNode.toString() ) ) ;
        code.endLine() ;
        code.append( "// " ) ;
        code.endLine() ;

        List args = new ArrayList() ;
        args.add( assignmentCode( code, nonTerminal, variables ) ) ;

        code.endLine().append( '[' ).endLine().indent() ;
        code.append( "LOOKAHEAD(2)" ) ;

        // Refactor with similar code from "namedRule" method?
        //
        Pattern pat = ( Pattern ) namedNode.arg[1].arg[1].arg[0].arg [0] ;
        List list = ( pat != null ) ? pat.toList() : Collections.EMPTY_LIST ;
        for ( Iterator p = list.iterator() ; p.hasNext() ; ) {
            Primitive prim = ( Primitive ) p.next() ;
            code.endLine() ;
            args.add( assignmentCode( code, prim, variables ) ) ;
        }

        StringBuffer arg = new StringBuffer() ;
        Iterator p = args.iterator() ;
        if ( p.hasNext() )
            arg.append( p.next().toString() ) ;
        while ( p.hasNext() ) {
            arg.append( ", " ) ;
            arg.append( p.next().toString() ) ;
        }

        String node =
                namedNode.arg[1].arg[1].arg[1].arg[0].tok[1].tokenName() ;

        code.endLine().append( "{return new " + node + "().setParms" ) ;
        code.append( "(" + arg + ") ;}" ) ;
        code.endLine().outdent().append( ']' ) ;

        String var = ( String ) args.get( 0 ) ;
        code.endLine().append( "{return (" + rule + ") " + var + " ;}" ) ;
        return code.toString() ;
    }

    private String namedRule( String rule,Primitive item,Pattern pat,String node,Variables vars ) {

        CodeBuffer code = new CodeBuffer() ;
        List args = new ArrayList() ;
        args.add( assignmentCode( code, item, vars ) ) ;

        List patList =
                ( pat != null ) ? pat.toList() : Collections.EMPTY_LIST ;

        for ( Iterator p = patList.iterator() ; p.hasNext() ; ) {
            Primitive prim = ( Primitive ) p.next() ;
            code.endLine() ;
            args.add( assignmentCode( code, prim, vars ) ) ;
        }

        StringBuffer arg = new StringBuffer() ;
        Iterator p = args.iterator() ;
        if ( p.hasNext() )
            arg.append( p.next().toString() ) ;
        while ( p.hasNext() ) {
            arg.append( ", " ) ;
            arg.append( p.next().toString() ) ;
        }

        code.endLine().append( "{return new " + node + "().setParms" ) ;
        code.append( "(" + arg + ") ;}" ) ;
        return code.toString() ;
    }

    private String production2string( String rule, ProductionNode production, Variables variables ) {

        if ( production instanceof Merger )
            return merger2string( ( Merger ) production, variables ) ;

        CodeBuffer code = new CodeBuffer() ;

        // Generate leading LOOKAHEAD, if any:
        //
        AstNode node = production.arg[0].arg [0] ;
        if ( node != null )
            code.append( "LOOKAHEAD(" + node.arg [0] + ") " ) ;

        // Separate the production types and generate code for each:
        //
        Rewrite rewrite = ( Rewrite ) production.arg [1] ;

        // Handle a "(item)+" list:
        //
        if ( rewrite instanceof SimpleListNode ) {
            SimpleListNode list = ( SimpleListNode ) rewrite ;
            code.append( simpleList( rule, list, variables ) ) ;
            return code.toString() ;
        }

        // Handle a "item1 (item2)*" list:
        //
        Primitive item1 = ( Primitive ) rewrite.arg [0] ;
        PrimitiveRewrite rest = ( PrimitiveRewrite ) rewrite.arg [1] ;
        if ( rest instanceof ComplexListNode ) {
            ComplexListNode list = ( ComplexListNode ) rest ;
            code.append( complexList( rule, item1, list, variables ) ) ;
            return code.toString() ;
        }

        // Handle a named rule (i.e., a ":: NodeName" at production's end):
        //
        node = rest.arg[0].arg [0] ;
        Pattern pattern = ( node != null ) ? ( Pattern ) node : null ;
        if ( rest.arg[1].arg [0] != null ) {
            String name = rest.arg[1].arg[0].tok[1].tokenName() ;
            code.append( namedRule( rule,item1,pattern,name,variables ) ) ;
            return code.toString() ;
        }

        // Currently, unnamed rules with right-hand sequences are errors:
        //
        if ( pattern != null )
            throw new IllegalStateException( "unnamed rule with right-hand sequence: " + rule ) ;

        // What remains are productions with singleton right-hand sides.
        // If the field is a rule name, it's an unnamed rule:
        //
        if ( item1 instanceof IdentifierNode ) {
            code.append( unnamedRule( rule, ( IdentifierNode ) item1, variables ) ) ;
            return code.toString() ;
        }

        throw new IllegalStateException( "singleton production without a name: " + rule ) ;
    }

    /**
     * Examines a {@link List} of productions for special cases where two
     * or more productions should be combined, such as left-recursion via
     * an "unnamed rule", and modifies the {@link List} to add the combined
     * productions while removing their source productions.
     *
     * @layer<bali2javacc>
     */
    private List productionsMerge( String rule ) {

        List productions = ( List ) get( rule ) ;
        if ( productions.size() < 2 )
            return productions ;

        // Extract productions to merge:
        //
        Map mergers = new HashMap() ;
        for ( Iterator p = productions.iterator() ; p.hasNext() ; ) {

            ProductionNode pn = ( ProductionNode ) p.next() ;
            if ( pn.arg[0].arg [0] != null ) // Disallow LOOKAHEAD.
                continue ;

            Rewrite rewrite = ( Rewrite ) pn.arg [1] ;
            if ( ! ( rewrite instanceof PrimitiveRewriteNode ) )
                continue ;
            if ( ! ( rewrite.arg [0] instanceof IdentifierNode ) )
                continue ;

            PrimitiveRewrite pr = ( PrimitiveRewrite ) rewrite.arg [1] ;
            if ( ! ( pr instanceof PatternNode ) )
                continue ;
            if ( pr.arg[0].arg [0] != null && pr.arg[1].arg [0] == null )
                continue ;

            IdentifierNode identifier = ( IdentifierNode ) rewrite.arg [0] ;
            String nonTerminal = identifier.tok[0].tokenName() ;
            Merger merger = ( Merger ) mergers.get( nonTerminal ) ;
            if ( merger == null ) {
                merger = new Merger( rule, identifier ) ;
                mergers.put( nonTerminal, merger ) ;
            }

            if ( pr.arg[1].arg [0] != null )
                merger.setNamed( pn ) ;
            else
                if ( pr.arg[0].arg [0] == null )
                    merger.setUnnamed( pn ) ;
        }

        if ( mergers.size() < 1 )
            return productions ;

        // For complete mergers, modify the {@link List} of productions:
        //
        List newProductions = new ArrayList( productions ) ;
        for ( Iterator p = mergers.keySet().iterator() ; p.hasNext() ; ) {
            String nonTerminal = ( String ) p.next() ;
            Merger merger = ( Merger ) mergers.get( nonTerminal ) ;
            if ( merger.isComplete() ) {
                ProductionNode named = merger.getNamed() ;
                ProductionNode unnamed = merger.getUnnamed() ;
                newProductions.remove( unnamed ) ;
                int namedIndex = newProductions.indexOf( named ) ;
                newProductions.set( namedIndex, merger ) ;
            }
        }

        return newProductions ;
    }

    private String rule2string( String rule ) {

        CodeBuffer code = new CodeBuffer() ;
        Variables variables = new Variables() ;

        Iterator p = productionsMerge( rule ).iterator() ;
        if ( ! p.hasNext() )
            throw new IllegalStateException( "no productions: " + rule ) ;

        ProductionNode node = ( ProductionNode ) p.next() ;
        code.append( production2string( rule, node, variables ) ) ;
        while ( p.hasNext() ) {
            variables.reset() ;
            node = ( ProductionNode ) p.next() ;
            code.endLine().append( '|' ).endLine() ;
            code.append( production2string( rule,node,variables ).trim() ) ;
        }

        String clauses = code.toString().trim() ;
        code.clear().append( rule + ' ' + rule + " () : {" ) ;
        code.endLine().indent() ;

        String declarations = variables.toString() ;
        if ( declarations.length() > 0 )
            code.appendLines( declarations ).endLine() ;

        code.outdent().append( "} {" ) ;
        code.endLine().indent().appendLines( clauses ) ;
        code.endLine().outdent().append( '}' ) ;

        return code.toString() ;
    }

    private String simpleList( String rule, SimpleListNode list, Variables vars ) {

        CodeBuffer code = new CodeBuffer() ;
        String result = vars.declare( rule, "list", "new " + rule + " ()" );

        // Handle LOOKAHEAD:
        //
        code.append( '(' ).endLine().indent() ;
        if ( list.arg[0].arg [0] != null ) {
            LookaheadNode node = ( LookaheadNode ) list.arg[0].arg [0] ;
            code.append( "LOOKAHEAD(" + node.arg [0] + ") " ) ;
            code.endLine() ;
        }

        String var =
                assignmentCode( code, ( Primitive ) list.arg[1], vars ) ;

        code.endLine().append( "{" + result + ".add (new " ) ;
        code.append( rule + "Elem().setParms (" + var + ")) ;}" ) ;
        code.endLine().outdent().append( ")+" ) ;

        code.endLine().append( "{return " + result + " ;}" ) ;
        return code.toString() ;
    }

    private String unnamedRule( String rule, IdentifierNode item, Variables variables ) {

        CodeBuffer code = new CodeBuffer() ;
        String var = assignmentCode( code, item, variables ) ;
        code.endLine().append( "{return (" + rule + ") " + var + " ;}" ) ;

        return code.toString() ;
    }

}
