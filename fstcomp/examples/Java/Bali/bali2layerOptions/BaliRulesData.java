layer layerArgs;

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
import java.util.regex.*;
import java.util.logging.Level;
import Jakarta.loader.Loader;
import Jakarta.loader.PrefixClassLoader;
import java.io.*;
import java.util.*;
import java.lang.*;

public refines class BaliRulesData {

    /**
     * Collects all non-terminals and rule names into {@link classNames}
     * 
     */ 
    public void collectClasses(StringBuffer className) {

        //Handles each production  
        for ( Iterator p = entrySet().iterator() ; p.hasNext() ; ) {
            //entry is a (name, production) pair

            Map.Entry entry = ( Map.Entry ) p.next() ;
            String ruleName = ( String ) entry.getKey() ;
            ruleClass( ruleName, className) ;

            List productions = ( List ) entry.getValue() ;
            for ( Iterator q = productions.iterator() ; q.hasNext() ; ) {
                Production production = ( Production ) q.next() ;
                productionClass( ruleName, production, className) ;
            }
        }
    }

    /**
     * Collects the ":: ruleName " part for a production
     * 
     */
    private void productionClass( String base, Production prod, StringBuffer className) {

        // The production is a (item)+ list            
        ProductionNode prodNode = ( ProductionNode ) prod ;
        Rewrite rewrite = ( Rewrite ) prodNode.arg [1] ;
        if ( rewrite instanceof SimpleListNode )
            return ;

        // The production is a "item1 (item2)*" list:            
        PrimitiveRewriteNode prn = ( PrimitiveRewriteNode ) rewrite ;
        PrimitiveRewrite pr = ( PrimitiveRewrite ) prn.arg [1] ;
        if ( pr instanceof ComplexListNode )
            return;

        // Named pattern (i.e., ":: NodeName" at production's end):
        PatternNode patternNode = ( PatternNode ) pr ;
        AstNode classNameNode = patternNode.arg[1].arg [0] ;
        if ( classNameNode != null ) {
            ClassNameNode node = ( ClassNameNode ) classNameNode ;
            String name = node.tok[1].getTokenName() ;
            className.append(name);
	    className.append(" ");
            return ;
        }

        // Don't generate a class for unnamed pattern *sequences*
        if ( patternNode.arg[0].arg [0] != null )
            throw new IllegalStateException( "unnamed rule with right-hand sequence: " + base ) ;

        // What remains are productions with a singleton fields.
        // If the field is a rule name, it's an unnamed rule:
        //
        Primitive prim = ( Primitive ) prn.arg [0] ;
        if ( prim instanceof IdentifierNode ) {
            return ;
        }

        return ;
    }

    private void ruleClass( String base, StringBuffer className ) {
        className.append(base);
	className.append(" ");
    }
}
