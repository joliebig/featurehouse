

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
// Specialized sub-classes of AST nodes:
//-----------------------------------------------------------------------//

/**
 * Represents the merger of a "named rule" with an "unnamed rule" where
 * the two both start with the same non-terminal symbol.
 *
 * @layer<bali2javacc>
 */
    
public class Merger extends  ProductionNode {

    public Merger( String ruleName, IdentifierNode nonTerminal ) {
        this.namedNode = null ;
        this.nonTerminal = nonTerminal ;
        this.ruleName = ruleName ;
        this.unnamedNode = null ;
    }

    public ProductionNode getNamed() {
        return namedNode ;
    }

    public IdentifierNode getNonterminal() {
        return nonTerminal ;
    }

    public String getRule() {
        return ruleName ;
    }

    public ProductionNode getUnnamed() {
        return unnamedNode ;
    }

    public boolean isComplete() {
        return namedNode != null && unnamedNode != null ;
    }

    public void setNamed( ProductionNode node ) {

        if ( this.namedNode != null && node != null )
            throw new IllegalStateException( "multiple named rules: " + this ) ;

        this.namedNode = node ;
    }

    public void setUnnamed( ProductionNode node ) {

        if ( this.unnamedNode != null && node != null )
            throw new IllegalStateException( "multiple unnamed rules: " + this ) ;

        this.unnamedNode = node ;
    }

    public String toString() {
        return
                "[rule->" + ruleName
                + ", nonTerminal->" + nonTerminal.tok[0].tokenName()
                + ", unnamed->" + unnamedNode
                + ", named->" + namedNode
                + ']' ;
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

    private ProductionNode namedNode ;
    private ProductionNode unnamedNode ;

    final private IdentifierNode nonTerminal ;
    final private String ruleName ;

}
