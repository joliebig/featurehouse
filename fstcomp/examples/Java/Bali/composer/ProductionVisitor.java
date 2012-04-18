

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.logging.Level;

public class ProductionVisitor extends  AstVisitor {

    public String getRuleName() {
        return ruleName ;
    }

    public void visit( ClassNameNode node ) {
        ruleName = node.tok[1].getTokenName() ;
    }

    public void visit( ComplexListNode node ) {
    /* Method deliberately left empty to increase efficiency! */
    }

    public void visit( SimpleListNode node ) {
    /* Method deliberately left empty to increase efficiency! */
    }

    private String ruleName = null ;

}
