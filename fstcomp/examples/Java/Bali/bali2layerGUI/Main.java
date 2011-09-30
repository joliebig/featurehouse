

import Jakarta.SwingUtils.*;
import Jakarta.util.Find;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.LinkedList;
import java.util.Collections;
import java.util.Comparator;
import javax.swing.JFileChooser;
import java.lang.Integer;
import java.io.*;
import java.util.*;
import java.util.regex.*;
import java.lang.*;

public class Main {
    public Object driver( String[] args ) throws Throwable {
        JFrame f= new JFrame( "bali2Ref" );
        f.getContentPane().add( new Gui() );
        Dimension d = f.getPreferredSize();
        f.setSize( d.width/2, d.height/2 );
        f.addWindowListener( new WindowAdapter() {
            public void windowClosing( WindowEvent e ) {
                System.exit( 0 );
            }
        } );
        f.pack();
        f.setVisible( true );
        return null;
    }
}
