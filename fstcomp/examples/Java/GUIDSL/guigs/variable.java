import javax.swing.*;
import java.io.*;

class variable {
    JComponent widget = null;
    boolean hidden = false; // is this variable hidden from view?
    String disp = "";
    String help = null;
    String helpfile = null; //turned this into a string, used to be of type File
    boolean tab; // does this production start a new tab?

    public JComponent setWidget( JComponent w ) {
        widget = w;
        return w;
    }

    public static variable define( String name, int type, gObj g, boolean redef ) {
        variable v = original( name,type, g, redef );
        if ( v!=null ) {
            // trim off leading "_" if present for display
            if ( name.startsWith( "_" ) )
                v.disp = name.substring( 1 );
            else
                v.disp = name;
        }
        return v;
    }

    public void print() {
        original();
        System.out.print( "    hidden=" + hidden + " display: " + disp );
        if ( widget != null )
            System.out.print( " widget: " +widget.getClass().getName() );
    }
}
