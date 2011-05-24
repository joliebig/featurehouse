

/**
 *This is a JAVA program for writing, opening, saving, editing the documents,
 *has @see ExampleFileFilter class (From SUN� -http://java.sun.com-) for filter the file
 */

/**
 *@King Fahd University of Petroleum and Minerals (KFUPM)
 *@auther: Al-Thubaiti Salah
 *@ICS201 PROJECT
 *@JAVA? Notepad (JNotepad)
 *@version# 2.0
 */

//import the packages for using the classes in them into the program
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

/**
 *A PUBLIC CLASS FOR NOTEPAD.JAVA
 */
public class Notepad extends JFrame {
    //for using the methods in these classes
    public Actions actions = new Actions( this );
    public Center center = new Center( this );

    //declaration of the private variables used in the program
    //create the text area
    private JTextArea textArea;
    //create the Menu Bar that contains the JMenu "filE, ediT, vieW, formaT"
    private JMenuBar Menubar;
    //Create the menu that contains the items
    private JMenu filE, ediT, vieW, formaT;
    //Create the menu items
    private JMenuItem neW, opeN, exiT;
    //Create the Tool Bar that contains the JButton
    private JToolBar toolBar;
    //Create the buttons
    private JButton newButton, openButton;
    //Create Scroll pane (JScrollPane) for the JTextArea
    private JScrollPane scrollpane;

    //for using textArea @Actions.java

    public JTextArea getTextArea() {
        return textArea;
    }
    //Constructor of Notepad
    public Notepad(){
        //set the title for Notepad and set the size for it.
        setTitle( "Untitled - JAVA? Notepad" );
        setSize( 800,600 );

        //get the graphical user interface components display area
        Container cp = getContentPane();
        /**
         *adding the text area,
         *adding the tool bar &
         *adding the scroll pane to the container
         */
        cp.add( textArea = new JTextArea() );
        cp.add( "North", toolBar = new JToolBar( "Tool Bar" ) );
        cp.add( scrollpane = new JScrollPane( textArea ) );

        //for setting the menu bar
        setJMenuBar( Menubar= new JMenuBar() );
        //adding file, edit, view, format to the menu bar
        Menubar.add( filE   = new JMenu( "File" ) );
        Menubar.add( ediT   = new JMenu( "Edit" ) );
        Menubar.add( vieW   = new JMenu( "View" ) );
        Menubar.add( formaT = new JMenu( "Format" ) );


        /**
         *adding neW, opeN, & exiT to the filE Menu,
         *adding a small image icon to the menu item &
         *adding separator between the menu item
         */
        filE.add( neW    = new JMenuItem( "New", new ImageIcon( this.getClass().getResource( "images/new.gif" ) ) ) );
        filE.add( opeN   = new JMenuItem( "Open", new ImageIcon( this.getClass().getResource( "images/open.gif" ) ) ) );
        filE.add( exiT   = new JMenuItem( "Exit" ) ); //, new ImageIcon(this.getClass().getResource("images/exit.gif"))));  -- exit.gif missing


        /**
         *allowing the file   menu to be selected by pressing ALT + F
         *allowing the edit   menu to be selected by pressing ALT + E
         *allowing the view   menu to be selected by pressing ALT + V
         *allowing the format menu to be selected by pressing ALT + O
         */
        filE.setMnemonic( 'f' );
        ediT.setMnemonic( 'e' );
        vieW.setMnemonic( 'v' );
        formaT.setMnemonic( 'o' );


        /**
         *allowing the neW       menu item to be selected by pressing ALT + N
         *allowing the opeN      menu item to be selected by pressing ALT + O
         *allowing the exiT      menu item to be selected by pressing ALT + F4
         */
        neW.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_N, ActionEvent.CTRL_MASK ) );
        opeN.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_O, ActionEvent.CTRL_MASK ) );
        exiT.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_F4, ActionEvent.CTRL_MASK ) );

        /** *adding newButton, openButton, the tool bar, *adding a
        small image icon to the menu item & *adding separator between
        the button */

        toolBar.add( newButton   = new JButton( new ImageIcon( this.getClass().getResource( "images/new.gif" ) ) ) );
        toolBar.add( openButton  = new JButton( new ImageIcon( this.getClass().getResource( "images/open.gif" ) ) ) );


        //adding a tool tip text to the button for descriping the image icon.
        newButton.setToolTipText( "New" );
        openButton.setToolTipText( "Open" );

        /**
         *setting the default close operation to false &
         *using own action (exiT action @Actions.java)
         */
        setDefaultCloseOperation( DO_NOTHING_ON_CLOSE );
        addWindowListener( new WindowAdapter() {
            public void windowClosing( WindowEvent e ) {
                actions.exiT();
            }
        } );
        /**
         *adding action listener for menu item: neW, opeN, exiT,
         *the actions was written @Actions.java
         */
        neW.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.neW();
            }
        } );
        opeN.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.opeN();
            }
        } );

        exiT.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.exiT();
            }
        } );



        /** *adding action listener for the button in the tool bar:
        newButton, openButton, *the actions was written
        @Actions.java */

        newButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.neW();
            }
        } );
        openButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.opeN();
            }
        } );

        /**
         *Setting the Line Wrap & Wrap Style Word features are true
         */
    	 textArea.setLineWrap( true );
    	 textArea.setWrapStyleWord( true );

        /**
         *for making the program at the center,
         *@see Center.java
         */
        center.nCenter();
        show();
    }
    //Main Method
    public static void main( String[] args ) {
        new Notepad();

    }
}
