

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

public class Gui extends JPanel {

    public  void initConstants() {}

    // declare and initialize atomic components here

    JTextField         layerName;
    JTextField         productionName;
    JTextField         fileName;
    JTextField         methodSignature;
    JRadioButton      fileInput;
    JRadioButton      stdInput;
	 JCheckBox         includeKernel;
    JButton            generate;
    JButton            addMethodButton;
    JButton            removeMethodButton;
    JButton            openFileButton;
    ButtonGroup     buttonGroup;
    JList              methodList;
    JFileChooser      fileChooser;
    JScrollPane       scrollPane;
    JPanel         textInput;
    JPanel         optionInput;
    JPanel         methodInput;
    JTable         parameterTable;
    JScrollPane     pane;
    String         inputFileName;
    String         DefaultFileName = " ";
    JPanel         parentPanel;
    DefaultListModel     model;

    public Gui() {

        generate            = new JButton( "Generate!" );
        addMethodButton     = new JButton( "Add       " );
        removeMethodButton = new JButton( "Remove  " );
        removeMethodButton.setEnabled( false );
        openFileButton = new JButton( "Browse" );
        openFileButton.setToolTipText( "open a file" );

        parentPanel = this;

        this.setLayout( new BorderLayout() );
        layerName = new JTextField( 10 );
        layerName.setText( Find.currentDirectory() );
        layerName.setBorder( BorderFactory.createTitledBorder( "Layer Name" ) );

        productionName = new JTextField( 10 );
        productionName.setText( "AST_Program" );
        productionName.setBorder( BorderFactory.createTitledBorder( "Name of start Production" ) );

        methodSignature = new JTextField( 10 );
        methodSignature.setText( "void execute()" );
        methodSignature.setBorder( BorderFactory.createTitledBorder( "Method Signature" ) );

        fileName = new JTextField( 20 );
        fileName.setText( "grammar.b" );
        fileName.setBorder( BorderFactory.createTitledBorder( "Name of Class list or Grammar File" ) );

        fileInput = new JRadioButton( "include main that uses file input" );
        fileInput.setSelected( false );

        stdInput = new JRadioButton( "include main that uses std input" );
        stdInput.setSelected( false );

        includeKernel = new JCheckBox( "include kernel classes" );
		  includeKernel.setSelected(false);

        ButtonGroup buttonGroup = new ButtonGroup();
        buttonGroup.add( fileInput );
        buttonGroup.add( stdInput );

        JPanel fileInputPanel = new JPanel();
        fileInputPanel.setBorder( BorderFactory.createEtchedBorder() );
        fileInputPanel.setLayout( new BoxLayout( fileInputPanel, BoxLayout.X_AXIS ) );
        fileInputPanel.add( fileName );
        fileInputPanel.add( openFileButton );

        fileChooser = new JFileChooser( ( new File( "." ) ).getPath() );
        fileChooser.setFileSelectionMode( JFileChooser.FILES_AND_DIRECTORIES );

        model = new DefaultListModel();
        methodList = new JList( model );
        scrollPane = new JScrollPane( methodList );
        methodList.setEnabled( true );

        textInput = new JPanel();
        textInput.setLayout( new BoxLayout( textInput, BoxLayout.X_AXIS ) );
        textInput.add( layerName );
        textInput.add( productionName );

        optionInput = new JPanel();
        optionInput.setBorder( BorderFactory.createEtchedBorder() );
        optionInput.setLayout( new BoxLayout( optionInput, BoxLayout.Y_AXIS ) );
        optionInput.add( fileInput );
        optionInput.add( stdInput );
		  optionInput.add( includeKernel );
    
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout( new BoxLayout( buttonPanel, BoxLayout.Y_AXIS ) );
        buttonPanel.add( addMethodButton );
        buttonPanel.add( removeMethodButton );

        JPanel north = new JPanel();
        north.setLayout( new BoxLayout( north, BoxLayout.Y_AXIS ) );
        north.add( textInput );
        north.add( optionInput );

        JPanel methodPanel = new JPanel();
        methodPanel.setLayout( new BorderLayout() );
        methodPanel.add( optionInput, BorderLayout.NORTH );
        methodPanel.add( methodSignature, BorderLayout.CENTER );
        methodPanel.add( scrollPane, BorderLayout.SOUTH );

        JPanel p = new JPanel();
        p.setLayout( new BoxLayout( p, BoxLayout.X_AXIS ) );
        p.add( methodPanel );
        p.add( buttonPanel );

        JPanel gPanel = new JPanel();
        gPanel.setLayout( new BorderLayout() );
        gPanel.add( p, BorderLayout.CENTER );
        gPanel.add( generate, BorderLayout.SOUTH );

        this.add( north, BorderLayout.NORTH );
        this.add( fileInputPanel, BorderLayout.CENTER );
        this.add( gPanel, BorderLayout.SOUTH );

        initListeners();
    }

    /**
     *Check to see if a method signature is syntactly correct
     * @layer<GramGui>
     */
    private boolean validMethod( String method ) {

        String methodRegex =  "\\s*([a-zA-Z][.a-zA-Z]*\\s+){0,1}"         //return type
                             + "\\s*[$_a-zA-Z][$_a-zA-Z0-9]*\\s*"        //method name 
                             + "\\(\\s*([a-zA-Z][.a-zA-Z]*\\s+"      //type
                             +"[$_a-zA-Z][$_a-zA-Z0-9]*\\s*"             // variable
                             +"(\\s*,\\s*[a-zA-Z][.a-zA-Z]*\\s+[$_a-zA-Z][$_a-zA-Z0-9]*)*){0,1}\\s*\\)\\s*";
        java.util.regex.Pattern p = java.util.regex.Pattern.compile( methodRegex );
        Matcher m = p.matcher( method );
        if( m.matches() )
            return true;
        else
            return false;
    }

    // initialize listeners here

    public void initListeners() {
        generate.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
               
                Arguments args = new Arguments();
                args.setLayer( layerName.getText() );
                args.setStart( productionName.getText() );

                String s = fileName.getText();
                File checkFile = new File( s );
                if( !checkFile.exists() ) {
                    JOptionPane.showMessageDialog( null, "Error: " + s
                    + " can not be found" );
                    return;
                }
                if( s.endsWith( ".b" ) ) {
                    args.setBaliFile( s );
                }
                else {
                    args.setClassFile( s );
                }
  
                if ( fileInput.isSelected() ) {
                    args.setFileInput( true );
                    args.setOutputMain( true );
                }

                if ( stdInput.isSelected() ) {
                    args.setStdInput( true );
                    args.setOutputMain( true );
                }

                int m = model.getSize();

                if ( m > 0 ) {
                    for( int i = 0; i < m; i++ ) {
                        String method = ( String ) methodList.getModel().getElementAt( i );
                        args.addMethod( method );

                    //System.out.println(method);
                    }
                }

                // generate Ast files, if necessary
                try {
					     if (includeKernel.isSelected() && m>0) 
						     generateAstNodeFiles( m );
                }
                catch ( Exception ex ) {
                    System.err.println( ex.getMessage() );
                    System.exit( 1 );
                }

                try {
                    args.collectClass();
                }
                catch ( Throwable t ) {
                    System.err.println( t.getMessage() );
                    System.exit( 1 );
                }

                Main instance = new Main() ;
                instance.generateObject( args ) ;

                JOptionPane.showMessageDialog( null, "code templates generated" );
                System.exit( 1 );

            }
        } );

        addMethodButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                String s = methodSignature.getText().trim();
                boolean duplicate = false;

                if( !validMethod( s ) ) {
                    JOptionPane.showMessageDialog( null, "Incorrect syntax for method: " + s );
                    return;
                }
                if ( s.equals( "" ) ) {
                    Toolkit.getDefaultToolkit().beep();
                    return;
                }

                int index = methodList.getSelectedIndex();
                int size = model.getSize();
                if( size == 0 ) {
                    removeMethodButton.setEnabled( true );
                }

                for( int i = 0; i < model.getSize(); i++ ) {
                    if( s.equals( methodList.getModel().getElementAt( i ) ) )
                        duplicate = true;
                }
                if( !duplicate ) {
                    if ( index == -1 || ( index+1 == size ) ) {
                        model.addElement( s );
                    }
                    else {
                        model.insertElementAt( s, index+1 );
                    }
                    methodSignature.setText( null );
                }
            }

        } );

        removeMethodButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {

                int index = methodList.getSelectedIndex();
                model.remove( index );

                int size = model.getSize();

                if ( size == 0 ) {

                    removeMethodButton.setEnabled( false );

                }
                else {
                    if ( index == model.getSize() ) //removed item in last position
                        index--;
                }

            }
        } );
    
        openFileButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e )
                        {
                int returnVal=0;
                returnVal = fileChooser.showOpenDialog( parentPanel );

                if ( returnVal == JFileChooser.APPROVE_OPTION )
                                  {
                    File Inputfile = fileChooser.getSelectedFile();
                    String Name = Inputfile.getName();

                    if ( Name.equals( DefaultFileName ) || Name.equals( "" ) ) {
                        // if the chosen name is the default you have to change it
                        JOptionPane.showMessageDialog( null, "Wrong File Name ","Error!",                                                                                                   JOptionPane.ERROR_MESSAGE );
                    }
                    inputFileName = Inputfile.getPath();
                    fileName.setText( inputFileName );
                }
            } // of action Performed
        } );
    
    }

    public void valueChanged( ListSelectionEvent e ) {
        if ( e.getValueIsAdjusting() == false ) {

            if ( methodList.getSelectedIndex() == -1 ) {
                //No selection, disable fire button.
                removeMethodButton.setEnabled( false );
                methodSignature.setText( "" );

            }
            else {
                //Selection, update text field.
                removeMethodButton.setEnabled( true );
                String name = methodList.getSelectedValue().toString();
                methodSignature.setText( name );
            }
        }
    }

	 private void generateAstNodeFiles( int n ) throws Exception {
      String name, type, parms, args;
      PrintStream ps;
		String[] m;
		int i;
		Arguments dummy = new Arguments();

		// create AstNode.jak

      ps = new PrintStream(new FileOutputStream( "AstNode.jak"));
      ps.println("refines class AstNode {");
      for( i = 0; i < n; i++ ) {
         String method = ( String ) methodList.getModel().getElementAt( i );
			m = dummy.decompose(method, null);
         type = m[0] + " ";
         name = m[1];
         args  = "(" + m[2] + ")";
         parms = "(" + m[3] + ")";

         ps.println("   public "+method+" { ");
         ps.println("      int i;");
         ps.println("      if (arg == null)");
         ps.println("         return;");
         ps.println("      for (i=0; i<arg.length; i++)");
         ps.println("         if (arg[i]!=null)");
         ps.println("            arg[i]."+name+parms+";");
         ps.println("   }");
      }
      ps.println("}");
		ps.close();

      // create AstList.jak

      ps = new PrintStream(new FileOutputStream( "AstList.jak"));
      ps.println("refines class AstList {");
      for( i = 0; i < n; i++ ) {
         String method = ( String ) methodList.getModel().getElementAt( i );
			m = dummy.decompose(method, null);
         type = m[0] + " ";
         name = m[1];
         args  = "(" + m[2] + ")";
         parms = "(" + m[3] + ")";
         ps.println("   public "+method+ " {");
         ps.println("      AstNode l;");
         ps.println("      if (arg[0]==null) return;");
         ps.println("      for (l = arg[0]; l!=null; l = l.right) {");
         ps.println("         if (l.arg[0] == null) ");
         ps.println("             continue;");
         ps.println("         l.arg[0]."+name+parms+";");
         ps.println("      }");
         ps.println("   }");
      }
      ps.println("}");
		ps.close();

      // create AstListNode.jak

      ps = new PrintStream(new FileOutputStream( "AstListNode.jak"));
      ps.println("refines class AstListNode {");
      for( i = 0; i < n; i++ ) {
         String method = ( String ) methodList.getModel().getElementAt( i );
			m = dummy.decompose(method, null);
         type = m[0] + " ";
         name = m[1];
         args  = "(" + m[2] + ")";
         parms = "(" + m[3] + ")";
         ps.println("   public " + method + " { ");
         ps.println("       Util.fatalError(\"AstListNode."
		+ name+"() method should not be called\");");
         ps.println("    }");
      }
      ps.println("}");
		ps.close();

      // create AstOptNode.jak

      ps = new PrintStream(new FileOutputStream( "AstOptNode.jak"));
      ps.println("refines class AstOptNode {");
      for( i = 0; i < n; i++ ) {
         String method = ( String ) methodList.getModel().getElementAt( i );
			m = dummy.decompose(method, null);
         type = m[0] + " ";
         name = m[1];
         args  = "(" + m[2] + ")";
         parms = "(" + m[3] + ")";
         ps.println("   public "+method+" {");
         ps.println("      if (arg[0]!=null)");
         ps.println("         arg[0]."+name+parms+";");
         ps.println("   }");
      }
      ps.println("}");
      ps.close();
   }
}
