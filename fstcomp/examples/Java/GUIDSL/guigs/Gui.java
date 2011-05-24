// Gui.java

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.*;
import java.util.*;

public class Gui extends SwingApp {

   static String equations = "equations";

   // initialize constants used in the application
   // REMEMBER -- make constants static!
    public static JTabbedPane tabs = new JTabbedPane();

   public void initConstants() {

   }

   // declare and initialize atomic components here
    JFileChooser fc; //file chooser
    javax.swing.filechooser.FileFilter conf; //file filters
    javax.swing.filechooser.FileFilter equat;

   public void initAtoms() {
        fc = new JFileChooser(System.getProperty("user.dir"));

        //create file filters for accepting .config files
        conf = new javax.swing.filechooser.FileFilter(){
            public boolean accept(File f){
                if(f != null) {
                   String filename = f.getName();
                   int i = filename.lastIndexOf('.');
                   if(i>0 && i<filename.length()-1) {
                      String ext =  filename.substring(i+1).toLowerCase();
                      if (ext.equals("config"))
                          return true;
                   };
               }
               return false;
            }
            public String getDescription(){
                return new String("Config files");
            }
        };
        fc.addChoosableFileFilter(conf);//add this filter to the file chooser

        //create a file filter for accepting just .equations files
        equat = new javax.swing.filechooser.FileFilter(){
            public boolean accept(File f){
                if (f!= null){
                    String filename = f.getName();
                    int i = filename.lastIndexOf('.');
                    if(i>0 && i<filename.length()-1){
                        String ext = filename.substring(i+1).toLowerCase();
                        if(ext.equals(equations))
                            return true;
                    }
                }
                return false;
            }
            public String getDescription(){
                return new String("Equations files");
            }
        };
        fc.addChoosableFileFilter(equat);//add this filter to the file chooser
   }

   // declare and initialize layout components here


    private JMenuBar menubar;
    private JToolBar toolbar;
    private JPanel content;//wraps around the tabs, has scrollbars
    //Create the menu that contains the items
    private JMenu file, help;
    //Create the menu items
    private JMenuItem reset, exit, saveConfig, saveEquat, openConfig, openEquat, initTable, propForm, debugModel, helpFile;

    private JButton resetb, openb, sv, htmlfile, opene, savee;//toolbar buttons

    private JPanel meaning;//wraps around the textarea, has scrollbars
    public JTextArea tarea;

    public int showhelp;//whether to display help or why a variable is marked with respect to help menu

   public void initLayout() {
      //for setting the menu bar
      setJMenuBar(menubar= new JMenuBar());

      //create toolbar
      toolbar = new JToolBar("Toolbar");

    //add buttons to the toolbar
     resetb = new JButton("Reset");
     resetb.setToolTipText("Reset the model");
     toolbar.add(resetb);
     openb = new JButton("Open Cfg");
     openb.setToolTipText("Open configuration");
     toolbar.add(openb);
     sv = new JButton("Save Cfg");
     sv.setToolTipText("Save Configuration");
     toolbar.add(sv);
     opene = new JButton("Open Eqn");
     opene.setToolTipText("Open Equation");
     toolbar.add(opene);
     savee = new JButton("Save Eqn");
     savee.setToolTipText("Save Equation");
     toolbar.add(savee);
      toolbar.addSeparator();
      JButton dbt = new JButton("DB Table");
      dbt.setToolTipText("Display Variable Table");
      dbt.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                grammar.initDebugTable();
            }
            });
      toolbar.add(dbt);
      JButton propform = new JButton("Formulas");
      propform.setToolTipText("Display Propagation Formulas");
      propform.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                grammar.initFormulas();
               }
          });
      toolbar.add(propform);
      JButton showdeb = new JButton("Debugger");
      showdeb.setToolTipText("Launch Model Debugger");
      final Gui current = this;
      showdeb.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                new ModelDebuggerGui(current, "Model Debugger", false);
            }
      });
      toolbar.add(showdeb);
      toolbar.addSeparator();
      htmlfile = new JButton("Help");
      htmlfile.setToolTipText("Open Help File");
      toolbar.add(htmlfile);

     //adding file,  help to the menu bar
      menubar.add(file  = new JMenu("File"));
      menubar.add(help   = new JMenu("Help"));

      //adding menu items to file menu
      file.add(reset = new JMenuItem("Reset"));
      file.addSeparator();
      file.add(saveConfig = new JMenuItem("Save Configuration"));
      file.add(saveEquat = new JMenuItem("Save Equations"));
      file.addSeparator();
      file.add(openConfig = new JMenuItem("Open Configuration"));
      file.add(openEquat = new JMenuItem("Open Equations"));
      file.addSeparator();
      file.add(exit = new JMenuItem("Close Program"));

      //adding menu items to help menu
      help.add(initTable = new JMenuItem("Display Variable Table"));
      help.add(propForm = new JMenuItem("Display Propagation Formulas"));
      help.add(debugModel = new JMenuItem("Launch Model Debugger"));
      showhelp = 0;
      help.addSeparator();

      final JRadioButtonMenuItem bhelp = new JRadioButtonMenuItem("Display help for variables");
      final JRadioButtonMenuItem breason = new JRadioButtonMenuItem("Display reason for variable selection");
      breason.setSelected(true);

      ButtonGroup bg = new ButtonGroup();
      bg.add(bhelp);// add radio buttons
      bg.add(breason); //to the button group
      bhelp.addChangeListener(new ChangeListener(){//uses showhelp to determine what should be displayed
            public void stateChanged(ChangeEvent e){
                if (breason.isSelected())
                    showhelp = 0;
                else
                    showhelp = 1;
            }
        });
      help.add(bhelp);
      help.add(breason);
      help.addSeparator();
      help.add(helpFile = new JMenuItem("Open Help File"));


      grammar.rootProduction.draw(0); //tabs have the gui now
      //wrap the tabs into the panel for scrollpane
      content = new JPanel();
      content.setLayout(new BorderLayout());
      content.add(tabs, BorderLayout.CENTER);

      //create the panel to wrap the text area in
      meaning = new JPanel();
      meaning.setLayout(new BorderLayout());
      meaning.setBackground(Color.lightGray);
      meaning.setOpaque(false);//transparent

      //create the text area
      tarea = new JTextArea();
      tarea.setRows(4);
      tarea.setMargin(new Insets(5, 5, 5, 5));
      tarea.setEditable(false);//do not allow users to change what is shown manually
      tarea.setLineWrap(true);
      tarea.setWrapStyleWord(true);
      tarea.setBackground(Color.white);

      //wrap the text area in jpanel
      JScrollPane scroll = new JScrollPane(tarea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
      JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
      meaning.add(scroll);
   }

   // initialize ContentPane here

    public void initContentPane() {
        ContentPane = new JPanel(new BorderLayout());
        ContentPane.setBorder(BorderFactory.createEtchedBorder());
        ContentPane.add(toolbar, BorderLayout.NORTH);
        ContentPane.add (new JScrollPane(content, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
            JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED));
        ContentPane.add( meaning, BorderLayout.SOUTH);
        ContentPane.setPreferredSize(new Dimension(600, 400));
        ContentPane.setMinimumSize(new Dimension(100, 70));
    }

   //helper functions to read and write configurations

   void writeConfig(File file){
        try{
            FileWriter fw = new FileWriter(file);
            Iterator it = grammar.UserSelections.iterator();
            fw.write("## ");
            while(it.hasNext()){
                variable vt = (variable)(it.next());
                fw.write(vt.name);
                fw.write(" ");
            }
            fw.close();//write to the file
        }
        catch(IOException e){}
   }

   void readConfig(File file){
        try{
                        FileReader fr = new FileReader(file);
                        StringBuffer sb = new StringBuffer();
                        char[] b = new char[8192];
                        int n;
                        while ((n = fr.read(b)) > 0)
                            sb.append(b, 0, n);
                        String input = sb.toString();
                        input = input.substring(input.indexOf("#")+2);
                        if (input != null){//parse and assign the variables;
                            StringTokenizer init = new StringTokenizer(input, "\n\r\f");
                            if (init.hasMoreTokens()){
                                StringTokenizer sttok = new StringTokenizer(init.nextToken());
                                grammar.UserSelections.clear();
                                while (sttok.hasMoreTokens()){
                                    String cur = sttok.nextToken();
                                    variable var = (variable)(variable.Vtable.get(cur));
                                    grammar.UserSelections.add(var);
                                }
                                grammar.propagate();
                                ActionList.setGui();
                            }
                        }
                        else
                            JOptionPane.showMessageDialog(null,
                                     "Error",
                                     "Empty file given, cannot read a configuration",
                                    JOptionPane.INFORMATION_MESSAGE);
                    }
                    catch(IOException e){}
   }

/**********INITIALIZE ACTION LISTENERS**************/

   public void initListeners() {
    final Gui current = this;//since listeners need to know about the gui

    //create reset action listener
    resetal ral = new resetal();
    reset.addActionListener(ral);
    resetb.addActionListener(ral);

    //save config action listener
    saveconfigal scal = new saveconfigal(this);
    saveConfig.addActionListener(scal);
    sv.addActionListener(scal);

    //save equations action listener
    saveequat seq = new saveequat(this);
    saveEquat.addActionListener(seq);
    savee.addActionListener(seq);

    //open config action listener
    openconfigal ocal = new openconfigal(this, conf);
    openConfig.addActionListener(ocal);
    openb.addActionListener(ocal);

    //open equations action listener
    openconfigal openeq = new openconfigal(this, equat);
    openEquat.addActionListener(openeq);
    opene.addActionListener(openeq);

    //help menu table initialization action listener
    initTable.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                grammar.initDebugTable();
            }
            });

     //table menu propagation formulas window action listener
     propForm.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                grammar.initFormulas();
               }
          });

      //file menu exit action listener
      exit.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                System.exit(0);
            }
        });

      //help menu debug model window action listener
      debugModel.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                new ModelDebuggerGui(current, "Model Debugger", false);
            }
      });

      //help menu help file windown action listener
      helpfileal hfal = new helpfileal(this);
      helpFile.addActionListener(hfal);
      htmlfile.addActionListener(hfal);

    }
   // place in this method any action for exiting application

   public void applicationExit() {

   }

    //was used when user would click on a feature button that's not yet implemented
    private void notImplemented(){
        JOptionPane.showMessageDialog(null,
                                     "Not yet implemented",
                                     "Not yet implemented",
                                    JOptionPane.INFORMATION_MESSAGE);
    }

   public Gui() {
    super();
    setLocationRelativeTo(null);
   // setResizable(false);
    pack();
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
  }

   public Gui(String AppTitle) {
    super(AppTitle);
    setLocationRelativeTo(null);
   // setResizable(false);
    pack();
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
  }

   public static void main(String[] args) {
      new Gui("Gui");
   }
}
