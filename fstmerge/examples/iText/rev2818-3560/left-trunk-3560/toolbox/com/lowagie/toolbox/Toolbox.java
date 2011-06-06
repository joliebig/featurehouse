



package com.lowagie.toolbox;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import java.io.*;
import java.util.*;

import javax.swing.*;
import javax.swing.text.*;

import com.lowagie.tools.Executable;


public class Toolbox extends JFrame implements ActionListener {
    
    private static final long serialVersionUID = -3766198389452935073L;
    
    private JDesktopPane desktop;
    
    private JScrollPane console;

    
    private Properties toolmap = new Properties();

    
    private int locationX = 0;

    
    private int locationY = 0;
    
    private ArrayList<AbstractTool> toolarray = new ArrayList<AbstractTool>();

    private Vector<String> menulist=new Vector<String>();
    private Vector<String> menuitemlist=new Vector<String>();
    
    public Toolbox() {
        super();
        setSize(600, 500);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setResizable(true);
        setTitle("iText Toolbox");
        desktop = new JDesktopPane();
        setJMenuBar(getMenubar());
        setIconImage(new ImageIcon(com.lowagie.toolbox.Toolbox.class.getResource(
                "1t3xt.gif")).getImage());
        Console c;
        try {
            c = new Console();
            console = new JScrollPane(c.textArea);
        } catch (IOException e) {
            e.printStackTrace();
        }
        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
                                              desktop,
                                              console);
        splitPane.setContinuousLayout(true);
        splitPane.setOneTouchExpandable(true);
        splitPane.setDividerLocation(300);
        setContentPane(splitPane);
        centerFrame(this);
        setVisible(true);
    }

    
    public static void main(String[] args) {
        try {
            Class.forName("com.lowagie.text.Document");
        } catch (ClassNotFoundException e) {
            JOptionPane.showMessageDialog(null,
                                          "You need the iText.jar in your CLASSPATH!",
                                          e.getClass().getName(),
                                          JOptionPane.ERROR_MESSAGE);
            System.exit(1);
        }
        Toolbox toolbox = new Toolbox();
        if (args.length > 0) {
            try {
                AbstractTool tool = toolbox.createFrame(args[0]);
                String[] nargs = new String[args.length - 1];
                System.arraycopy(args, 1, nargs, 0, args.length - 1);
                tool.setMainArguments(nargs);
                tool.execute();
            } catch (PropertyVetoException ex) {
            } catch (ClassNotFoundException ex) {
            } catch (IllegalAccessException ex) {
            } catch (InstantiationException ex) {
            }
        }
    }

    
    private JMenuBar getMenubar() {
        Properties p = new Properties();
        try {
            p.load(Toolbox.class.getClassLoader().getResourceAsStream(
                    "com/lowagie/toolbox/tools.txt"));
            String usertoolstxt = System.getProperty("user.home") +
                                  System.getProperty("file.separator") +
                                  "tools.txt";
            File uttf = new File(usertoolstxt);
            if (uttf.isFile() && uttf.exists()) {
                p.load(new FileInputStream(usertoolstxt));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        toolmap = new Properties();
        JMenuBar menubar = new JMenuBar();
        JMenu file = new JMenu(ToolMenuItems.FILE);
        file.setMnemonic(KeyEvent.VK_T);
        JMenuItem close = new JMenuItem(ToolMenuItems.CLOSE);
        close.setMnemonic(KeyEvent.VK_C);
        close.addActionListener(this);
        file.add(close);
        JMenu view = new JMenu(ToolMenuItems.VIEW);
        JMenuItem reset = new JMenuItem(ToolMenuItems.RESET);
        reset.addActionListener(this);
        view.add(reset);



        JMenu tools = new JMenu(ToolMenuItems.TOOLS);




        buildPluginMenuItems(new TreeMap<Object, Object>(p), tools);
        JMenu help = new JMenu(ToolMenuItems.HELP);
        JMenuItem about = new JMenuItem(ToolMenuItems.ABOUT);


        about.setMnemonic(KeyEvent.VK_A);
        about.addActionListener(this);
        help.add(about);
        JMenuItem versions = new JMenuItem(ToolMenuItems.VERSION);


        versions.addActionListener(this);
        help.add(versions);
        menubar.add(file);
        menubar.add(tools);
        menubar.add(view);
        menubar.add(Box.createGlue());
        menubar.add(help);
        return menubar;
    }

    
    private void buildPluginMenuItems(Map<Object, Object> tmp, JMenu tools) {
        String name, tool;
        JMenu current = null;
        JMenuItem item;

        for (Map.Entry<Object, Object> entry: tmp.entrySet()) {
            name = (String) entry.getKey();
            if (current == null || !name.startsWith(current.getText())) {
                String menu = name.substring(0, name.indexOf('.'));
                menulist.add(menu);
                current = new JMenu(menu);
                tools.add(current);
            }
            String menuitem = name.substring(current.getText().length() + 1);
            menuitemlist.add(menuitem);
            item = new JMenuItem(menuitem);
            item.addActionListener(this);
            tool = (String) entry.getValue();
            try {
                if (Class.forName(tool) != null) {
                    toolmap.put(item.getText(), tool);
                    current.add(item);
                }
            } catch (ClassNotFoundException e) {
                System.err.println("Plugin " + name
                                   + " was not found in your CLASSPATH.");
            }
        }
    }

    
    public AbstractTool createFrame(String name) throws InstantiationException,
            IllegalAccessException, ClassNotFoundException,
            PropertyVetoException {
        AbstractTool ti = null;
        String classname = (String) toolmap.get(name);
        ti = (AbstractTool) Class.forName(classname).newInstance();
        toolarray.add(ti);
        JInternalFrame f = ti.getInternalFrame();
        f.setLocation(locationX, locationY);
        locationX += 25;
        if (locationX > this.getWidth() + 50) {
            locationX = 0;
        }
        locationY += 25;
        if (locationY > this.getHeight() + 50) {
            locationY = 0;
        }
        f.setVisible(true);
        desktop.add(f);
        f.setSelected(true);
        return ti;
    }

    
    public static void centerFrame(Frame f) {
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension frameSize = f.getSize();
        if (frameSize.height > screenSize.height) {
            frameSize.height = screenSize.height;
        }
        if (frameSize.width > screenSize.width) {
            frameSize.width = screenSize.width;
        }
        f.setLocation((screenSize.width - frameSize.width) / 2,
                      (screenSize.height - frameSize.height) / 2);
    }

    
    public void actionPerformed(ActionEvent evt) {
        if (ToolMenuItems.CLOSE.equals(evt.getActionCommand())) {
            System.out.println("The Toolbox is closed.");
            System.exit(0);
        } else if (ToolMenuItems.ABOUT.equals(evt.getActionCommand())) {
            System.out
                    .println("The iText Toolbox is part of iText, a Free Java-PDF Library.\nVisit http://itexttoolbox.sourceforge.net/ for more info.");
            try {
                Executable
                        .launchBrowser("http://itexttoolbox.sourceforge.net/");
            } catch (IOException ioe) {
                JOptionPane
                        .showMessageDialog(
                                this,
                                "The iText Toolbox is part of iText, a Free Java-PDF Library.\nVisit http://itexttoolbox.sourceforge.net/ for more info.");
            }
        } else if (ToolMenuItems.RESET.equals(evt.getActionCommand())) {
            JInternalFrame[] framearray = desktop.getAllFrames();
            int xx = 0, yy = 0;
            for (int i = 0; i < framearray.length; i++) {
                if (!framearray[i].isIcon()) {
                    try {
                        int frameDistance = framearray[i].getHeight() -
                                            framearray[i].getContentPane().
                                            getHeight();
                        framearray[i].setMaximum(false);
                        int fwidth = framearray[i].getWidth();
                        int fheight = framearray[i].getHeight();
                        framearray[i].reshape(xx, yy, fwidth, fheight);
                        xx += frameDistance;
                        yy += frameDistance;
                        if (xx + fwidth > desktop.getWidth()) {
                            xx = 0;
                        }
                        if (yy + fheight > desktop.getHeight()) {
                            yy = 0;
                        }
                    } catch (PropertyVetoException e) {
                    }
                }
            }
        } else if (ToolMenuItems.VERSION.equals(evt.getActionCommand())) {
            JFrame f = new Versions();
            centerFrame(f);
            f.setVisible(true);
        } else {
            try {
                createFrame(evt.getActionCommand());
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    
    public class Console {
        class ErrorContext extends StyleContext {
            private static final long serialVersionUID = 7766294638325167438L;
            public static final String STDERROR = "Error";
            public static final String STDOUT = "StdOut";

            public ErrorContext() {
                super();
                Style root = getStyle(DEFAULT_STYLE);
                Style s = addStyle(STDERROR, root);
                StyleConstants.setForeground(s, Color.RED);
                s = addStyle(STDOUT, root);
                StyleConstants.setForeground(s, Color.BLACK);
            }
        }


        PipedInputStream piOut;
        PipedInputStream piErr;
        PipedOutputStream poOut;
        PipedOutputStream poErr;
        ErrorContext errorcontext = new ErrorContext();
        JTextPane textArea = new JTextPane(new DefaultStyledDocument(
                errorcontext));
        PrintStream oriout;
        PrintStream orierr;

        
        public Console() throws IOException {
            
            piOut = new PipedInputStream();
            poOut = new PipedOutputStream(piOut);
            oriout = System.out;
            System.setOut(new PrintStream(poOut, true));

            
            piErr = new PipedInputStream();
            poErr = new PipedOutputStream(piErr);
            orierr = System.err;
            System.setErr(new PrintStream(poErr, true));

            
            textArea.setEditable(false);

            
            new ReaderThread(piOut, ErrorContext.STDOUT).start();
            new ReaderThread(piErr, ErrorContext.STDERROR).start();
        }

        class ReaderThread extends Thread {
            PipedInputStream pi;
            String type;

            ReaderThread(PipedInputStream pi, String type) {
                this.pi = pi;
                this.type = type;
            }

            
            public void run() {
                final byte[] buf = new byte[1024];

                while (true) {
                    try {
                        final int len = pi.read(buf);
                        if (len == -1) {
                            break;
                        }
                        javax.swing.text.Document doc = textArea.getDocument();
                        AttributeSet attset = errorcontext.getStyle(type);
                        String snippet = new String(buf, 0, len);
                        doc.insertString(doc.getLength(),
                                         snippet, attset);
                        oriout.print(snippet);
                        textArea.setCaretPosition(textArea.getDocument().
                                                  getLength());
                    } catch (BadLocationException ex) {
                    } catch (IOException e) {
                    }
                }
            }
        }
    }


    public Vector<String> getMenulist() {
        return menulist;
    }
}
