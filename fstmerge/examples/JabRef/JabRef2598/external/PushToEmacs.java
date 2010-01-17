package net.sf.jabref.external; 

import java.io.IOException; 
import java.io.InputStream; 

import javax.swing.*; 

import net.sf.jabref.*; 
import com.jgoodies.forms.builder.DefaultFormBuilder; 
import com.jgoodies.forms.layout.FormLayout; 


public  class  PushToEmacs implements  PushToApplication {
	

    private JPanel settings = null;

	
    private JTextField citeCommand = new JTextField(30);

	

    private boolean couldNotConnect=false, couldNotRunClient=false;

	

    public String getName() {
        return Globals.menuTitle("Insert selected citations into Emacs") ;
    }


	

    public String getApplicationName() {
        return "Emacs";
    }


	

    public String getTooltip() {
        return Globals.lang("Push selection to Emacs");
    }


	

    public Icon getIcon() {
        return GUIGlobals.getImage("emacs");
    }


	

    public String getKeyStrokeName() {
        return "Push to Emacs";
    }


	

    public JPanel getSettingsPanel() {
        if (settings == null)
            initSettingsPanel();
        citeCommand.setText(Globals.prefs.get("citeCommandEmacs"));
        return settings;
    }


	

    public void storeSettings() {
        Globals.prefs.put("citeCommandEmacs", citeCommand.getText());
    }


	

    private void initSettingsPanel() {
        DefaultFormBuilder builder = new DefaultFormBuilder(
                new FormLayout("left:pref, 4dlu, fill:pref", ""));
        builder.append(Globals.lang("Cite command") + ":");
        builder.append(citeCommand);
        settings = builder.getPanel();
    }


	

    public void pushEntries(BibtexDatabase database, BibtexEntry[] entries, String keys, MetaData metaData) {

        couldNotConnect=false;
        couldNotRunClient=false;
        try {
            String[] com = Globals.ON_WIN ?
                
                
                
                
                new String[] {"gnuclient", "-qe",
                "(insert \\\"\\\\" + Globals.prefs.get("citeCommandEmacs").replaceAll("\\\\", "\\\\\\\\") +
                        "{" + keys + "}\\\")"}
            :
                
                
                
                
                new String[] {"gnuclient", "-batch", "-eval",
                "(insert \"" + Globals.prefs.get("citeCommandEmacs").replaceAll("\\\\", "\\\\\\\\") +
                       "{" + keys + "}\")"};

            final Process p = Runtime.getRuntime().exec(com);

            Runnable errorListener = new Runnable() {
                public void run() {
                    InputStream out = p.getErrorStream();
                    int c;
                    StringBuffer sb = new StringBuffer();
                    try {
                        while ((c = out.read()) != -1)
                            sb.append((char) c);
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                    
                    if (sb.toString().trim().length() > 0) {
			System.out.println(sb.toString());
                        couldNotConnect = true;
                        return;
                    }
                }
            };
            Thread t = new Thread(errorListener);
            t.start();
            t.join();
        }
        catch (IOException excep) {
            couldNotRunClient = true;
            return;
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

    }


	

    public void operationCompleted(BasePanel panel) {
        if (couldNotConnect)
            JOptionPane.showMessageDialog(
                panel.frame(),
                "<HTML>"+
                Globals.lang("Could not connect to a running gnuserv process. Make sure that "
                +"Emacs or XEmacs is running,<BR>and that the server has been started "
                +"(by running the command 'gnuserv-start').")
                +"</HTML>",
                Globals.lang("Error"), JOptionPane.ERROR_MESSAGE);
        else if (couldNotRunClient)
            JOptionPane.showMessageDialog(
                panel.frame(),
                Globals.lang("Could not run the 'gnuclient' program. Make sure you have "
                +"the gnuserv/gnuclient programs installed."),
                Globals.lang("Error"), JOptionPane.ERROR_MESSAGE);
        else {
            panel.output(Globals.lang("Pushed citations to Emacs"));
        }
    }


	

    public boolean requiresBibtexKeys() {
        return true;
    }



}
