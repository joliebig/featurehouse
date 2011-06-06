



package com.lowagie.toolbox;

import java.awt.Desktop;
import java.awt.event.*;
import java.io.File;
import java.util.ArrayList;

import javax.swing.*;

import com.lowagie.toolbox.arguments.AbstractArgument;
import com.lowagie.tools.Executable;


public abstract class AbstractTool implements ActionListener {

    
    public static ArrayList<String> versionsarray = new ArrayList<String>();

    
    protected JInternalFrame internalFrame = null;
    
    protected ArrayList<AbstractArgument> arguments = new ArrayList<AbstractArgument>();
    
    protected int menuoptions = MENU_EXECUTE;
    
    public static final int MENU_EXECUTE = 1;
    
    public static final int MENU_EXECUTE_SHOW = 2;
    
    public static final int MENU_EXECUTE_PRINT = 4;
    
    public static final int MENU_EXECUTE_PRINT_SILENT = 8;

    
    private Desktop awtdesktop = null;
    private JMenuBar menubar;

    
    public AbstractTool() {
        if (Desktop.isDesktopSupported()) {
            awtdesktop = Desktop.getDesktop();
        }
    }

    
    public void setArguments(ArrayList<AbstractArgument> arguments) {
        this.arguments = arguments;
    }

    
    public void setMainArguments(String[] args) {
        int counter = 0;
        for (AbstractArgument argument: arguments) {
            if (args.length > counter) {
                argument.setValue(args[counter]);
            } else {
                break;
            }
            counter++;
        }
    }

    
    public ArrayList<AbstractArgument> getArguments() {
        return arguments;
    }

    
    public Object getValue(String name) throws InstantiationException {
        for (AbstractArgument argument: arguments) {
            if (name.equals(argument.getName())) {
                return argument.getArgument();
            }
        }
        return null;
    }

    
    public void setInternalFrame(JInternalFrame internalFrame) {
        this.internalFrame = internalFrame;
    }

    public void setMenubar(JMenuBar menubar) {
        this.menubar = menubar;
    }

    
    public JInternalFrame getInternalFrame() {
        if (internalFrame == null) {
            createFrame();
        }
        return internalFrame;
    }

    
    public JMenuBar getMenubar() {
        menubar = new JMenuBar();
        JMenu tool = new JMenu(ToolMenuItems.TOOL);
        tool.setMnemonic(KeyEvent.VK_F);
        JMenuItem usage = new JMenuItem(ToolMenuItems.USAGE);
        usage.setMnemonic(KeyEvent.VK_U);
        usage.addActionListener(this);
        tool.add(usage);
        JMenuItem args = new JMenuItem(ToolMenuItems.ARGUMENTS);
        args.setMnemonic(KeyEvent.VK_A);
        args.addActionListener(this);
        tool.add(args);
        if ((menuoptions & MENU_EXECUTE) > 0) {
            JMenuItem execute = new JMenuItem(ToolMenuItems.EXECUTE);
            execute.setMnemonic(KeyEvent.VK_E);
            execute.addActionListener(this);
            tool.add(execute);
        }
        if ((menuoptions & MENU_EXECUTE_SHOW) > 0) {
            JMenuItem execute = new JMenuItem(ToolMenuItems.EXECUTESHOW);
            execute.addActionListener(this);
            tool.add(execute);
        }
        if ((menuoptions & MENU_EXECUTE_PRINT) > 0) {
            JMenuItem execute = new JMenuItem(ToolMenuItems.EXECUTEPRINT);
            execute.addActionListener(this);
            tool.add(execute);
        }
        if ((menuoptions & MENU_EXECUTE_PRINT_SILENT) > 0) {
            JMenuItem execute = new JMenuItem(ToolMenuItems.EXECUTEPRINTSILENT);
            execute.addActionListener(this);
            tool.add(execute);
        }
        JMenuItem close = new JMenuItem(ToolMenuItems.CLOSE);
        close.setMnemonic(KeyEvent.VK_C);
        close.addActionListener(this);
        tool.add(close);
        menubar.add(tool);
        if (!arguments.isEmpty()) {
            JMenu params = new JMenu(ToolMenuItems.ARGUMENTS);
            tool.setMnemonic(KeyEvent.VK_T);
            JMenuItem item;
            for (AbstractArgument argument: arguments) {
                item = new JMenuItem(argument.getName());
                item.setToolTipText(argument.getDescription());
                item.addActionListener(argument);
                params.add(item);
            }
            menubar.add(params);
        }
        return menubar;
    }
    
    public String getUsage() {
        StringBuffer buf = new StringBuffer("java ");
        buf.append(getClass().getName());
        for (AbstractArgument argument: arguments) {
            buf.append(' ');
            buf.append(argument.getName());
        }
        buf.append('\n');
        for (AbstractArgument argument: arguments) {
            buf.append(argument.getUsage());
        }
        return buf.toString();
    }

    
    private String getArgs() {
        StringBuffer buf = new StringBuffer("Current arguments:\n");
        for (AbstractArgument argument: arguments) {
            buf.append("  ");
            buf.append(argument.getName());
            if (argument.getValue() == null) {
                buf.append(" = null\n");
            } else {
                buf.append(" = '");
                buf.append(argument.toString());
                buf.append("'\n");
            }
        }
        return buf.toString();
    }

    
    public void actionPerformed(ActionEvent evt) {
        if (ToolMenuItems.CLOSE.equals(evt.getActionCommand())) {
            System.out.println("=== " + getInternalFrame().getTitle() +
                               " CLOSED ===");
            internalFrame.dispose();
        }
        if (ToolMenuItems.USAGE.equals(evt.getActionCommand())) {
            JOptionPane.showMessageDialog(internalFrame, getUsage());
        }
        if (ToolMenuItems.ARGUMENTS.equals(evt.getActionCommand())) {
            JOptionPane.showMessageDialog(internalFrame, getArgs());
        }
        if (ToolMenuItems.EXECUTE.equals(evt.getActionCommand())) {
            this.execute();
        }
        if (ToolMenuItems.EXECUTESHOW.equals(evt.getActionCommand())) {
            this.execute();
            try {
                if (awtdesktop != null &&
                    awtdesktop.isSupported(Desktop.Action.OPEN)) {
                    awtdesktop.open(getDestPathPDF());
                } else {
                    Executable.openDocument(getDestPathPDF());
                }
            } catch (Exception e) {
                System.err.println(e.getMessage());
            }
        }
        if (ToolMenuItems.EXECUTEPRINT.equals(evt.getActionCommand())) {
            this.execute();
            try {
                if (awtdesktop != null &&
                    awtdesktop.isSupported(Desktop.Action.PRINT)) {
                    awtdesktop.print(getDestPathPDF());
                } else {
                    Executable.printDocument(getDestPathPDF());
                }
            } catch (Exception e) {
                System.err.println(e.getMessage());
            }
        }
        if (ToolMenuItems.EXECUTEPRINTSILENT.equals(evt.getActionCommand())) {
            this.execute();
            try {
                Executable.printDocumentSilent(getDestPathPDF());
            } catch (Exception e) {
                System.err.println(e.getMessage());
            }
        }
    }

    
    protected abstract File getDestPathPDF() throws InstantiationException;

    
    protected abstract void createFrame();

    
    public abstract void execute();

    
    public abstract void valueHasChanged(AbstractArgument arg);

    
    protected static void addVersion(String version) {
        version = version.substring(5, version.length() - 2);
        versionsarray.add(version);
    }
}
