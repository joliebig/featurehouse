package net.sf.jabref;

import javax.swing.*;
import java.awt.*;


public class DisplayPanel {

    private JPanel panel = new JPanel();
    private BibtexEntry activeEntry = null;

    public DisplayPanel() {
        panel.setLayout(new BorderLayout());
    }

    public void setEntry(BibtexEntry entry) {
        activeEntry = entry;
    }

    public JPanel getPanel() {
        return panel;
    }
}
