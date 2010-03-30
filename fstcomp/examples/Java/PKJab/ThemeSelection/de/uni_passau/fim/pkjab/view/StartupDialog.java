package de.uni_passau.fim.pkjab.view;

import javax.swing.JButton;

import java.awt.BorderLayout;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

class StartupDialog {

    private static final String CHOOSE_THEME = "Thema aendern";

    protected void setupGui() {
        original();
        
        final JButton chooseThemeButton = new JButton(CHOOSE_THEME);
        chooseThemeButton.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    ThemeChooser chooser = new ThemeChooser(null, CHOOSE_THEME, true);
                    
                    AbstractTheme nTheme = chooser.getTheme();
                    
                    if (nTheme != null) {
                        PKjabToolkit.setTheme(nTheme);
                        PKjabToolkit.setupUI();
                    }
                }
            });
            
            getContentPane().add(chooseThemeButton, BorderLayout.PAGE_END);
    }

}