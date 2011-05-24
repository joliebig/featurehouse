package de.uni_passau.fim.pkjab.view;

import javax.swing.JTextField;
import javax.swing.JPasswordField;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.BorderFactory;

import java.awt.GridLayout;
import java.awt.BorderLayout;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

class StartupDialog extends JDialog {
        
        private static final String ENTER_USER = "User: ";
        
        private static final String ENTER_DOMAIN = "Domain: ";
        
        private static final String ENTER_PASSWORD = "Passwort: ";
        
        private static final String BUTTON_OK = "OK";
        
        private static final String BUTTON_CANCEL = "Abbrechen";
        
        private JTextField userField = new JTextField();
        
        private JTextField domainField = new JTextField();
        
        private JPasswordField passwordField = new JPasswordField();
        
        private String user;
        
        private String domain;
        
        private String password;
        
        StartupDialog(JFrame owner, String title, boolean modal) {
            super(owner, title, modal);
            
            setupGui();
            
            pack();
            setLocationRelativeTo(null);
            setVisible(true);
        }
        
        protected void setupGui() {
        	
            JPanel contentPane = new JPanel();
            contentPane.setLayout(new GridLayout(4, 2, 5, 10));
            contentPane.add(new JLabel(ENTER_USER));
            contentPane.add(userField);
            contentPane.add(new JLabel(ENTER_DOMAIN));
            contentPane.add(domainField);
            contentPane.add(new JLabel(ENTER_PASSWORD));
            contentPane.add(passwordField);
            
            JButton okButton = new JButton(BUTTON_OK);
            okButton.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    user = userField.getText();
                    domain = domainField.getText();
                    password = new String(passwordField.getPassword());
                    setVisible(false);
                }
            });
            
            JButton cancelButton = new JButton(BUTTON_CANCEL);
            cancelButton.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {
                    setVisible(false);
                }
            });
            
            contentPane.add(okButton);
            contentPane.add(cancelButton);
            
            contentPane.setBorder(BorderFactory.createEmptyBorder(10, 20, 5, 20));
            
            JPanel content = new JPanel();
            content.setLayout(new BorderLayout());
            content.add(contentPane, BorderLayout.PAGE_START);
            
            setContentPane(content);
            getRootPane().setDefaultButton(okButton);
        }
        
        String getUser() {
            return user;
        }
        
        String getDomain() {
            return domain;
        }
        
        String getPassword() {
            return password;
        }
    }