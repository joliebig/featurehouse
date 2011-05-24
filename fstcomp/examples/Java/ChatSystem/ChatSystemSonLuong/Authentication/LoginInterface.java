

import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JFrame;
import java.awt.Dimension;
import javax.swing.JButton;
import javax.swing.JTextField;
import java.awt.ComponentOrientation;
import java.awt.Rectangle;
import javax.swing.JLabel;
import java.awt.event.KeyEvent;
import javax.swing.SwingConstants;
import javax.swing.JPasswordField;
import java.io.*;
import java.net.*;
import javax.swing.*;


public class LoginInterface extends JFrame {
	
	
	Client client;
		
	private static final long serialVersionUID = 1L;
	private JPanel jContentPane = null;
	private JButton bLogIn = null;
	protected JTextField txtUserName = null;
	private JLabel lbUsrname = null;
	private JLabel lbPwd = null;
	protected JPasswordField txtPwd = null;
	/**
	 * This is the default constructor
	 */
	public LoginInterface(Client client) {
		super();
		initialize();
		this.client = client;
		setVisible(true);
		txtUserName.requestFocus();
		
	}

	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize() {
		this.setSize(248, 316);
		this.setContentPane(getJContentPane());
		this.setTitle("EPMDChat_Client");
		this.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				windowClosed(null);
				//client.shutDown();
			}
		});
		
	}
	
	
	/**
	 * This method initializes jContentPane
	 * 
	 * @return javax.swing.JPanel
	 */
	private JPanel getJContentPane() {
		if (jContentPane == null) {
			lbPwd = new JLabel();
			lbPwd.setBounds(new Rectangle(60, 105, 121, 16));
			lbPwd.setHorizontalAlignment(SwingConstants.LEFT);
			lbPwd.setText("Password:");
			lbPwd.setDisplayedMnemonic(KeyEvent.VK_UNDEFINED);
			lbUsrname = new JLabel();
			lbUsrname.setBounds(new Rectangle(60, 45, 121, 16));
			lbUsrname.setDisplayedMnemonic(KeyEvent.VK_UNDEFINED);
			lbUsrname.setHorizontalAlignment(SwingConstants.LEFT);
			lbUsrname.setText("User name:");
			jContentPane = new JPanel();
			jContentPane.setLayout(null);
			jContentPane.add(getTxtUserName(), null);
			jContentPane.add(getBLogIn(), null);
			jContentPane.add(lbUsrname, null);
			jContentPane.add(lbPwd, null);
			jContentPane.add(getTxtPwd(), null);
		}
		return jContentPane;
	}

	/**
	 * This method initializes bLogIn	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getBLogIn() {
		if (bLogIn == null) {
			bLogIn = new JButton();
			bLogIn.setText("Sign In");
			bLogIn.setPreferredSize(new Dimension(80, 23));
			bLogIn.setBounds(new Rectangle(75, 180, 91, 27));
			bLogIn.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
			bLogIn.addMouseListener(new java.awt.event.MouseAdapter() {
				public void mouseClicked(java.awt.event.MouseEvent e) {
					//getConfiguration();
					client.connectToServer();
					
				}
			});
		}
		return bLogIn;
	}
	
	
	

	/**
	 * This method initializes txtUserName	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getTxtUserName() {
		if (txtUserName == null) {
			txtUserName = new JTextField();
			txtUserName.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
			txtUserName.setPreferredSize(new Dimension(20, 20));
			txtUserName.setBounds(new Rectangle(60, 75, 121, 23));
			txtUserName.setHorizontalAlignment(JTextField.LEFT);
			txtUserName.addMouseListener(new java.awt.event.MouseAdapter() {
				public void mouseClicked(java.awt.event.MouseEvent e) {
					txtUserName.selectAll();
				}
			});
		}
		return txtUserName;
	}

	/**
	 * This method initializes txtPwd	
	 * 	
	 * @return javax.swing.JPasswordField	
	 */
	private JPasswordField getTxtPwd() {
		if (txtPwd == null) {
			txtPwd = new JPasswordField();
			txtPwd.setBounds(new Rectangle(60, 135, 121, 24));
			txtPwd.addMouseListener(new java.awt.event.MouseAdapter() {
				public void mouseClicked(java.awt.event.MouseEvent e) {
					txtPwd.setText(null);
				}
			});
		}
		return txtPwd;
	}

}  //  @jve:decl-index=0:visual-constraint="198,31"
