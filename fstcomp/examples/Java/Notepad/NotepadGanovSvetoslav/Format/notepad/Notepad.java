package notepad;

import javax.swing.JCheckBoxMenuItem;

class Notepad {
	//fields
	private JMenu formaT;
	private JMenuItem fonT;
	private JCheckBoxMenuItem lineWraP;
	private JButton fontButton;
	
	//initialization
	Notepad() {
		//MENU
		//menu items
		//font
		fonT = new JMenuItem("Font", new ImageIcon(this.getClass().getResource("images/font.gif")));
		//line wrap
		lineWraP = new JCheckBoxMenuItem("Line Wrap");
		//format
		formaT = new JMenu("Format");
		formaT.add(fonT);
		formaT.add(lineWraP);
		formaT.setMnemonic('o');
		// add items to menu
		menubar.add(formaT);
		//menu listeners
		//Format
		lineWraP.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.lineWraP();
			}
		});
		fonT.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.fonT();
			}
		});
		
		//TOOLBAR
		//font
		fontButton  = new JButton(new ImageIcon(this.getClass().getResource("images/font.gif")));
		fontButton.setToolTipText("Font");
		//add buttons to toolbar
		toolBar.add(fontButton);
		toolBar.addSeparator();
        //button/menu listeners
		fontButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.fonT();
			}
		});
	}
	
	//for using lineWrap & textArea @Actions.java
	public JCheckBoxMenuItem getLineWrap(){
		return lineWraP;
	}
}			
