package notepad;

class Notepad {
	//HELP FEATURE
	//fields
	private JMenu helP;
	private JMenuItem abouT;
	private JButton aboutButton;
	
	//initialization
	Notepad() {
		//MENU
		//menu items
		//help
		helP   = new JMenu("Help");
		helP.add(abouT = new JMenuItem("About Notepad", new ImageIcon(this.getClass().getResource("images/about.gif"))));
		helP.setMnemonic('h');
		// add items to menu
		menubar.add(helP);
		//
		abouT.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.abouT();
			}
		});
		//TOOLBAR
		//about
		aboutButton = new JButton(new ImageIcon(this.getClass().getResource("images/about.gif")));
		aboutButton.setToolTipText("About Notepad");
		//add buttons to toolbar
		toolBar.add(aboutButton);
		toolBar.addSeparator();
      	//button/menu listeners
		aboutButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.abouT();
			}
		});		
	}
	//END HELP FEATURE	
}		
