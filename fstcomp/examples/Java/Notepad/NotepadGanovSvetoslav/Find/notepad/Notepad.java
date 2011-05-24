package notepad;

class Notepad {
	//FIND FEATURE
	//fields 
  	private JMenuItem finD, findNexT;
	private JButton findButton;
	
	//initialization
	Notepad() {
		//MENU
		//menu items
		//find
		finD = new JMenuItem("Find", new ImageIcon(this.getClass().getResource("images/find.gif")));
		finD.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, ActionEvent.CTRL_MASK));
		//find next
		findNexT = new JMenuItem("Find Next");
		findNexT.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F3, ActionEvent.CTRL_MASK));
		//add items to menu
		if (ediT == null) {
			ediT = new JMenu("Edit");
		}
		ediT.add(finD);
		ediT.add(findNexT);
		ediT.addSeparator();
		menubar.add(ediT);
		//find
		finD.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.finD();
			}
		});
		//find next
		findNexT.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.findNexT();
			}
		});
		
		//TOOLBAR
		//find
		toolBar.add(findButton  = new JButton(new ImageIcon(this.getClass().getResource("images/find.gif"))));
		toolBar.addSeparator();
		//button listeners
		//find
	    findButton.setToolTipText("Find");
      //button/menu listeners
		findButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.finD();
			}
		});
	}	
}			
