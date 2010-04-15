package notepad;

class Notepad {
	//fields 
	private JMenuItem cuT, copY, pastE, selectALL;
	private JButton cutButton, copyButton, pasteButton;
	
	//initialization
	Notepad() {
  	      //MENU
		//menu items
		//cut
		cuT = new JMenuItem("Cut",  new ImageIcon(this.getClass().getResource("images/cut.gif")));
		cuT.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X, ActionEvent.CTRL_MASK));
		//copy
		copY = new JMenuItem("Copy", new ImageIcon(this.getClass().getResource("images/copy.gif")));
		copY.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, ActionEvent.CTRL_MASK));
		//paste
		pastE= new JMenuItem("Paste",new ImageIcon(this.getClass().getResource("images/paste.gif")));
		pastE.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V, ActionEvent.CTRL_MASK));
		//select all 
		selectALL= new JMenuItem("Select All");
		selectALL.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A, ActionEvent.CTRL_MASK));
		//add items to menu
		if (ediT == null) {
			ediT = new JMenu("Edit");
		}
		ediT.add(cuT);
		ediT.add(copY);
		ediT.add(pastE);
		ediT.add(selectALL);
		ediT.setMnemonic('e');
		menubar.add(ediT);
		ediT.addSeparator();
		//menu listeners
		//cut
		cuT.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.cuT();
			}
		});
		//copy
		copY.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.copY();
			}
		});
		//paste
		pastE.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.pastE();
			}
		});
		//select all
		selectALL.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.selectALL();
			}
		});

		//TOOLBAR
		//cut
		cutButton   = new JButton(new ImageIcon(this.getClass().getResource("images/cut.gif")));
		//copy
		copyButton  = new JButton(new ImageIcon(this.getClass().getResource("images/copy.gif")));
		//paste
		pasteButton = new JButton(new ImageIcon(this.getClass().getResource("images/paste.gif")));
		//add buttons to toolbar
		toolBar.add(cutButton);
		cutButton.setToolTipText("Cut");
		toolBar.add(copyButton);
		copyButton.setToolTipText("Copy");
		toolBar.add(pasteButton);
		pasteButton.setToolTipText("Paste");
		toolBar.addSeparator();
		//button listeners
		cutButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.cuT();
			}
		});
		copyButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.copY();
			}
		});
		pasteButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent ae){
				actions.pastE();
			}
		});
	}	
}			
