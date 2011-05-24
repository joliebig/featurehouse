

class Notepad {
	
	private JMenuItem savE, saveAS;
	private JButton saveButton, saveAsButton;


	Notepad() {
        filE.add( savE   = new JMenuItem( "Save", new ImageIcon( 
				  this.getClass().getResource( "images/save.gif" ) ) ) );
        filE.add( saveAS = new JMenuItem( "Save As", new ImageIcon(
				  this.getClass().getResource( "images/saveAs.gif" ) ) ) );
        savE.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_S, ActionEvent.CTRL_MASK ) );

        toolBar.add( saveButton  = new JButton( new ImageIcon( this.getClass().getResource( "images/save.gif" ) ) ) );
        toolBar.add( saveAsButton= new JButton( new ImageIcon( this.getClass().getResource( "images/saveAs.gif" ) ) ) );
        saveButton.setToolTipText( "Save" );
        saveAsButton.setToolTipText( "Save As" );

        savE.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.savE();
            }
        } );
        saveAS.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.saveAs();
            }
        } );

        saveButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.savE();
            }
        } );
        saveAsButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.saveAs();
            }
        } );
	}
	
}