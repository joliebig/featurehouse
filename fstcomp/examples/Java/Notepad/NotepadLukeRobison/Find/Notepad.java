

class Notepad {
	private JMenuItem finD, findNexT;
	private JButton findButton;

	Notepad() {
        ediT.add( finD = new JMenuItem( "Find", new ImageIcon( this.getClass().getResource( "images/find.gif" ) ) ) );
        ediT.add( findNexT = new JMenuItem( "Find Next" ) );

        toolBar.add( findButton  = new JButton( new ImageIcon( this.getClass().getResource( "images/find.gif" ) ) ) );

        finD.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_F, ActionEvent.CTRL_MASK ) );
        findNexT.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_F3, ActionEvent.CTRL_MASK ) );
		
        findButton.setToolTipText( "Find" );

        finD.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.finD();
            }
        } );
        findNexT.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.findNexT();
            }
        } );	

        findButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.finD();
            }
        } );
}
}