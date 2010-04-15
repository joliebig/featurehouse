

/*
 *has a print class (from AarbTeam2000� -http://wwww.arabteam2000.com-) for printing the documents
 */


class Notepad {
	private JMenuItem prinT;
	private JButton printButton;
	
	Notepad() {
        filE.add( prinT  = new JMenuItem( "Print", new ImageIcon( this.getClass().getResource( "images/print.gif" ) ) ) );

        prinT.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_P, ActionEvent.CTRL_MASK ) );
		
        toolBar.add( printButton = new JButton( new ImageIcon( this.getClass().getResource( "images/print.gif" ) ) ) );

        printButton.setToolTipText( "Print" );

        prinT.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.prinT();
            }
        } );

        printButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.prinT();
            }
        } );


	}

}