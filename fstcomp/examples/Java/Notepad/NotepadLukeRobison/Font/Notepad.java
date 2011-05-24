

class Notepad {
	private JMenuItem fonT;
	private JButton fontButton;

Notepad() {
        formaT.add( fonT = new JMenuItem( "Font", new ImageIcon( this.getClass().getResource( "images/font.gif" ) ) ) );

        toolBar.add( fontButton  = new JButton( new ImageIcon( this.getClass().getResource( "images/font.gif" ) ) ) );

        fontButton.setToolTipText( "Font" );

        fonT.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.fonT();
            }
        } );
		
        fontButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.fonT();
            }
        } );

	}
}