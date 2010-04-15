

class Notepad {

    private JMenuItem abouT;
    private JButton aboutButton;
    private JMenu helP;

    Notepad() {
        Menubar.add( helP   = new JMenu( "Help" ) );
        helP.add( abouT = new JMenuItem( "About Notepad", new ImageIcon( this.getClass().getResource( "images/about.gif" ) ) ) );
        helP.setMnemonic( 'h' );
        toolBar.add( aboutButton = new JButton( new ImageIcon( this.getClass().getResource( "images/about.gif" ) ) ) );
        aboutButton.setToolTipText( "About Notepad" );

        abouT.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.abouT();
            }
        } );
        aboutButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.abouT();
            }
        } );
        }
}