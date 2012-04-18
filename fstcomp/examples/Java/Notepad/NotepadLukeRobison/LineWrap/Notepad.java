

class Notepad {
	private JCheckBoxMenuItem lineWraP;
	
	public JCheckBoxMenuItem getLineWrap() {
        return lineWraP;
    }
	
	Notepad() {
	    formaT.add( lineWraP = new JCheckBoxMenuItem( "Line Wrap" ) );

        lineWraP.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.lineWraP();
            }
        } );
}
}