

class Notepad {
	private JMenuItem selectALL;
        

	Notepad() {
        ediT.add( selectALL= new JMenuItem( "Select All" ) );
	  selectALL.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_A, ActionEvent.CTRL_MASK ) );

        selectALL.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent ae ) {
                actions.selectALL();
            }
        } );
       }
}