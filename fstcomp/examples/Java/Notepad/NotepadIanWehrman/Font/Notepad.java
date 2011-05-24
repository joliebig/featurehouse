
class Notepad {

    JMenuItem  fonT;
    JButton fontButton;


    void initMenuItems() {
        
    	original();

        ediT.addSeparator();
        formaT.add(fonT = new JMenuItem("Font", new ImageIcon(this.getClass().getResource("images/font.gif"))));

    }



    void initToolBar() {

    	original();

        toolBar.addSeparator();        
toolBar.add(fontButton  = new JButton(new ImageIcon(this.getClass().getResource("images/font.gif"))));


    }


    void initToolTips() {

    	original();
        fontButton.setToolTipText("Font");
    }

    void initActionListeners() {

    	original();

        fonT.addActionListener(new ActionListener(){
                public void actionPerformed(ActionEvent ae){
                    actions.fonT();
                }
            });

        fontButton.addActionListener(new ActionListener(){
                public void actionPerformed(ActionEvent ae){
                    actions.fonT();
                }
            });

    }

    void setup() {

    	original();
        
    }

}
