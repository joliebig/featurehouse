

class Notepad {

    JMenuItem finD, findNexT;
    JButton findButton;


    void initMenuItems() {
        
        original();

        ediT.addSeparator();

        ediT.add(finD = new JMenuItem("Find", new ImageIcon(this.getClass().getResource("images/find.gif"))));
        ediT.add(findNexT = new JMenuItem("Find Next"));


    }

    void initAccelerators() {


    	original();

        finD.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, ActionEvent.CTRL_MASK));
        findNexT.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F3, ActionEvent.CTRL_MASK));

    }



    void initToolBar() {

    	original();

        toolBar.addSeparator();        
        toolBar.add(findButton  = new JButton(new ImageIcon(this.getClass().getResource("images/find.gif"))));


    }


    void initToolTips() {

    	original();

        findButton.setToolTipText("Find");
    }

    void initActionListeners() {

    	original();


        finD.addActionListener(new ActionListener(){
                public void actionPerformed(ActionEvent ae){
                    actions.finD();
                }
            });

        findNexT.addActionListener(new ActionListener(){
                public void actionPerformed(ActionEvent ae){
                    actions.findNexT();
                }
            });

        findButton.addActionListener(new ActionListener(){
                public void actionPerformed(ActionEvent ae){
                    actions.finD();
                }
            });

    }

    void setup() {

    	original();
        
    }

}
