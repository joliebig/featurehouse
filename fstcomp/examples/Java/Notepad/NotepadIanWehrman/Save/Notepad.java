
class Notepad {

    JMenuItem savE, saveAS;
    JButton saveButton, saveAsButton;


    void initMenuItems() {
        
    	original();

        filE.addSeparator();
        filE.add(savE   = new JMenuItem("Save", new ImageIcon(this.getClass().getResource("images/save.gif"))));
        filE.add(saveAS = new JMenuItem("Save As", new ImageIcon(this.getClass().getResource("images/saveAs.gif"))));

    }

    void initAccelerators() {


    	original();

        savE.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK));

    }



    void initToolBar() {

    	original();

        toolBar.addSeparator();        
        toolBar.add(saveButton  = new JButton(new ImageIcon(this.getClass().getResource("images/save.gif"))));
        toolBar.add(saveAsButton= new JButton(new ImageIcon(this.getClass().getResource("images/saveAs.gif"))));


    }


    void initToolTips() {

    	original();
        saveButton.setToolTipText("Save");
        saveAsButton.setToolTipText("Save As");

    }

    void initActionListeners() {

    	original();

        savE.addActionListener(new ActionListener(){
                public void actionPerformed(ActionEvent ae){
                    actions.savE();
                }
            });
        saveAS.addActionListener(new ActionListener(){
                public void actionPerformed(ActionEvent ae){
                    actions.saveAs();
                }
            });

        saveButton.addActionListener(new ActionListener(){
                public void actionPerformed(ActionEvent ae){
                    actions.savE();
                }
            });
        saveAsButton.addActionListener(new ActionListener(){
                public void actionPerformed(ActionEvent ae){
                    actions.saveAs();
                }
            });
    }

    void setup() {

    	original();
        
    }

}
