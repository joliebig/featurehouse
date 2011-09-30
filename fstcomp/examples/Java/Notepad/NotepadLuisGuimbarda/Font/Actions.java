

class Actions{
    public Fonts font = new Fonts();

    /**
     *@see FONTS.JAVA
     *this is a font class which is for changing the font, style & size
     */
    public void fonT(){
        font.setVisible(true); //setting the visible is true
        font.pack(); //pack the panel
        //making an action for ok button, so we can change the font
        font.getOkjb().addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                n.getTextArea().setFont(font.font());
                //after we chose the font, then the JDialog will be closed
                font.setVisible(false);
            }
        });
        //making an action for cancel button, so we can return to the old font.
        font.getCajb().addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent ae){
                //after we cancel the, then the JDialog will be closed
                font.setVisible(false);
            }
        });
    }
}
