

class Actions {

    //for wraping the line & wraping the style word
    public void lineWraP(){
        if(n.getLineWrap().isSelected()){
            /**
             *make the line wrap & wrap style word is true
             *when the line wrap is selected
             */
            n.getTextArea().setLineWrap(true);
            n.getTextArea().setWrapStyleWord(true);
        }
        else{
            /**
             *make the line wrap & wrap style word is false
             *when the line wrap isn't selected
             */
            n.getTextArea().setLineWrap(false);
            n.getTextArea().setWrapStyleWord(false);
        }
    }



}