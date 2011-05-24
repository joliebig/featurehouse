

class Center{
    Fonts f; //for using the object in the Fonts.java
    public Center(Fonts f){
        this.f = f;
    }
    public void fCenter(){
        //Centering the window
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        f.setLocation((screenSize.width-f.getWidth())/2,(screenSize.height-f.getHeight())/2);
    }
}
