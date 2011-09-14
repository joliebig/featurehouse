import javax.swing.JLabel; 

public  class  GuiField  extends JLabel {
	
    private int boxIndex;

	
    private int fieldIndex;

	

    public int getBoxIndex() {
        return boxIndex;
    }

	

    public int getFieldIndex() {
        return fieldIndex;
    }

	

    public GuiField(int boxIndex, int fieldIndex) {
        this.boxIndex = boxIndex;
        this.fieldIndex = fieldIndex;
    }


}
