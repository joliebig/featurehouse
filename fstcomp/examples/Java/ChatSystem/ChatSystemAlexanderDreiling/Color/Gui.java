


public class Gui {
	/*if[COLOR]*/
	protected Color curTxtCol;
	protected Button colorButton;
	/*end[COLOR]*/
	
	protected void addGraphicalElements() {
		/*if[COLOR]*/
		curTxtCol = Color.BLACK;
		colorButton = new Button("Color");
		add(colorButton, new GridBagConstraints(1,0,1,1,0,0,GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0,0,0,0),0,0));
		/*end[COLOR]*/
	}
	

	public boolean handleEvent(Event e) {
		if ((e.target == colorButton) && (e.id == Event.ACTION_EVENT)) {
			Color tmpColor;
			tmpColor = JColorChooser.showDialog(this,"Select a Text Color", curTxtCol);
			if (tmpColor != null){
				curTxtCol = tmpColor;
			}		
			inputField.setForeground(curTxtCol);
		}
		
		return original(e);
	}
	
	protected void modifyTextMessage(TextMessage txtMsg) {
		/*if[COLOR]*/
		txtMsg.addSetting(Utils.COLORKEY, Integer.toString(curTxtCol.getRGB()));
		/*end[COLOR]*/
		
		original(txtMsg);
	}

}