

public class ChatGui
{
	protected JComboBox box;
	
	public void addCom()
	{
		original();
		
		String[] colors = { "Black", "Blue", "Red", "Green", "Orange" };
		box = new JComboBox(colors);
		box.setSelectedIndex(0);
		box.addActionListener(this);
		panel.add(box);
		
	}
	
	

	public void actionPerformed(ActionEvent e)
	{
		original(e);
		
		if (e.getSource() == box) {
			switch (box.getSelectedIndex()) {
			case 0:
				currentColor = Color.black;
				break;
			case 1:
				currentColor = Color.blue;
				break;
			case 2:
				currentColor = Color.red;
				break;
			case 3:
				currentColor = Color.green;
				break;
			case 4:
				currentColor = Color.orange;
				break;

			default:
				currentColor = Color.black;
				break;
			}

		}	
		
	}


}