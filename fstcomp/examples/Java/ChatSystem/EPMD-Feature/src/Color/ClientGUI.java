

public class ClientGUI {
	private JCheckBox colorBox;
	private boolean color = false;
	
	private final static String COLOR_PREFIX = "{color}";
	private final static String NOCOLOR_PREFIX = "{nocolor}";
	
	public void initGUI() throws Exception {
		original();
		colorBox = new JCheckBox("Color");
		colorBox.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				color = colorBox.isSelected();
			}
		});
		
		add(colorBox);
	}
	
	public String postMessageReceived(String newMessage) {
		String msg = original(newMessage);
		
		if (msg.contains(COLOR_PREFIX))
			msg = "(colored) " + msg.replace(COLOR_PREFIX, "");
		else if (msg.contains(NOCOLOR_PREFIX))
			msg = msg.replace(NOCOLOR_PREFIX, "");
		
		return msg;
	}
	
	public String preMessageSent(String msg) {
		msg = original(msg);
		if (color)
			msg = COLOR_PREFIX + msg;
		else
			msg = NOCOLOR_PREFIX + msg;
		return msg;
	}
}