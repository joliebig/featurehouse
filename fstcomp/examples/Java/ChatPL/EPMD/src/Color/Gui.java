

import java.awt.Button;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.GridBagConstraints;
import java.awt.Insets;

import javax.swing.JColorChooser;

public class Gui {

	private Button button;
	private Color color;

	public void createLayout() {
		original();
		
		color = Color.black;
		button = new Button("Textfarbe");
		button.addActionListener(new ActionListener() {
				
			public void actionPerformed(ActionEvent e) {
				color = JColorChooser.showDialog(null, "color", color);
				button.setForeground(color);
			}
		});
		
		Insets insets = new Insets(0, 0, 0, 0);
		add(button, new GridBagConstraints(0, 2, 1, 1, 1.0, 0, GridBagConstraints.SOUTH, GridBagConstraints.HORIZONTAL, insets, 0, 0));
	}
	
	public void send(String msg) {
		StringBuilder builder = new StringBuilder();
		builder.append("[color=").append(color.getRGB()).append("]").append(msg);
		original(builder.toString());	
	}
	
	public void onMessageReceived(String text) {
		
		// Ist eine Farbe enthalten?
		int start = text.indexOf("[color=");
		if (start == -1) {
			original(text);
			return;
		}
		int end = text.indexOf(']', start);
		if (end == -1) {
			original(text);
			return;
		}
		
		// Farbe bestimmen
		String rgb = text.substring(start + 7, end);
		style = doc.addStyle("rgb", null);
		StyleConstants.setForeground(style, new Color(Integer.parseInt(rgb)));
		
		original(text.replace(text.substring(start, end + 1), ""));
	}
}