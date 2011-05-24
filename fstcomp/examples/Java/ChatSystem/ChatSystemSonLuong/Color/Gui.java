

import java.awt.*;
import java.util.*;
import javax.swing.*;
import java.awt.event.*;


public class Gui {
	protected JMenuBar menuBar;
	protected JMenu colorMenu;
	protected JMenuItem selectColor;
	
	void InitLayout(){
		original();
		menuBar = new JMenuBar();
		colorMenu = new JMenu("Color");
		colorMenu.setMnemonic(KeyEvent.VK_C);
		selectColor = new JMenuItem("Select a color");
		colorMenu.add(selectColor);
		menuBar.add(colorMenu);
		add("North",menuBar);
		outputTextbox.setForeground(chatClient.textColor);
		inputField.setForeground(chatClient.textColor);
		selectColor.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				Color newColor = JColorChooser.showDialog(null, "Choose a color", chatClient.textColor);
				chatClient.textColor = newColor;
				outputTextbox.setForeground(chatClient.textColor);
				inputField.setForeground(chatClient.textColor);			
			}
		});
	}
	public void newChatLine(TextMessage msg) {
		Color msgColor = msg.getColor();
		System.out.println(msgColor.toString());
		outputTextbox.setForeground(msg.getColor());
		original(msg);
	}
}	