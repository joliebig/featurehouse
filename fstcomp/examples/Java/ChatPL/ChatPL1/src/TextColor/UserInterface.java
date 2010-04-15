

import javax.swing.JComboBox;
import javax.swing.JPanel;
import java.awt.Color;
public class UserInterface {

	private JComboBox farbauswahl;
	private JPanel panel;
	public void initUI(){
		original();
		farbauswahl=new JComboBox(new DefaultComboBoxModel(new String[] { "Schwarz", "Rot", "Blau", "Gr�n", "Gelb" }));
		jPanel_add.add(farbauswahl);
	}
	
	
	/*	
	public void newChatLine(String line){
			jTextArea.append(line);
	}
	*/
	
	public void handleMessage(){
		TextMessage msg=new ColorMessage(jTextField.getText());
		((ColorMessage) msg).setColor(farbauswahl.getSelectedItem().toString());
		client.send(msg);
		jTextField.setText("");
	}
	
	private void jTextField1ActionPerformed(ActionEvent evt) {
		
		TextMessage msg=new ColorMessage(jTextField.getText());
		((ColorMessage) msg).setColor(farbauswahl.getSelectedItem().toString());
		client.send(msg);
		jTextField.setText("");
	}	
	
}