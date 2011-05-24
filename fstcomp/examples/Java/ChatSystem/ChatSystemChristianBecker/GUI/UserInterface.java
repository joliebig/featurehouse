

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;
import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.border.BevelBorder;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


public class UserInterface {
	private JFrame frame;
	private JPanel jPanel;
	public JPanel jPanel_add;
	public JTextField jTextField;
	public JTextArea jTextArea;
	private JScrollPane jScrollPane;
	
	
	public void newChatLine(String line){
		
		jTextArea.append(line);
	}
	
	
	public void initUI(){
		
		frame=new JFrame();
		original();
		frame.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
		frame.getContentPane().setLayout(null);
		frame.setPreferredSize(new java.awt.Dimension(408, 315));
		{
				frame.setSize(408, 320);
				frame.setVisible(true);
				jScrollPane = new JScrollPane();
				frame.getContentPane().add(jScrollPane);
				jScrollPane.setBounds(12, 12, 368, 196);
				{
					jTextArea = new JTextArea();
					jScrollPane.setViewportView(jTextArea);
					jTextArea.setBounds(12, 12, 368, 196);
					jTextArea.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
					jTextArea.setEditable(false);
				}
		}
		{
				
				jTextField = new JTextField();
				jTextField.setText("");
				frame.getContentPane().add(jTextField);
				
				jTextField.setBounds(12, 220, 368, 21);
				
				jTextField.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent evt) {
						jTextField1ActionPerformed(evt);
					}
				});
		}
		{
				jPanel_add = new JPanel();
				frame.getContentPane().add(jPanel_add);
				jPanel_add.setBounds(12, 247, 368, 34);
		
		}
		frame.pack();
		frame.setVisible(true);
		
		
	}
	
	public void handleMessage(){
		client.send(jTextField.getText());
		jTextField.setText("");
	}
	
	private void jTextField1ActionPerformed(ActionEvent evt) {
		handleMessage();
		/*
		
		client.send(jTextField.getText());
		jTextField.setText("");
		*/
	}
	
	
}