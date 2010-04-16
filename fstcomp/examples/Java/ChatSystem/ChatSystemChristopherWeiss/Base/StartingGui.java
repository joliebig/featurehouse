

import java.awt.GridLayout;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTextField;




public class StartingGui extends JFrame implements ActionListener {

	private static final long serialVersionUID = 1L;

	protected JTextField portField;
	protected JLabel portLabel;

	protected JTextField hostField;
	protected JLabel hostLabel;

	protected JLabel nameLabel;
	protected JTextField nameField;

	protected JButton conButton;

	protected Client client;
	protected String username;
	
	protected String password = "";

	public StartingGui() {

		System.out.println("starting gui...");

		this.getContentPane();

		this.setLayout(new GridLayout(10, 1));

		add();

		conButton = new JButton();
		conButton.setText("Connect...");

		conButton.addActionListener(this);

		this.add(conButton);

		this.setSize(300, 200);

		this.setVisible(true);

	}

	public void add()
	{
		hostLabel = new JLabel();
		hostLabel.setText("Enter host:");
		this.add(hostLabel);
		hostField = new JTextField();
		hostField.setText("localhost");
		this.add(hostField);

		portLabel = new JLabel();
		portLabel.setText("Enter port:");
		this.add(portLabel);
		portField = new JTextField();
		portField.setText("8080");
		this.add(portField);

		nameLabel = new JLabel();
		nameLabel.setText("Enter Name:");
		this.add(nameLabel);
		nameField = new JTextField();
		this.add(nameField);

		
	}	
		


	public void checkPassword(String s) {
		//System.out.println(s);
		if (s.equals("OK")) {

			
			this.setVisible(false);
		} 

	}

//	@Override
	public void actionPerformed(ActionEvent e) {
		
		if (e.getSource() == conButton) {

			this.client = new Client(hostField.getText(), Integer.parseInt(portField.getText()),nameField.getText(),this);
			AuthMessage msg = new AuthMessage("Client",password );//passwordField.getText()
			
			this.client.send(msg);

		}

	}

}