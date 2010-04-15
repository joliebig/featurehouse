

public class StartingGui 
{
	protected JTextField passwordField;
	protected JLabel passwordLabel;
	
	public void add()
	{
		original();
		
		passwordLabel = new JLabel();
		passwordLabel.setText("Enter password:");
		this.add(passwordLabel);
		passwordField = new JTextField();
		this.add(passwordField);
		
	}
	
	public void actionPerformed(ActionEvent e) {

		password = passwordField.getText();
		original(e);
		
	}
}