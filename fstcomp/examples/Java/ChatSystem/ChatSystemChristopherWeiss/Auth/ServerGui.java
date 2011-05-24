

public class ServerGui 
{
	JTextField passwordText;
	
			
	public void add() 
	{
		JLabel passwordLabel = new JLabel();
		passwordLabel.setText("Passwort: ");
		
		passwordText = new JTextField();
		
		pane.add(passwordLabel);
		pane.add(passwordText);
		
		original();
	}
	
	public void actionPerformed(ActionEvent e) {

		Object object = e.getSource();

		if (object == startButton) {
			
		try {
				this.setVisible( false );
				System.out.println("Server started...");
				Server server = new Server(Integer.parseInt(portText.getText()),passwordText.getText());
			} catch (NumberFormatException e1) {
				e1.printStackTrace();
			} catch (IOException e1) {
				e1.printStackTrace();
			}
			

		}

	}
}