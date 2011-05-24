

public class StartingGui 
{
	
	public void checkPassword(String s) {
		
		 
		original(s);
		if (s.equals("OK")) {

			new ChatGui("Chat", client, nameField.getText());
			
		}
	}
	
}