


public class UserInterface implements ChatLineListener{
	Client client;
	
	public UserInterface(Client client){
		this.client=client;	
		initUI();
	
	}
	
	public void newChatLine(String line){
	
	}
	
	public void initUI(){
		client.addLineListener(this);
		
	}
}