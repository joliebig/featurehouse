

public class Client {
	LoginInterface login;
	
	public static void main(String[] args){
		boolean authentication = true;
		Client client = new Client(authentication);
		
	}
	Client(boolean validate){
		getConfiguration();
		login = new LoginInterface(this);
	}
	synchronized boolean connectToServer(){
		 
		
		try
		{
			System.out.println("Connecting to " + servAddr + " (port " + servPort
					+ ")...");
			InetAddress addr = InetAddress.getByName(servAddr); 
			s = new Socket(addr, servPort);
		}
		catch( UnknownHostException e )
		{
			JOptionPane.showMessageDialog( login,"Host Not Found, Reconfigure...","Host Lookup Error",JOptionPane.ERROR_MESSAGE );
			return false;
		}
		catch( IOException e )
		{
			JOptionPane.showMessageDialog( login,"Server Not Found, Check If Server Exists...","Socket Error",JOptionPane.ERROR_MESSAGE );
			return false;
		}

		try
		{
			this.outputStream = new ObjectOutputStream((s.getOutputStream()));
			this.inputStream = new ObjectInputStream((s.getInputStream()));
		}
		catch( IOException e )
		{
			JOptionPane.showMessageDialog( login,"Cannot Create Data Stream, Closing Client...","Data Stream Error",JOptionPane.ERROR_MESSAGE );
			try
			{
				s.close();
			}
			catch( IOException io_e )
			{}
			
			return false;
		}
		if (!verify()) return false;
			
		login.setVisible(false);
		//window = new Gui("EPMD Chat", this);
		initUI();
		thread = new Thread(this);
		thread.start();
				
		return true;
		
	}
	private boolean verify(){			
		LoginRequest req = new LoginRequest();
		
		req.Usrname = login.txtUserName.getText();
		req.Pwd = new String(login.txtPwd.getPassword());
		
		try{
			outputStream.writeObject(req);
			LoginReply reply = (LoginReply)inputStream.readObject();
			if( reply.status == false ){
				JOptionPane.showMessageDialog( login,"Username or password is wrong","Login failed",JOptionPane.ERROR_MESSAGE );
				return false;
			}
			
			return true;
		} catch( Exception e ){}
		
		return false;
	} 
}