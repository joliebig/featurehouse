

public class Connection {
	public void run() {
		String clientName = socket.getInetAddress().toString();
		try {
			while (connectionOpen) {
				try {
					Object msg = inputStream.readObject();
					handleIncomingMessage(clientName, msg);
				} catch (ClassNotFoundException e) {
					e.printStackTrace();
				}

			}
		} catch (IOException ex) {
			ex.printStackTrace();
		} finally {
			server.removeConnection(this);
			TextMessage quitNotification =  new TextMessage(clientName + " has left.");
			server.broadcast(quitNotification);
			try {
				socket.close();
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}
	}
	protected void handleIncomingMessage(String name, Object msg) {
		if (msg instanceof LoginRequest){
			String uName = ((LoginRequest)msg).Usrname;
			String pwd = ((LoginRequest)msg).Pwd;
			if (server.userData.containsKey(uName)){
				if (((String)server.userData.get(uName)).equals(pwd)){
					try{
						LoginReply reply = new LoginReply();
						reply.status = true;
						outputStream.writeObject( reply );
					} catch( IOException w ){w.toString();}
				}else{
					try{
						LoginReply reply = new LoginReply();
						reply.status = false;
						outputStream.writeObject( reply );
					} catch( IOException w ){w.toString();}
				}	
			}else{
				try{
					LoginReply reply = new LoginReply();
					reply.status = false;
					outputStream.writeObject( reply );
				} catch( IOException w ){w.toString();}
			} 
		}
		original(name,msg);
			
	} 
}