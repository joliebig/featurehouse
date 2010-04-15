


public class Connection {
	/*if[AUTHO]*/	
	private boolean connectionAuthorized = false;
	private final static String AUTHORIZATIONMSG = "EPMD";
	/*end[AUTHO]*/
	
	
	protected void handleIncomingMessage(Object msg) {
		if (msg instanceof TextMessage) {
			String tmpContent = ((TextMessage) msg).getContent();
			
			/*if[AUTHO]*/	
			if (!connectionAuthorized) {
				if (tmpContent.equals(AUTHORIZATIONMSG)) {
					connectionAuthorized = true;
					directSend("You are authorized now. Go ahead!");
				}
				else {
					directSend("Authorization failed. Try again with this here '"+AUTHORIZATIONMSG +"' :-)!");	
				}
				return;
			} 
			/*end[AUTHO]*/	
		}
			
		
		original(msg);
	
		
	}
	
	/*if[AUTHO]*/	
	/**
	 * sends a message directly to the client, without authorization
	 * 
	 * @param line
	 *            text of the message
	 */
	public void directSend(String line) {
		try {
			synchronized (outputStream) {
				outputStream.writeObject(new TextMessage(line));
			}
			outputStream.flush();
		} catch (IOException ex) {
			connectionOpen = false;
		}
	}
	/*end[AUTHO]*/	

	public void send(TextMessage msg) {
		/*if[AUTHO]*/	
		if (!connectionAuthorized)
			return;
		/*end[AUTHO]*/	
		
		
		original(msg);

	}

}