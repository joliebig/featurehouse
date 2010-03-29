
class ConnectionCallback {
	
	public synchronized void secondInitWasSuccessful(Set features) throws IOException {
		super.secondInitWasSuccessful(features);
		if (connection.state != ConnectionState.DISCONNECTING) {
			connection.getOutput().getRoster();
		}
	}
	
	public synchronized void handleQuery(Iq iq) throws IOException {
		if (iq.getChild() != null && iq.type.equals("result") 
				&& iq.getChild().getName().equals("query") && iq.getChild().uri.equals("jabber:iq:roster")) {
			System.out.println("handleQuery: Got Roster: " + connection.getRoster());
		} else {
			super.handleQuery(iq);
		}
	}
}
