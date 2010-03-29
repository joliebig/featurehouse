
public class ConnectionState {

	public static final ConnectionState OFFLINE = new ConnectionState();
	public static final ConnectionState CONNECTING = new ConnectionState();
	public static final ConnectionState AUTHENTICATING = new ConnectionState();
	public static final ConnectionState ONLINE = new ConnectionState();
	public static final ConnectionState DISCONNECTING = new ConnectionState();
}
