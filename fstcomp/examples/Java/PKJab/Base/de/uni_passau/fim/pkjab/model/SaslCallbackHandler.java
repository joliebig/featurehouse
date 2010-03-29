

import java.io.IOException;

import pkjab.de.uni_passau.fim.pkjab.model.Connection;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

class SaslCallbackHandler implements CallbackHandler {

	private final Connection connection;
	
	SaslCallbackHandler(Connection connection) {
		this.connection = connection;
	}

		public void handle(Callback[] callbacks) throws IOException,
				UnsupportedCallbackException {
			
			for (int i = 0; i < callbacks.length; i++) {
				if (callbacks[i] != null) {
					if (callbacks[i] instanceof NameCallback) {
						((NameCallback)(callbacks[i])).setName(connection.getJid().getUser());
						
					} else if (callbacks[i] instanceof PasswordCallback) {
						((PasswordCallback)callbacks[i]).setPassword(connection.password.toCharArray());
					}
				}
			}
		}							

}