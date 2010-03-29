

import java.io.IOException;

import pkjab.de.uni_passau.fim.pkjab.model.tags.AbstractXMLTag;
import pkjab.de.uni_passau.fim.pkjab.model.tags.Iq;
import pkjab.de.uni_passau.fim.pkjab.model.tags.XMLTag;

class ConnectionCallback {
		
	private static final XMLTag VERSION_QUERY_TAG = new XMLTag("query", "jabber:iq:version");
		
	public synchronized void handleQuery(Iq iq) throws IOException {
		AbstractXMLTag child = iq.getChild();
		if (child != null && iq.type.equals("get") && child.equals(VERSION_QUERY_TAG)) {
			connection.getOutput().sendVersion("PKJab", "0.1",
				System.getProperty("os.name") + " " + System.getProperty("os.version")
				+ " " + System.getProperty("os.arch"), iq.id, iq.from, iq.to);

		} else {
			super.handleQuery(iq);
		}
	}
}
