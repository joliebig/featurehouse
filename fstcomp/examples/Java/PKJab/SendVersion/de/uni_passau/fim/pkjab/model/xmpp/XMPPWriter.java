
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import pkjab.de.uni_passau.fim.pkjab.model.messages.Message;
import pkjab.de.uni_passau.fim.pkjab.util.Base64;
import pkjab.de.uni_passau.fim.pkjab.util.UserState;

class XMPPWriter {

	public void sendVersion(String name, String version, String os, String id,
			String to, String from)	throws IOException {
		sendIqQuery("result", "jabber:iq:version", String.format(
			"<name>%s</name><version>%s</version><os>%s</os>", name, version, os),
			id, to, from);
	}
}
