package de.uni_passau.fim.pkjab.model.xmpp;

import java.io.IOException;

import javax.security.sasl.SaslClient;


public class XMPPAuthHandler {

	private final SaslClient sasl;
	private final XMPPWriter output;
	
	public XMPPAuthHandler(SaslClient sasl, XMPPWriter output) {
		this.sasl = sasl;
		this.output = output;
	}
	
	void challenge(byte[] challenge) throws IOException {
		System.out.println("authenticationChallenge");
		output.sendAuthResponse(sasl.evaluateChallenge(challenge));
	}
	
	void successful(byte[] finalResponse) {
		System.out.println("Authentication successful! YEEEEHAA!");
	}
}
