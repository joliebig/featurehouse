package de.uni_passau.fim.pkjab.view;

import java.text.DateFormat;
import java.util.Date;

class MessageFrame {

	private static final DateFormat dateFormat = DateFormat.getTimeInstance(DateFormat.MEDIUM);

 protected String messageToText(boolean incoming, Message message) {
		return dateFormat.format(new Date(message.getTime()))
			+ " " + original(incoming, message);
	}

}
