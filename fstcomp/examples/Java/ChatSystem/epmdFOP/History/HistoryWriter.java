

import java.awt.Color;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class HistoryWriter implements ChatLineListener, ConnectionInterface {

	private File history;
	private BufferedWriter historyWriter;
	
	public HistoryWriter(String file) {
		history = new File(file);
		System.out.println(history.getAbsolutePath());
		try {
			historyWriter = new BufferedWriter(new FileWriter(history, true));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void newChatLine(String line) {
		try {
			historyWriter.write(line);
			historyWriter.flush();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void newChatLine(String line, Color color) {
		newChatLine(line);
	}

	protected void finalize() {
		try {
			this.historyWriter.close();
		} catch (IOException e) { }
	}

	public void send(TextMessage msg) {
		newChatLine(msg.getContent() + "\n");
	}
}
