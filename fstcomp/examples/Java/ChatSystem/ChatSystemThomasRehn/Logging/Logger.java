


import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Date;

public class Logger {
	private final String id;
	private BufferedWriter bw;
	
	public Logger(String id) {
		this.id = id;	
	}
	
	private BufferedWriter getWriter() throws IOException {
		if (bw == null) {
			bw =  new BufferedWriter(new FileWriter(id + ".log"));
		}
		return bw;
	}
	
	public void log(String message) throws IOException {
		getWriter().write(new Date() + ": " + message);
		getWriter().flush();
	}
}
