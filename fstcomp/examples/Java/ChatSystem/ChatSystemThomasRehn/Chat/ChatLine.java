

public class ChatLine {
	private final String user;
	private final String line;
	private final long time;
	
	public ChatLine(String user, String line, long time) {
		this.time = time;
		this.line = line.trim();
		this.user = user;
	}

	public String getLine() {
		return line;
	}

	public long getTime() {
		return time;
	}

	public String getUser() {
		return user;
	}
}
