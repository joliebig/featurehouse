


import java.awt.Color;
import java.io.Serializable;
import java.util.HashMap;

/**
 * serializable message that can be send over the sockets between client and
 * server. 
 */
public class TextMessage implements Serializable {

	private static final long serialVersionUID = -9161595018411902079L;
	private String content;
	private String sender;
	
	private StringBuffer settings = new StringBuffer();
	
	public TextMessage(String content) {
		super();
		this.content = content;
	}
	
	public void setContent(String content) {
		this.content = content;
	}

	public String getContent() {
		return content;
	}
	
	public void setSender(String sender) {
		this.sender = sender;
	}

	public String getSender() {
		return sender;
	}
	
	public void setSetting(String settings) {
		this.settings.delete(0,this.settings.length());
		this.settings.append(settings);
		
	}
	public void addSetting(String key, String value) {
		settings.append("#"+key+"="+value);		
	}
	
	public String getSetting(String key) {	
		
		String[] settingsArray =  settings.toString().split("#");
		String[] tmpArray;
		
		for (int i = 0; i < settingsArray.length; i++) {
			tmpArray = settingsArray[i].split("=");
			if ((tmpArray.length == 2) && (tmpArray[0].equals(key)))
					return tmpArray[1];
		}
		return null;
	}
	
	public String getSettings() {	
		return settings.toString();
	}
	
	
}
