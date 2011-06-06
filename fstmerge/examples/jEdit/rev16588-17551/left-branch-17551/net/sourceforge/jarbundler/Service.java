package net.sourceforge.jarbundler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;




public class Service {
	private static final List EMPTYLIST = new ArrayList(0);

	
	
	private String portName = null;
	

	
	private String message = null;
	

	
	private String menuItem = null;
	
	
	private String[] sendTypes = null;
	

	
	private String[] returnTypes = null;
	

	
	private String keyEquivalent = null;
	

	
	private String userData = null;
	

	
	private String timeout = null;
	

	public void setPortName(String portName) {
		this.portName = portName;
	}
	
	public String getPortName() {
		return portName;
	}
	
	public void setMessage(String message) {
		this.message = message;
	}
	
	public String getMessage() {
		return message;
	}
	
	public void setMenuItem(String menuItem) {
		this.menuItem = menuItem;

	}
	
	public String getMenuItem() {
		return menuItem;
	}
	
	public void setSendTypes(String sendTypes) {
		this.sendTypes = sendTypes.split("[\\s,]");
	}
	
	public List getSendTypes() {
		return (sendTypes == null) ? EMPTYLIST : Arrays.asList(sendTypes);
	}
	
	public void setReturnTypes(String returnTypes) {
		this.returnTypes = returnTypes.split("[\\s,]");
	}
	
	public List getReturnTypes() {
		return (returnTypes == null) ? EMPTYLIST : Arrays.asList(returnTypes);
	}
	
	public void setKeyEquivalent(String keyEquivalent) {
		this.keyEquivalent = keyEquivalent;
	}
	
	public String getKeyEquivalent() {
		return keyEquivalent;
	}
	
	public void setUserData(String userData) {
		this.userData = userData;
	}
	
	public String getUserData() {
		return userData;
	}
	
	public void setTimeout(String timeout) {
		this.timeout = timeout;
	}
	
	public String getTimeout() {
		return timeout;
	}
}
