package net.sourceforge.jarbundler;

import java.lang.String;

import java.io.File;

import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;




public class DocumentType {

	private static final List EMPTYLIST = new ArrayList(0);

	
	public String name = null;

	

	public String[] extensions = null;
	

	public String[] osTypes = null;
	

	public String[] mimeTypes = null;

	

	public File iconFile = null;
	

	public String role = null;

	

	public boolean isBundle = false;

	
	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	
	public void setExtensions(String extensions) {
		this.extensions = extensions.split("[\\s,]");
	}

	public List getExtensions() {
		return (extensions == null) ? EMPTYLIST : Arrays.asList(extensions);
	}

	
	public void setOSTypes(String osTypes) {
		this.osTypes = osTypes.split("[\\s,]");
	}

	public List getOSTypes() {
		return (osTypes == null) ? EMPTYLIST : Arrays.asList(osTypes);
	}

	
	public void setMimeTypes(String mimeTypes) {
		this.mimeTypes = mimeTypes.split("[\\s,]");
	}

	public List getMimeTypes() {
		return (mimeTypes == null) ? EMPTYLIST : Arrays.asList(this.mimeTypes);
	}

	
	public void setIconFile(File iconFile) {
		this.iconFile = iconFile;
	}

	public File getIconFile() {
		return iconFile;
	}

	
	public void setRole(String role) {
		this.role = role;
	}

	public String getRole() {
		return role;
	}

	
	public void setBundle(boolean isBundle) {
		this.isBundle = isBundle;
	}

	public boolean isBundle() {
		return isBundle;
	}

}
