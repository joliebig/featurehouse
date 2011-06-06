package net.sourceforge.jarbundler;

public class JavaProperty {

	

	private String name = null;
	private String value = null;

	

	public JavaProperty() {
	}

	
	public void setName(String name) {
		this.name = name;
	}

	
	public String getName() {

		if (this.name == null)
			return null;

		return this.name.trim();
	}

	

	public void setValue(String value) {
		this.value = value;
	}

	
	public String getValue() {

		if (this.value == null)
			return null;

		return this.value.trim();
	}

}
