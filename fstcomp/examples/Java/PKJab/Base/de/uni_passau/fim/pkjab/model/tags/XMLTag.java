

public class XMLTag extends AbstractXMLTag {
	
	private final String name;
	//private String content = null;
	
	public XMLTag(String name, String uri) {
		super(uri);
		if (name == null) {
			throw new IllegalArgumentException();
		}
		this.name = name;
	}

	public String getName() {
		return name;
	}

/*	public boolean hasContent() {
		return content != null;
	}
	
	public void setContent(String content) {
		if (this.content == null) {
			this.content = content;
		} else {
			System.err.println("Trying to reassign content of tag " + this);
		}
	}

	public String getContent() {
		return content;
	}*/
}
