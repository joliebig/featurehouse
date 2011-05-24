package merger;

public class ContentMergeException extends Exception {
	private static final long serialVersionUID = -4331950077931514300L;
	private String content;
	public ContentMergeException(String c) {
		super();
		content = c;
	}
	public String toString() {
		return "ContentMergeException: invalid structure of content: " + content;		
	}
}
