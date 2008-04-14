package cide.astgen;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import junit.framework.Assert;

import org.junit.Before;

public class WhiteSpaceTest {

	protected ByteArrayOutputStream byteStream;
	protected PrintStream printStream;

	public WhiteSpaceTest() {
		super();
	}

	protected void assertWhiteSpaceEqual(String a, String b) {
		String ashort = removeWhiteSpace(a);
		String bshort = removeWhiteSpace(b);
		Assert.assertEquals(b + "\n\nexpected:\n" + a, ashort, bshort);
	}

	private String removeWhiteSpace(String a) {
		return a.replaceAll("[\\ ,\\t,\\n,\\r]", "");
	}

	public void setUp() throws Exception {
		this.byteStream = new ByteArrayOutputStream();
		this.printStream = new PrintStream(byteStream);
	}

	protected String getOutput() {
		return byteStream.toString().trim();
	}
}