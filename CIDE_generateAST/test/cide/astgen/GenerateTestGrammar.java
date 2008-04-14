package cide.astgen;

import java.io.File;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class GenerateTestGrammar {

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void RunASTGen() throws Exception {
		Main main = new Main("TestGrammar");
		System.out.println(new File("test/").getAbsolutePath());
		main.runGenerator("test/TestGrammar.gcide", new File("test/generated"),"generated");
	}

}
