package de.ovgu.cide.fstgen;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.junit.Before;
import org.junit.Test;

import cide.astgen.nparser.ast.NGrammar;
import cide.astgen.nparser.parser.ParseException;
import cide.astgen.nparser.parser.SlimJJParser;

public class ParserTest extends TestCase{

	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testParser() throws ParseException, FileNotFoundException {
		SlimJJParser parser = new SlimJJParser(new FileInputStream(
				"test/fj.gcide"));
		NGrammar grammar = parser.Grammar();
	}

	@Test
	public void testParseAnnotation() throws ParseException,
			FileNotFoundException {
		NGrammar g;
		g=parse("@A main:x;");
		Assert.assertEquals(1,g.findProduction("main").getChoices().get(0).getAnnotations().size());
		Assert.assertNotNull(g.findProduction("main").getChoices().get(0).findAnnotation("A"));
		Assert.assertEquals(0,g.findProduction("main").getChoices().get(0).findAnnotation("A").values.keySet().size());
		g=parse("@A() @B main:x;");
		Assert.assertEquals(2,g.findProduction("main").getChoices().get(0).getAnnotations().size());
		Assert.assertNotNull(g.findProduction("main").getChoices().get(0).findAnnotation("B"));
		g=parse("@A(a=1, b=2) main:x;");
		Assert.assertEquals(2,g.findProduction("main").getChoices().get(0).findAnnotation("A").values.keySet().size());
		Assert.assertEquals("1",g.findProduction("main").getChoices().get(0).findAnnotation("A").values.get("a"));
		g=parse("@A(a=1, b=\"x\") main:x;");
		Assert.assertEquals("1",g.findProduction("main").getChoices().get(0).findAnnotation("A").values.get("a"));
		Assert.assertEquals("\"x\"",g.findProduction("main").getChoices().get(0).findAnnotation("A").values.get("b"));
		g=parse("@A(1) main:x;");
		Assert.assertEquals(1,g.findProduction("main").getChoices().get(0).findAnnotation("A").values.keySet().size());
	}
	
	@Test
	public void testParserFST() throws ParseException, FileNotFoundException {
		SlimJJParser parser = new SlimJJParser(new FileInputStream(
				"test/fj_fst.gcide"));
		NGrammar grammar = parser.Grammar();
	}

	private NGrammar parse(String t) throws ParseException,
			FileNotFoundException {
		SlimJJParser parser = new SlimJJParser(new ByteArrayInputStream(
				("GRAMMARSTART " + t).getBytes()));
		return parser.Grammar();
	}

}
