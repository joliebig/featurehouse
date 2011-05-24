package cide.astgen;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import cide.astgen.nparser.ast.ListAnnotationTest;
import cide.astgen.nparser.ast.NAbstractValueTest;
import cide.astgen.nparser.visitor.CreateSimplePrintVisitorVisitorTest;
import cide.astgen.nparser.visitor.ReferenceTest;

@RunWith(Suite.class)
@Suite.SuiteClasses( { NAbstractValueTest.class,
		CreateSimplePrintVisitorVisitorTest.class, ListAnnotationTest.class,
		ReferenceTest.class })
public class GeneratorTests {
}
