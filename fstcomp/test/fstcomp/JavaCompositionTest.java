package fstcomp;

import static org.junit.Assert.*;
import static fstcomp.ComposerTestUtil.*;

import java.io.File;
import integrationtests.Checksum;
import org.junit.Test;

public class JavaCompositionTest {

	@Test
	public void testDefaultComposition() {
		
		String expression = "test/fstcomp/Java/GPL/GPLComp.features";
		String outputDir = "result/fstcomp/output/Java_GPL_GPLComp__Default";
		
		compose(expression, outputDir, null, null);
		
		assertEquals("9F4BC38FA03480D2C1144C1260A04E2A", Checksum.calculateChecksum(new File(outputDir)));
	}

}