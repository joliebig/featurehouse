package fstcomp;

import static fstcomp.ComposerTestUtil.compose;
import static org.junit.Assert.assertEquals;
import integrationtests.Checksum;

import java.io.File;

import org.junit.Test;

public class JavaFeatureAnnotationTest {
	@Test
	public void testFeatureAnnotatedComposition() {
		String expression = "test/fstcomp/Java/GPL/GPLComp.features";
		String outputDir = "result/fstcomp/output/Java_GPL_GPLComp__FeaAnn";
		
		compose(expression, outputDir, null, new String[] {"--featureAnnotationJava"});
		
		assertEquals("35D9C6CEC4F96A6EBFAA0599A813B5CB", Checksum.calculateChecksum(new File(outputDir), null));
	}
}
