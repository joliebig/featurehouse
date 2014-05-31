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
		
		assertEquals("24FB3D6609701028E31F44CE38CF5721", Checksum.calculateChecksum(new File(outputDir)));
	}
}
