package fstcomp;

import static fstcomp.ComposerTestUtil.compose;
import static org.junit.Assert.assertEquals;
import integrationtests.Checksum;

import java.io.File;
import java.io.FilenameFilter;

import org.junit.Test;

public class JavaFeatureAnnotationTest {
	@Test
	public void testFeatureAnnotatedComposition() {
		String expression = "test/fstcomp/Java/GPL/GPLComp.features";
		String outputDir = "result/fstcomp/output/Java_GPL_GPLComp__FeaAnn";
		
		compose(expression, outputDir, null, new String[] {"--featureAnnotationJava"});
		String actualChecksum = Checksum.calculateChecksum(new File(outputDir),new FilenameFilter() {
			@Override
			public boolean accept(File dir, String name) {
				return (name.endsWith(".java") || name.endsWith(".aj"));
			}
		});
		assertEquals("D751713988987E9331980363E24189CE", actualChecksum);
	}
}
