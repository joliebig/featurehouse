package fstcomp;

import static fstcomp.ComposerTestUtil.compose;
import static org.junit.Assert.assertEquals;
import integrationtests.Checksum;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;

import org.junit.Ignore;
import org.junit.Test;

public class JavaFeatureAnnotationVarEncTest {

	@Test
	public void testFeatureAnnotatedVarEncComposition() throws IOException {
		BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter("test/fstcomp/Java/GPL/model.cnf"));
		bufferedWriter.write("(Base)");
		bufferedWriter.close();

		String expression = "test/fstcomp/Java/GPL/GPLComp.features";
		String outputDir = "result/fstcomp/output/Java_GPL_GPLComp__FeaAnnVarEnc";
		
		compose(expression, outputDir, null, new String[] {"--liftJava", "--featureAnnotationJava"});
		String expectedChecksum = "96F9FB420E1CDB6C609BBD32D3B340EF";
		String actualChecksum = Checksum.calculateChecksum(new File(outputDir),new FilenameFilter() {
			@Override
			public boolean accept(File dir, String name) {
				return (name.endsWith(".java") || name.endsWith(".aj"));
			}
		});
		assertEquals("actual checksum " + actualChecksum + " did not match expected Checksum " + expectedChecksum, 
				expectedChecksum, actualChecksum);
	}
}
