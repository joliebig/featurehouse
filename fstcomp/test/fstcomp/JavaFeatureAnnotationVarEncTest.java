package fstcomp;

import static fstcomp.ComposerTestUtil.compose;
import static org.junit.Assert.assertEquals;
import integrationtests.Checksum;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
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
		assertEquals("26FAA65CEA5763CF61275D0A8CC3C31C", Checksum.calculateChecksum(new File(outputDir)));
	}
}
