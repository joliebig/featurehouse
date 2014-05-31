package fstcomp;

import static fstcomp.ComposerTestUtil.compose;
import static org.junit.Assert.assertEquals;
import integrationtests.Checksum;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.junit.Test;

public class JavaVarEncTest {

	@Test
	public void testVarEncComposition() throws IOException {
		BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter("test/fstcomp/Java/GPL/model.cnf"));
		bufferedWriter.write("(Base)");
		bufferedWriter.close();
		
		String expression = "test/fstcomp/Java/GPL/GPLComp.features";
		String outputDir = "result/fstcomp/output/Java_GPL_GPLComp__VarEnc";
		
		compose(expression, outputDir, null, new String[] {"--liftJava"});
		
		assertEquals("F1E61C56834E9B0601E67A664BFAA0B6", Checksum.calculateChecksum(new File(outputDir)));
	}

}
