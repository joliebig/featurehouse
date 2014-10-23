package fstcomp;

import static fstcomp.ComposerTestUtil.compose;
import static fstcomp.ComposerTestUtil.deleteDirWithContents;
import static fstcomp.ComposerTestUtil.setupProductLineFeatures;
import static org.junit.Assert.assertEquals;
import fstcomp.ComposerTestUtil.Feature;
import integrationtests.Checksum;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

public class JavaCompositionTest {

	@Test
	public void testDefaultComposition() {
		try {
		String expression = "test/fstcomp/Java/GPL/GPLComp.features";
		String outputDir = "result/fstcomp/output/Java_GPL_GPLComp__Default";
		
		compose(expression, outputDir, null, null);
		String actualChecksum = Checksum.calculateChecksum(new File(outputDir),new FilenameFilter() {
			@Override
			public boolean accept(File dir, String name) {
				return (name.endsWith(".java") || name.endsWith(".aj"));
			}
		});
		assertEquals("D751713988987E9331980363E24189CE", actualChecksum);
		} catch (Throwable t) {
			System.err.println(t.getMessage());
			throw t;
		}
	}
	
	@Test
	public void testAbstractKeywordComposition() throws IOException {
		try {
		// setup this minimal project in a tmp directory
		List<Feature> features = new ArrayList<Feature>();
		Feature a = new Feature("A"); features.add(a);
		a.addFile("FileC.java", "public abstract class FileC {public abstract void m();}");
		Feature b = new Feature("B"); features.add(b);
		a.addFile("FileC.java", "public abstract class FileC {public abstract void m();}");
		File mainDir = setupProductLineFeatures(features);
		File outputDir = new File(mainDir, "output");
		// compose with FH
		compose(new File(mainDir, "features.exp").getAbsolutePath(), outputDir.getAbsolutePath(), new File(mainDir, "features").getAbsolutePath(), null);
		// verify checksum of result
		String actualChecksum = Checksum.calculateChecksum(outputDir,new FilenameFilter() {
			@Override
			public boolean accept(File dir, String name) {
				return (name.endsWith(".java") || name.endsWith(".aj"));
			}
		});
		assertEquals("Checksum did not match. Generated files in " + mainDir.getAbsolutePath(),
				"D751713988987E9331980363E24189CE", actualChecksum);
		// if we arrive at this point, we can delete the tmp dir
		deleteDirWithContents(mainDir);
		} catch (Throwable t) {
			System.err.println(t.getMessage());
			throw t;
		}
	}

}
