package fstcomp;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import composer.FSTGenComposer;


public class ComposerTestUtil {

	public static void compose(String expression, String outputDirectory, String baseDirectory, String[] extraArgs) {

		new File(outputDirectory).mkdir();

		if (baseDirectory == null) {
			File expressionFile = new File(expression);
			baseDirectory = expressionFile.getParent();
		}

		List<String> arguments = new ArrayList<String>();

		arguments.add("--base-directory");
		arguments.add(baseDirectory);
		arguments.add("--expression");
		arguments.add(expression);
		arguments.add("--output-directory");
		arguments.add(outputDirectory);

		if (extraArgs != null) {
			arguments.addAll(Arrays.asList(extraArgs));
		}

		//print arguments to stdout so they appear in the test output
		//System.out.print("FSTComposer Arguments:");
		//System.out.println(Arrays.toString(arguments.toArray()));

		metadata.CompositionMetadataStore.reinitialize();
		PrintStream ps = new PrintStream(new OutputStream() {
			@Override
			public void write(int b) throws IOException {/* ignore all output */}
		});
		FSTGenComposer.composeWithPrintStream(arguments.toArray(new String[0]), ps);
		ps.close();
	}

	/**
	 * Sets up a temporary directory which contains the parameter features in a standard FeatureHouse project setup.
	 * The returned directory contains a directory "features" which should be passed as --baseDirectory and a file "features.exp" which should be passed as --expression.
	 */
	public static File setupProductLineFeatures(List<ComposerTestUtil.Feature> features) throws IOException {
		File tmpDir = createTempDirectory();
		File featuresDir = new File(tmpDir, "features");
		featuresDir.mkdirs();
		for (Feature f : features) {
			File fdir = new File (featuresDir, f.name);
			fdir.mkdirs();
			for (int i =0; i < f.filenames.size(); i++) {
				File contentFile = new File(fdir, f.filenames.get(i));
				contentFile.getParentFile().mkdirs();
				try(FileWriter fw = new FileWriter(contentFile)) {
					fw.write(f.filecontents.get(i));
				}
			}
		}
		try(FileWriter fw = new FileWriter(new File(tmpDir,"features.exp"))) {
			for (Feature f : features) {
				fw.write(f.name + "\n");
			}
		}
		return tmpDir;
	}
	static class Feature {
		String name;
		List<String> filenames = new ArrayList<String>();
		List<String> filecontents = new ArrayList<String>();
		public Feature(String name) {
			super();
			this.name = name;
		}
		public void addFile(String name, String content) {
			filenames.add(name);
			filecontents.add(content);
			assert filecontents.size()==filenames.size();
		}
	}
	private static File createTempDirectory() {
		long startTime = System.currentTimeMillis();
		File systemTmpDir = new File(System.getProperty("java.io.tmpdir"));
		for (int suffixCounter = 0; suffixCounter < 2000; suffixCounter++) {
			File tempDir = new File(systemTmpDir,
					startTime + "_" + suffixCounter);
			if (tempDir.mkdir()) {
				return tempDir;
			}
		}
		throw new IllegalStateException("Failed to create directory within "
				+ 2000 + " attempts (tried "
				+ startTime + "_" + "0 to " + startTime + "_" + (2000 - 1) + ')');
	}
	/**
	 * Deletes any contents of the given directory if possible (might have problems if files are accessed concurrently).
	 */
	public static void deleteDirWithContents(File subject) throws IOException {
		if (!subject.exists()) return;
		if (!subject.isDirectory()) 
			throw new IllegalArgumentException("Can only delete Directory Contents: " + subject.getAbsolutePath());
		File[] elements = subject.listFiles();
		for (int i = 0; i < elements.length; i++) {
			File nSubject = elements[i];
			if (nSubject.isDirectory()) {
				deleteDirWithContents(nSubject);
				nSubject.delete();
			} else {
				nSubject.delete();
			}
		}
		subject.delete();
	}
}
