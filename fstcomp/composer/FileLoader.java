package composer;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLStreamException;

import modification.ModificationComposition;
import modification.content.UnknownContentTypeParseException;
import modification.content.UnknownFileTypeParseException;
import modification.xmlParser.XmlParser;

import org.xml.sax.SAXException;

import builder.ArtifactBuilderInterface;
import cide.gparser.ParseException;
import cide.gparser.TokenMgrError;
import de.ovgu.cide.fstgen.ast.FSTFeatureNode;

public class FileLoader {

	private static final String MODIFICATION_FOLDER_TAG = "_mod";

	private static final String MODIFICATION_CONTROL_FILE_TAG = "mod.xml";

	private LinkedList<ArtifactBuilderInterface> builderList;

	private MyFileFilter fileFilter = new MyFileFilter();

	private DirectoryFileFilter directoryFileFilter = new DirectoryFileFilter();

	private FSTGenProcessor composer;

	private ModificationComposition modcomposition = new ModificationComposition();

	private boolean preprocessFiles = false;

	public boolean isPreprocessFiles() {
		return preprocessFiles;
	}

	public void setPreprocessFiles(boolean preprocessFiles) {
		this.preprocessFiles = preprocessFiles;
	}

	public FileLoader(FSTGenProcessor genComposer) {
		builderList = new LinkedList<ArtifactBuilderInterface>();
		composer = genComposer;
	}

	public void registerArtifactBuilder(ArtifactBuilderInterface builder) {
		builderList.add(builder);
	}

	public void unregisterArtifactBuilder(ArtifactBuilderInterface builder) {
		builderList.remove(builder);
	}

	public LinkedList<ArtifactBuilderInterface> getArtifactBuilders() {
		return builderList;
	}

	public void loadFiles(String equationFileName,
			String equationBaseDirectoryName, boolean aheadEquation)
			throws FileNotFoundException, ParseException {
		parseEquationFile(equationFileName, equationBaseDirectoryName,
				aheadEquation);
	}

	public void loadFiles(String equationFileName,
			String equationBaseDirectoryName, boolean aheadEquation,
			String[] features) throws FileNotFoundException, ParseException {
		parseEquationFile(equationFileName, equationBaseDirectoryName,
				aheadEquation, features);
	}

	private void parseEquationFile(String equationFileName,
			String equationBaseDirectoryName, boolean aheadEquation)
			throws FileNotFoundException, ParseException {
		parseEquationFile(equationFileName, equationBaseDirectoryName,
				aheadEquation, null);
	}

	private void parseEquationFile(String equationFileName,
			String equationBaseDirectoryName, boolean aheadEquation,
			String[] features) throws FileNotFoundException, ParseException {
		if (equationFileName == null || equationFileName.length() == 0)
			throw new FileNotFoundException();
		File equationFile = new File(equationFileName);

		String equationFileContent = "";
		BufferedReader fileReader = null;
		try {
			fileReader = new BufferedReader(new FileReader(equationFile));
			String line = fileReader.readLine();
			while (line != null) {
				if (!line.startsWith("#")) {
					equationFileContent += line + " ";
				}
				line = fileReader.readLine();
			}
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			if (fileReader != null) {
				try {
					fileReader.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		if (!equationFileContent.equals("")) {
			if (features == null) {
				features = equationFileContent.split("\\s");
			}
			FSTGenComposer.outStream.println("Found the following features in expression file:");
			for (String s : features)
				FSTGenComposer.outStream.println(s);

			// if (features.length == 0) {
			// features = equationFileContent.split(" ");
			// }
			// System.out.println("BaseDirectory: " + baseDirectoryName);
			Iterator<ArtifactBuilderInterface> iterator = builderList
					.iterator();
			if (!equationBaseDirectoryName.trim().endsWith(
					"" + File.separatorChar)) {
				equationBaseDirectoryName = equationBaseDirectoryName.trim()
						+ File.separatorChar;
			}
			while (iterator.hasNext()) {
				iterator.next().setBaseDirectoryName(
						getDirectoryName(new File(equationBaseDirectoryName)));
			}
			for (int i = 0; i < features.length; i++) {
				if (features[i].trim().length() > 0) {
					File feature = new File(equationBaseDirectoryName
							+ features[i]);
					if (!feature.exists()) {
						FSTGenComposer.outStream.println("Did not find feature directory " + feature.getAbsolutePath() + "; treating it as empty feature.");
					}
					// Initialize each ArtifactBuilder with the current feature features[i].
					// If we do not initialize it FSTGenMerger fails, because a feature might
					// be empty and does not contain any code artifacts for merging.
					Iterator<ArtifactBuilderInterface> biter = builderList.iterator();
					
					while (biter.hasNext()) {
						biter.next().addFeature(new FSTFeatureNode(features[i]));
					}

					parseDirectory(feature, !aheadEquation);
				}
			}
		}
	}

	/**
	 * 
	 * @param directory -- name of the directory we are currently looking at
	 * @param recursive ??
	 * @throws FileNotFoundException
	 * @throws ParseException
	 */
	void parseDirectory(File directory, boolean recursive)
			throws FileNotFoundException, ParseException {
		if (directory.getName().equals(MODIFICATION_FOLDER_TAG)) {
			// TODO _mod folder should only be allowed as direct subfolder of
			// feature folder

			XmlParser xmlpars;
			try {
				xmlpars = new XmlParser(new File(directory.getPath()
						+ File.separator + MODIFICATION_CONTROL_FILE_TAG));
				modcomposition.addAll(xmlpars.parse());
			} catch (XMLStreamException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ParserConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (SAXException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (UnknownFileTypeParseException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (UnknownContentTypeParseException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		} else if (recursive) {

			File[] files = directory.listFiles(fileFilter);
			if (files != null) {

				for (int i = 0; i < files.length; i++) {
					Iterator<ArtifactBuilderInterface> iterator = builderList
							.iterator();
					while (iterator.hasNext()) {
						ArtifactBuilderInterface builder = iterator.next();
						if (builder.acceptFile(files[i])) {
							try {
								if (isPreprocessFiles())
									builder.setPreprocessNode(true);
								builder.processFile(files[i]);
							} catch (ParseException e) {
								composer.getErrorFiles().add(files[i]);
								composer.fireParseErrorOccured(e);
							} catch (TokenMgrError e) {
								composer.getErrorFiles().add(files[i]);
								throw (e);
							}
						}
					}
				}
			}
			File[] directories = directory.listFiles(directoryFileFilter);
			if (directories != null) {
				for (int i = 0; i < directories.length; i++) {
					parseDirectory(directories[i], recursive);
				}
			}
		} else {
			File[] files = directory.listFiles(fileFilter);
			if (files != null) {
				for (int i = 0; i < files.length; i++) {
					Iterator<ArtifactBuilderInterface> iterator = builderList
							.iterator();
					while (iterator.hasNext()) {
						ArtifactBuilderInterface builder = iterator.next();
						if (builder.acceptFile(files[i])) {
							try {
								builder.processFile(files[i]);
							} catch (ParseException e) {
								composer.getErrorFiles().add(files[i]);
								composer.fireParseErrorOccured(e);
								e.printStackTrace();
							}
						}
					}
				}
			} else {
				System.out
						.println("Input directory does not contain any parsable files: "
								+ directory.getPath());
			}

		}
	}

	private String getDirectoryName(File file) {
		String result = "";
		if (file.isDirectory()) {
			result = file.getPath();
		} else {
			result = file.getParentFile().getPath();
		}
		return result;
	}

	private class DirectoryFileFilter implements FileFilter {
		public DirectoryFileFilter() {

		}

		public boolean accept(File pathname) {
			if (pathname.isFile()) {
				return false;
			} else if (pathname.isDirectory()) {
				// ignore .svn directories
				return !pathname.getName().equals(".svn");
			}
			return false;
		}
	}

	private class MyFileFilter implements FileFilter {

		public MyFileFilter() {

		}

		public boolean accept(File pathname) {
			if (pathname.isFile()) {
				return true;
			}
			return false;
		}

	}

	public ModificationComposition getModifications() {
		return modcomposition;
	}
}
