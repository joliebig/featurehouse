package builder;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.StringTokenizer;

import cide.gparser.ParseException;
import de.ovgu.cide.fstgen.ast.AbstractFSTParser;
import de.ovgu.cide.fstgen.ast.FSTFeatureNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;

public abstract class ArtifactBuilder implements ArtifactBuilderInterface {

	private String baseDirectoryName;

	private LinkedList<FSTNonTerminal> featureNodes = new LinkedList<FSTNonTerminal>();

	private String[] suffixes;
	
	private boolean preprocessNode = false;

	public boolean isPreprocessNode() {
		return preprocessNode;
	}

	public void setPreprocessNode(boolean preprocessNode) {
		this.preprocessNode = preprocessNode;
	}

	public ArtifactBuilder(String suffix) {
		String[] suffixes = new String[1];
		suffixes[0] = suffix;
		this.suffixes = suffixes;
	}

	public ArtifactBuilder(String[] suffixes) {
		this.suffixes = suffixes;
	}

	protected String getSuffix() {
		if (getSuffixes().length > 0)
			return suffixes[0];
		else
			return "";
	}

	protected String[] getSuffixes() {
		return suffixes;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see builder.ArtifactBuilderInterface#getFeatures()
	 */
	public LinkedList<FSTNonTerminal> getFeatures() {
		return featureNodes;
	}

	public abstract void processNode(FSTNonTerminal parent, StringTokenizer st,
			File inputFile) throws FileNotFoundException, ParseException;

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * builder.ArtifactBuilderInterface#setBaseDirectoryName(java.lang.String)
	 */
	public void setBaseDirectoryName(String baseDirectoryName) {
		this.baseDirectoryName = baseDirectoryName;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see builder.ArtifactBuilderInterface#getBaseDirectoryName()
	 */
	public String getBaseDirectoryName() {
		return this.baseDirectoryName;
	}

	/**
	 * Return the Feature Tree corresponding to the given Name. Constructs a new
	 * Feature if none matches.
	 * 
	 * @param name
	 *            the name of a feature
	 * @return matching Feature Tree
	 */
	private FSTNonTerminal getFeatureNodeByName(String name) {
		for (FSTNonTerminal featureNode : featureNodes) {
			if (featureNode.getName().equals(name))
				return featureNode;
		}
		FSTNonTerminal newFeatureNode = new FSTFeatureNode(name);
		featureNodes.add(newFeatureNode);
		return newFeatureNode;
	}
	
	public void preprocessFile(File inputFile) throws FileNotFoundException { }

	/*
	 * (non-Javadoc)
	 * 
	 * @see builder.ArtifactBuilderInterface#processFile(java.io.File)
	 */
	public void processFile(File inputFile) throws FileNotFoundException,
			ParseException {
		System.out.println("processing: " + inputFile.getPath());
		FSTNonTerminal parent = null;
		StringTokenizer st = new StringTokenizer(inputFile.getPath(),
				File.separator);

		// Directory containing the Features

		int beginBaseDir = this.getBaseDirectoryName().lastIndexOf(
				File.separator) < 0 ? 0 : this.getBaseDirectoryName()
				.lastIndexOf(File.separator) + 1;
		int endBaseDir = this.getBaseDirectoryName().length();
		String baseDir = this.getBaseDirectoryName().substring(beginBaseDir,
				endBaseDir);

		String fileDir = st.nextToken();

		// forward through st until the current token is baseDir
		while (st.hasMoreTokens()) {
			if (baseDir.equals(fileDir))
				break;
			fileDir = st.nextToken();
		}

		if (baseDir.equals(fileDir)) {
			String featureName = st.nextToken(); // the name of the feature must
													// be the next token
			// get the correct Feature tree
			parent = getFeatureNodeByName(featureName);

			// memorize the feature to which the nodes belong
			AbstractFSTParser.fstnodes.add(new FSTNonTerminal("Feature", parent
					.getName()));
			// mark that the next file of FST begins
			AbstractFSTParser.fstnodes.add(new FSTNonTerminal("EOF Marker",
					inputFile.toString()));

			while (st.hasMoreTokens()) {
				if (st.countTokens() > 1) {
					// still folders between current position and file
					FSTNonTerminal newNode = new FSTNonTerminal("Folder", st
							.nextToken());
					FSTNonTerminal oldNode = (FSTNonTerminal) parent
							.getCompatibleChild(newNode);
					if (parent != null && oldNode == null) {
						parent.addChild(newNode);
						parent = newNode;
					} else {
						parent = oldNode;
					}

				} else {
					processNode(parent, st, inputFile);
				}
			}
		}
	}

	@Override
	public boolean acceptFile(File inputFile) {
		if (inputFile.isFile()) {
			for (String suffix : getSuffixes()) {
				if (inputFile.getName().endsWith(suffix)) {
					AbstractFSTParser.fstnodes.add(0, new FSTNonTerminal(
							"language", suffix));
					return true;
				}
			}
		}
		return false;
	}
}
