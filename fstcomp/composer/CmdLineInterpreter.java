package composer;


import java.io.File;

public class CmdLineInterpreter {
	
	public static final String INPUT_OPTION_EQUATIONFILE = "--expression";

	public static final String INPUT_OPTION_AHEAD_EQUATION_FILE = "--ahead";

	public static final String INPUT_OPTION_BASE_DIRECTORY = "--base-directory";

	public static final String INPUT_OPTION_SHOW_GUI = "--gui";
	
	public static final String INPUT_OPTION_SHOW_XML = "--xml";
	
	public static final String INPUT_OPTION_SHOW_SUM = "--sum";

	public static final String INPUT_OPTION_SHOW_FST = "--fst";

	public static final String INPUT_OPTION_FILE_OUTPUT = "--write";

	public static final String INPUT_OPTION_OUTPUT_DIRECTORY = "--output-directory";

	public static final String INPUT_OPTION_RESOLVE_REFERENCES = "--resolve-references";

	public static final String INPUT_OPTION_HELP = "--help";

	public static final String INPUT_OPTION_COUNT = "--count";

	public boolean isCount = false;
	
	public boolean isAheadEquationFile;

	public String equationFileName;

	public String equationBaseDirectoryName;

	public String outputDirectoryName=null;
	
	public boolean isBaseDirectoryName = false;
	
	public boolean showXML = false;
	
	public boolean showFST = false;
	
	public boolean showSum = false;

	public boolean showGui = false;

	public boolean fileOutput = false;
	
	
	public void parseCmdLineArguments(String[] args) {
		boolean errorOccured = false;
		if (args != null && args.length > 0) {
			for (int i = 0; i < args.length; i++) {
				if (args[i].equals(INPUT_OPTION_EQUATIONFILE)) {
					i++;
					if (i < args.length) {
						equationFileName = args[i];
						if(!isBaseDirectoryName)
							equationBaseDirectoryName = getDirectoryName(new File(equationFileName)) + File.separator;
						equationFileName = equationFileName.replace("\\", File.separator);
						equationFileName = equationFileName.replace("/", File.separator);
					} else {
						System.out.println("Error occured option: " + INPUT_OPTION_EQUATIONFILE);
						errorOccured = true;
					}
				} else if (args[i].equals(INPUT_OPTION_BASE_DIRECTORY)) {
					i++;
					if (i < args.length) {
						equationBaseDirectoryName = args[i];
						equationBaseDirectoryName = equationBaseDirectoryName.replace("\\", File.separator);
						equationBaseDirectoryName = equationBaseDirectoryName.replace("/", File.separator);
						isBaseDirectoryName = true;
					} else {
						System.out.println("Error occured option: " + INPUT_OPTION_BASE_DIRECTORY);
						errorOccured = true;
					}
				} else if (args[i].equals(INPUT_OPTION_OUTPUT_DIRECTORY)) {
					i++;
					if (i < args.length) {
						outputDirectoryName = args[i];
					} else {
						System.out.println("Error occured option: " + INPUT_OPTION_OUTPUT_DIRECTORY);
						errorOccured = true;
					}
				} else if (args[i].equals(INPUT_OPTION_COUNT)) {
					isCount = true;
				} else if (args[i].equals(INPUT_OPTION_FILE_OUTPUT)) {
					fileOutput = true;
				} else if (args[i].equals(INPUT_OPTION_SHOW_GUI)) {
					showGui = true;
				} else if (args[i].equals(INPUT_OPTION_SHOW_XML)) {
					showXML = true;
				} else if (args[i].equals(INPUT_OPTION_SHOW_FST)) {
					showFST = true;
				} else if (args[i].equals(INPUT_OPTION_SHOW_SUM)) {
					showSum = true;
				} else if (args[i].equals(INPUT_OPTION_AHEAD_EQUATION_FILE)) {
					isAheadEquationFile = true;
				} else if (args[i].equals(INPUT_OPTION_RESOLVE_REFERENCES)) {
					System.out.println("The option '" + INPUT_OPTION_RESOLVE_REFERENCES + "' is obsolete.");
				} else if (args[i].equals(INPUT_OPTION_HELP)) {
					printHelp(false);
				} else {
					errorOccured = true;
				}
			}
		} else {
			errorOccured = true;
		}
		if (errorOccured) {
			printHelp(errorOccured);
		}
	}
	
	private static void printHelp(boolean errorOccured) {
		if (errorOccured) {
			System.out.println("Incorrect command line parameters!");
		}
		System.out.println("Use `java -jar FeatureHouse.jar " 
				+ INPUT_OPTION_EQUATIONFILE + " <file name> ["
				+ INPUT_OPTION_BASE_DIRECTORY + " <directory name>]'"
);
		System.out.println("The option `" + INPUT_OPTION_EQUATIONFILE
				+ "' defines the name of the file that lists the input features/components.");
		System.out.println("The option `" + INPUT_OPTION_BASE_DIRECTORY
				+ "' defines the working directory, which is the search path for the input features/components.");
	}
	
	private static String getDirectoryName(File file) {
		String result = "";
		if (file.isDirectory()) {
			result = file.getPath();
		} else {
			result = file.getAbsoluteFile().getParentFile().getPath();
		}
		return result;
	}
}
