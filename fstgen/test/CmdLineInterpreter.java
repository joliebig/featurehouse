

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
							equationBaseDirectoryName = getDirectoryName(new File(equationFileName)) + "/";
					} else {
						System.out.println("Error occured option: " + INPUT_OPTION_EQUATIONFILE);
						errorOccured = true;
					}
				} else if (args[i].equals(INPUT_OPTION_BASE_DIRECTORY)) {
					i++;
					if (i < args.length) {
						equationBaseDirectoryName = args[i];
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
			System.out.println("Ein Fehler ist aufgetreten!");
		}
		System.out.println("Composer " 
				+ INPUT_OPTION_EQUATIONFILE + " filename ["
				+ INPUT_OPTION_AHEAD_EQUATION_FILE + "] ["
				+ INPUT_OPTION_BASE_DIRECTORY + " directory name] ["
				+ INPUT_OPTION_SHOW_FST + "] [" 
				+ INPUT_OPTION_SHOW_XML + "] [" 
				+ INPUT_OPTION_SHOW_SUM + "] [" 
				+ INPUT_OPTION_SHOW_GUI + "] [" 
				+ INPUT_OPTION_FILE_OUTPUT + "]");
		System.out.println(INPUT_OPTION_EQUATIONFILE
				+ " name of the file that lists the input features/components");
		System.out.println(INPUT_OPTION_AHEAD_EQUATION_FILE 
				+ " defines that the composer should run in AHEAD/Jak mode");
		System.out.println(INPUT_OPTION_BASE_DIRECTORY
				+ " defines the working directory, which is the search path for the input features/components");
		System.out.println(INPUT_OPTION_SHOW_XML
				+ " defines that an XML representation of the input components are written to the standard output");
		System.out.println(INPUT_OPTION_SHOW_GUI
				+ " defines that the input components are displayed in a GUI");
		System.out.println(INPUT_OPTION_SHOW_FST
				+ " defines that the FST of the composed program is written to the standard output");
		System.out.println(INPUT_OPTION_SHOW_SUM
				+ " defines that the algebraic expression of the composed program is written to the standard output");
		System.out.println(INPUT_OPTION_FILE_OUTPUT
				+ " advises the composer to write the composed program to the file system");
		System.out.println(INPUT_OPTION_OUTPUT_DIRECTORY
				+ " defines the directory where the composed program is written (requires "+INPUT_OPTION_FILE_OUTPUT+")");
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
