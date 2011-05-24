package cide.astgen.ant;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.javacc.parser.Main;

public class JavaCCTask extends Task {

	private String grammarFileName;
	private String targetDirectory;

	public void execute() throws BuildException {
		int result = 0;
		try {
			result = Main.mainProgram(new String[] {
					"-OUTPUT_DIRECTORY:" + targetDirectory, grammarFileName });
		} catch (Exception e) {
			e.printStackTrace();
			throw new BuildException(e);
		}
		if (result != 0)
			throw new BuildException("Build failed with errorcode " + result);
	}

	public void setGrammarFileName(String g) {
		this.grammarFileName = g;
	}

	public void setTargetDirectory(String td) {
		this.targetDirectory = td;
	}

}
