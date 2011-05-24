package cide.astgen.ant;

import java.io.File;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import cide.astgen.Main;

public class AstgenTask extends Task {

	private String grammarFileName;
	private String targetDirectory;
	private String targetPackage;

	public void execute() throws BuildException {
		Main m = new Main(grammarFileName.substring(0, grammarFileName
				.lastIndexOf('.')));
		try {
			m.runGenerator(grammarFileName, new File(targetDirectory),
					targetPackage);
		} catch (Throwable e) {
			e.printStackTrace();
			throw new BuildException(e);
		}
	}

	public void setGrammarFileName(String g) {
		this.grammarFileName = g;
	}

	public void setTargetDirectory(String td) {
		this.targetDirectory = td;
	}

	public void setTargetPackage(String tp) {
		this.targetPackage = tp;
	}

}
