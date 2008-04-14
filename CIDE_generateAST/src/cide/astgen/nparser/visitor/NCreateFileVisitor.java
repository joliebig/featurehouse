package cide.astgen.nparser.visitor;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;

public class NCreateFileVisitor extends NVisitor {

	protected String targetPackage;
	protected PrintStream out;

	public NCreateFileVisitor(File targetDir, String fileName,
			String targetPackage) throws FileNotFoundException {
		this(new PrintStream(new File(targetDir, fileName)), targetPackage);
	}

	public NCreateFileVisitor(PrintStream out, String targetPackage) {
		this.targetPackage = targetPackage;
		this.out = out;
	}

	private void print(String p) {
		out.print(p);
	}

	protected void println(String p, int indent) {
		for (int i = 0; i < indent; i++)
			print("\t");
		out.println(p);
	}

}