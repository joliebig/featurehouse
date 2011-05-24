package printer.binary;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import printer.ArtifactPrintVisitor;
import printer.PrintVisitorException;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTNonTerminal;
import de.ovgu.cide.fstgen.ast.FSTTerminal;

public class BinaryPrintVisitor extends ArtifactPrintVisitor {

	public BinaryPrintVisitor(String suffix) {
		super(suffix + "-File");
	}
	
	public void processNode(FSTNode node, File folderPath) throws PrintVisitorException {
		if(node instanceof FSTNonTerminal) {
			FSTNonTerminal nonterminal = (FSTNonTerminal)node;
			assert(nonterminal.getChildren().isEmpty());
			assert(!(nonterminal.getChildren().get(0) instanceof FSTTerminal));
			
			String originalPath = ((FSTTerminal)nonterminal.getChildren().get(0)).getBody();
			File dst = new File(folderPath, nonterminal.getName());
			File src = new File(originalPath);
			try {
				dst.createNewFile();
				InputStream in = new FileInputStream(src);
				OutputStream out = new FileOutputStream(dst);
				byte[] buf = new byte[1024];
				int len;
				while ((len = in.read(buf)) > 0) {
					out.write(buf, 0, len);
				}
				in.close();
				out.close();
			} catch (IOException e) {
				throw new PrintVisitorException(e.getMessage());
			}
		} else {
			assert(!(node instanceof FSTNonTerminal));
		}
	}
}
