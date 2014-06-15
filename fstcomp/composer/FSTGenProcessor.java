package composer;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedList;

import modification.traversalLanguageParser.addressManagement.DuplicateFreeLinkedList;
import printer.FeaturePrintVisitor;
import printer.PrintVisitorInterface;
import printer.alloy.AlloyPrintVisitor;
import printer.asmetal.AsmetaLPrintVisitor;
import printer.binary.BinaryPrintVisitor;
import printer.capprox.CApproxHeaderPrintVisitor;
import printer.capprox.CApproxPrintVisitor;
import printer.csharp.CSharpPrintVisitor;
import printer.fj.FJPrintVisitor;
import printer.haskell.HaskellPrintVisitor;
import printer.java.JavaPrintVisitor;
import printer.javacc.JavaCCPrintVisitor;
import printer.jcop.JCopPrintVisitor;
import printer.sdf.SDFPrintVisitor;
import printer.str.STRPrintVisitor;
import printer.text.TextPrintVisitor;
import printer.xmi.XMIPrintVisitor;
import processor.capprox.CIncludeGuardGenerator;
import builder.ArtifactBuilderInterface;
import builder.alloy.AlloyBuilder;
import builder.asmetal.AsmetaLBuilder; 
import builder.binary.BinaryBuilder;
import builder.capprox.CApproxBuilder;
import builder.csharp.CSharpBuilder;
import builder.fj.FJBuilder;
import builder.haskell.HaskellBuilder;
import builder.java.JavaBuilder;
import builder.javacc.JavaCCBuilder;
import builder.jcop.JCopBuilder;
import builder.sdf.SDFBuilder;
import builder.str.STRBuilder;
import builder.text.TextBuilder;
import builder.xmi.XMIBuilder;
import de.ovgu.cide.fstgen.ast.FSTNode;
import de.ovgu.cide.fstgen.ast.FSTVisitor;

public class FSTGenProcessor {

	protected FileLoader fileLoader = new FileLoader(this);
	protected FeaturePrintVisitor featureVisitor = new FeaturePrintVisitor();
	/**
	 * to collect all files in which an error occurred
	 */
	protected DuplicateFreeLinkedList<File> errorFiles;
	private ArrayList<FSTNode> fstnodes;

	public FSTGenProcessor() {
		registerArtifactBuilder(new FJBuilder());
		registerArtifactBuilder(new AlloyBuilder());
		registerArtifactBuilder(new JavaBuilder());
		registerArtifactBuilder(new CSharpBuilder());
		registerArtifactBuilder(new CApproxBuilder());
		registerArtifactBuilder(new AsmetaLBuilder());		
		registerArtifactBuilder(new HaskellBuilder());
		registerArtifactBuilder(new JavaCCBuilder());
		registerArtifactBuilder(new JCopBuilder());
		registerArtifactBuilder(new XMIBuilder());
		registerArtifactBuilder(new SDFBuilder());
		registerArtifactBuilder(new STRBuilder());
		registerArtifactBuilder(new TextBuilder(".properties"));
		registerArtifactBuilder(new TextBuilder(".txt"));
		registerArtifactBuilder(new BinaryBuilder(".jpg"));
		registerArtifactBuilder(new BinaryBuilder(".gif"));
		registerArtifactBuilder(new BinaryBuilder(".png"));
		registerArtifactBuilder(new BinaryBuilder(".wav"));
		registerArtifactBuilder(new BinaryBuilder(".aj"));
		registerPrintVisitor(new FJPrintVisitor());
		registerPrintVisitor(new AlloyPrintVisitor());
		registerPrintVisitor(new JavaPrintVisitor());
		registerPrintVisitor(new JCopPrintVisitor());
		registerPrintVisitor(new CSharpPrintVisitor());
		registerPrintVisitor(new CApproxPrintVisitor());
		registerPrintVisitor(new CApproxHeaderPrintVisitor());
		registerPrintVisitor(new JavaCCPrintVisitor());
		registerPrintVisitor(new AsmetaLPrintVisitor());
		registerPrintVisitor(new HaskellPrintVisitor());
		registerPrintVisitor(new XMIPrintVisitor());
		registerPrintVisitor(new SDFPrintVisitor());
		registerPrintVisitor(new STRPrintVisitor());
		registerPrintVisitor(new TextPrintVisitor(".properties"));
		registerPrintVisitor(new TextPrintVisitor(".txt"));
		registerPrintVisitor(new BinaryPrintVisitor(".jpg"));
		registerPrintVisitor(new BinaryPrintVisitor(".gif"));
		registerPrintVisitor(new BinaryPrintVisitor(".png"));
		registerPrintVisitor(new BinaryPrintVisitor(".wav"));
		registerPrintVisitor(new BinaryPrintVisitor(".aj"));

		errorFiles = new DuplicateFreeLinkedList<File>();
		
		registerFSTVisitor(new CIncludeGuardGenerator());
	}
	
	public void setFstnodes(ArrayList<FSTNode> fstnodes) {
		this.fstnodes = fstnodes;
	}

	public ArrayList<FSTNode> getFstnodes() {
		return fstnodes;
	}

	public DuplicateFreeLinkedList<File> getErrorFiles() {
		return errorFiles;
	}

	public void registerArtifactBuilder(ArtifactBuilderInterface builder) {
		fileLoader.registerArtifactBuilder(builder);
	}

	public void unregisterArtifactBuilder(ArtifactBuilderInterface builder) {
		fileLoader.unregisterArtifactBuilder(builder);
	}

	public LinkedList<ArtifactBuilderInterface> getArtifactBuilders() {
		return fileLoader.getArtifactBuilders();
	}
	

	public void registerPrintVisitor(PrintVisitorInterface visitor) {
		this.featureVisitor.registerPrintVisitor(visitor);
	}

	public void unregisterPrintVisitor(PrintVisitorInterface visitor) {
		this.featureVisitor.unregisterPrintVisitor(visitor);
	}

	public LinkedList<PrintVisitorInterface> getPrintVisitors() {
		return featureVisitor.getPrintVisitors();
	}

	private LinkedList<IParseErrorListener> parseErrorListeners = new LinkedList<IParseErrorListener>();

	public void addParseErrorListener(IParseErrorListener listener) {
		if (!parseErrorListeners.contains(listener))
			parseErrorListeners.add(listener);
	}

	public void removeParseErrorListener(IParseErrorListener listener) {
		parseErrorListeners.remove(listener);
	}

	public void fireParseErrorOccured(cide.gparser.ParseException e1) {
		for (IParseErrorListener listener : parseErrorListeners)
			listener.parseErrorOccured(e1);
		e1.printStackTrace();
	}
	
	private LinkedList<ICompositionErrorListener> compositionErrorListeners = new LinkedList<ICompositionErrorListener>();

	public void addCompositionErrorListener(ICompositionErrorListener listener) {
		if (!compositionErrorListeners.contains(listener)) {
			compositionErrorListeners.add(listener);
		}
	}

	public void removeCompositionErrorListener(ICompositionErrorListener listener) {
		compositionErrorListeners.remove(listener);
	}

	public void fireCompositionErrorOccured(CompositionException e) {
		for (ICompositionErrorListener listener : compositionErrorListeners) {
			listener.parseErrorOccured(e);
		}
		e.printStackTrace();
	}
	
	public void registerFSTVisitor(FSTVisitor visitor) {
	    fstVisitors.add(visitor);
	}

	public void unregisterFSTVisitor(FSTVisitor visitor) {
	    fstVisitors.remove(visitor);
	}

	public LinkedList<FSTVisitor> getFSTVisitors() {
	    return fstVisitors;
	}
	
	private LinkedList<FSTVisitor> fstVisitors = new LinkedList<FSTVisitor>();

}
