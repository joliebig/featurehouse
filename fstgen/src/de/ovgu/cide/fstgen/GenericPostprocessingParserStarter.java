package de.ovgu.cide.fstgen;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import cide.gparser.CharStream;
import cide.gparser.OffsetCharStream;
import de.ovgu.cide.fstgen.ast.AbstractFSTParser;
import de.ovgu.cide.fstgen.ast.AbstractFSTPrintVisitor;
import de.ovgu.cide.fstgen.fstProcessing.*;

public class GenericPostprocessingParserStarter {

	/**
	 * @param args
	 *            parserClass, mainProduction, targetFile
	 * @throws ClassNotFoundException
	 * @throws IllegalAccessException
	 * @throws InstantiationException
	 * @throws NoSuchMethodException
	 * @throws InvocationTargetException
	 * @throws SecurityException
	 * @throws IllegalArgumentException
	 * @throws FileNotFoundException
	 */
	public static void main(String[] args) throws ClassNotFoundException,
			InstantiationException, IllegalAccessException,
			IllegalArgumentException, SecurityException,
			InvocationTargetException, NoSuchMethodException,
			FileNotFoundException {
		String parserClassName = args[0];
		String mainProduction = args[1];
		String targetFileName = args[2];
		String fstProcessorClassName = args[3];
//		if (args.length>3)

//		System.out.println(args[0]);
//		System.out.println(args[1]);
//		System.out.println(args[2]);

		Class.forName("de.ovgu.cide.fstgen.ast.AbstractFSTParser");

		File inputFile = new File(targetFileName);
		if (!inputFile.exists())
			throw new FileNotFoundException(targetFileName);
		OffsetCharStream input = new OffsetCharStream(new FileInputStream(
				inputFile));

		// parse
		Class<?> parserClass = Class.forName(parserClassName);
		Constructor<?> parserConstructor = parserClass
				.getConstructor(CharStream.class);
		AbstractFSTParser parser = (AbstractFSTParser) parserConstructor
				.newInstance(input);
		parserClass.getMethod(mainProduction,boolean.class)
				.invoke(parser, new Boolean(false));

		System.out.println("********** before FST processing **************");
		System.out.println(parser.getRoot().printFST(0));
		
		// invoke FST processor
		Class<?> fstProcessorClass = Class.forName(fstProcessorClassName);
		FSTprocessor fstProcessor = ((FSTprocessor) fstProcessorClass.newInstance());
		fstProcessor.processFST(AbstractFSTParser.fstnodes);
		
		System.out.println("********** after FST processing **************");
		System.out.println(parser.getRoot().printFST(0));
		
		// reconstruct FST after processing
		fstProcessor.reconstructFST(AbstractFSTParser.fstnodes);
		
		System.out.println("********** after FST reconstruction **************");
		System.out.println(parser.getRoot().printFST(0));
		
		// pretty print
		System.out.println("********** pretty printed **************");
		String pkg = parserClassName.substring(0,parserClassName.lastIndexOf("."));
		Class<?> printerClass=Class.forName(pkg+".SimplePrintVisitor");
		AbstractFSTPrintVisitor printer=(AbstractFSTPrintVisitor) printerClass.newInstance();
		parser.getRoot().accept(printer);
		System.out.println(printer.getResult());
	}

}
