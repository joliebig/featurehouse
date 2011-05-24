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

public class GenericParserStarter {

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

		Class<?> parserClass = Class.forName(parserClassName);
		Constructor<?> parserConstructor = parserClass
				.getConstructor(CharStream.class);
		AbstractFSTParser parser = (AbstractFSTParser) parserConstructor
				.newInstance(input);
		parserClass.getMethod(mainProduction,boolean.class)
				.invoke(parser, new Boolean(false));

		System.out.println(parser.getRoot().printFST(0));
		
		String pkg = parserClassName.substring(0,parserClassName.lastIndexOf("."));
		Class<?> printerClass=Class.forName(pkg+".SimplePrintVisitor");
		AbstractFSTPrintVisitor printer=(AbstractFSTPrintVisitor) printerClass.newInstance();
		parser.getRoot().accept(printer);
		System.out.println(printer.getResult());
	}

}
