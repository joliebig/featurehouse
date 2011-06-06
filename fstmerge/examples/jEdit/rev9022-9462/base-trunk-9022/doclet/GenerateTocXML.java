

package doclet;

import com.sun.javadoc.*;
import com.sun.tools.doclets.standard.Standard;

import java.io.*;
import java.util.Arrays;

public class GenerateTocXML
{
	public static final String OUT = "toc.xml";
	public static final String HEADER = "<?xml version='1.0'?>\n<TOC>\n"
		+ "<ENTRY HREF='overview-summary.html'><TITLE>jEdit API Reference</TITLE>";
	public static final String FOOTER = "</ENTRY></TOC>\n";

	public static boolean start(RootDoc root)
	{
		if (!Standard.start(root))
		{
			return false;
		}
		try
		{
			FileWriter out = new FileWriter(Standard.htmlDoclet.configuration().destDirName + OUT);
			out.write(HEADER);

			PackageDoc[] packages = root.specifiedPackages();
			for(int i = 0; i < packages.length; ++i)
			{
				processPackage(out,packages[i]);
			}

			out.write(FOOTER);
			out.close();

			return true;
		}
		catch(IOException e)
		{
			e.printStackTrace();
			return false;
		}
	}

	public static int optionLength(String option)
	{
		return Standard.optionLength(option);
	}

	public static boolean validOptions(String[][] options, DocErrorReporter reporter)
	{
		return Standard.validOptions(options,reporter);
	}

	public static LanguageVersion languageVersion()
	{
		return Standard.languageVersion();
	}

	private static void processPackage(Writer out, PackageDoc pkg)
		throws IOException
	{
		out.write("<ENTRY HREF='");
		String pkgPath = pkg.name().replace('.','/') + "/";
		out.write(pkgPath);
		out.write("package-summary.html'><TITLE>");
		out.write(pkg.name());
		out.write("</TITLE>\n");

		ClassDoc[] classes = pkg.allClasses();
		String[] classNames = new String[classes.length];
		for(int i = 0; i < classes.length; i++)
		{
			classNames[i] = classes[i].name();
		}
		Arrays.sort(classNames);

		for(int i = 0; i < classes.length; i++)
		{
			processClass(out,pkgPath,classNames[i]);
		}

		out.write("</ENTRY>");
	}

	private static void processClass(Writer out, String pkgPath, String clazz)
		throws IOException
	{
		out.write("<ENTRY HREF='");
		out.write(pkgPath);
		out.write(clazz);
		out.write(".html'><TITLE>");
		out.write(clazz);
		out.write("</TITLE>\n");
		out.write("</ENTRY>");
	}
}
