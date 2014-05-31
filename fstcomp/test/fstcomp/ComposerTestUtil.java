package fstcomp;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import composer.FSTGenComposer;


public class ComposerTestUtil {

	public static void compose(String expression, String outputDirectory, String baseDirectory, String[] extraArgs) {

		new File(outputDirectory).mkdir();

		if (baseDirectory == null) {
			File expressionFile = new File(expression);
			baseDirectory = expressionFile.getParent();
		}

		List<String> arguments = new ArrayList<String>();

		arguments.add("--base-directory");
		arguments.add(baseDirectory);
		arguments.add("--expression");
		arguments.add(expression);
		arguments.add("--output-directory");
		arguments.add(outputDirectory);

		if (extraArgs != null) {
			arguments.addAll(Arrays.asList(extraArgs));
		}

		//print arguments to stdout so they appear in the test output
		System.out.print("FSTComposer ");
		System.out.println(Arrays.toString(arguments.toArray()));

		FSTGenComposer.main(arguments.toArray(new String[0]));

	}

}
