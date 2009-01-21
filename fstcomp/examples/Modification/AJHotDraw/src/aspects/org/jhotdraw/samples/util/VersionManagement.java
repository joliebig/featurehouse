
package org.jhotdraw.util; 
import java.io.*; 
import java.util.*; 
import java.util.jar.*; 
public  class  VersionManagement {
		public static String JHOTDRAW_COMPONENT = "org.jhotdraw/";

		public static String JHOTDRAW_JAR = "jhotdraw.jar";

		public static Package[] packages = {	Package.getPackage("org.jhotdraw.applet"),	Package.getPackage("org.jhotdraw.application"),	Package.getPackage("org.jhotdraw.contrib"),	Package.getPackage("org.jhotdraw.figures"),	Package.getPackage("org.jhotdraw.framework"),	Package.getPackage("org.jhotdraw.standard"),	Package.getPackage("org.jhotdraw.util")	};

		public static String getJHotDrawVersion() {	Package pack = packages[4];	return pack.getSpecificationVersion();	}

		public static String getPackageVersion(final Package lookupPackage) {	if (lookupPackage == null) {	return null;	}	String specVersion = lookupPackage.getSpecificationVersion();	if (specVersion != null) {	return specVersion;	}	else {	String normalizedPackageName = normalizePackageName(lookupPackage.getName());	String nextPackageName = getNextPackage(normalizedPackageName);	return getPackageVersion(Package.getPackage(nextPackageName));	}	}

		public static boolean isCompatibleVersion(String compareVersionString) {	Package pack = packages[4];	if (compareVersionString == null) {	return pack.getSpecificationVersion() == null;	}	else {	return pack.isCompatibleWith(compareVersionString);	}	}

		public static String readVersionFromFile(String applicationName, String versionFileName) {	try {	FileInputStream fileInput = new FileInputStream(versionFileName);	Manifest manifest = new Manifest();	manifest.read(fileInput);	Map entries = manifest.getEntries();	Iterator entryIterator = entries.entrySet().iterator();	while (entryIterator.hasNext()) {	Map.Entry currentEntry = (Map.Entry)entryIterator.next();	String packageName = currentEntry.getKey().toString();	packageName = normalizePackageName(packageName);	Attributes attributes = (Attributes)currentEntry.getValue();	String packageSpecVersion = attributes.getValue(Attributes.Name.SPECIFICATION_VERSION);	packageSpecVersion = extractVersionInfo(packageSpecVersion);	return packageSpecVersion;	}	}	catch (IOException exception) {	exception.printStackTrace();	}	return null;	}

		protected static String getNextPackage(String searchPackage) {	if (searchPackage == null) {	return null;	}	int foundNextPackage = searchPackage.lastIndexOf('.');	if (foundNextPackage > 0) {	return searchPackage.substring(0, foundNextPackage);	}	else {	return null;	}	}

		public static String normalizePackageName(String toBeNormalized) {	String replaced = toBeNormalized.replace('/', '.');	replaced = replaced.replace(File.pathSeparatorChar, '.');	if (replaced.endsWith(".")) {	int lastSeparator = replaced.lastIndexOf('.');	return replaced.substring(0, lastSeparator);	}	else {	return replaced;	}	}

		public static String extractVersionInfo(String versionString) {	if (versionString == null) {	return null;	}	if (versionString.length() == 0) {	return "";	}	int startIndex = versionString.indexOf("\"");	if (startIndex < 0) {	startIndex = 0;	}	else {	startIndex++;	}	int endIndex = versionString.lastIndexOf("\"");	if (endIndex < 0) {	endIndex = versionString.length();	}	return versionString.substring(startIndex, endIndex);	}


}
