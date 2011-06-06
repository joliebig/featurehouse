

package net.sourceforge.jarbundler;


import net.sourceforge.jarbundler.AppBundleProperties;
import net.sourceforge.jarbundler.DocumentType;
import net.sourceforge.jarbundler.JavaProperty;
import net.sourceforge.jarbundler.PropertyListWriter;


import java.io.BufferedWriter;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;


import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;


import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.FileScanner;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;

import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.PatternSet;

import org.apache.tools.ant.taskdefs.MatchingTask;
import org.apache.tools.ant.taskdefs.Chmod;
import org.apache.tools.ant.taskdefs.Delete;

import org.apache.tools.ant.util.FileUtils;



import java.lang.Boolean;
import java.lang.Process;
import java.lang.Runtime;
import java.lang.String;
import java.lang.System;


public class JarBundler extends MatchingTask {

	private static final String DEFAULT_STUB = "/System/Library/Frameworks/JavaVM.framework/Versions/Current/Resources/MacOS/JavaApplicationStub";

	private static final String ABOUTMENU_KEY = "com.apple.mrj.application.apple.menu.about.name";
	private static final Set menuItems = new HashSet();
	private File mAppIcon;

	private File mRootDir;

	private final List mJavaFileLists = new ArrayList();
	private final List mJarFileSets = new ArrayList();

	private final List mExecFileLists = new ArrayList();
	private final List mExecFileSets = new ArrayList();

	private final List mResourceFileLists = new ArrayList();
	private final List mResourceFileSets = new ArrayList();

	private final List mJarFileLists = new ArrayList();
	private final List mJavaFileSets = new ArrayList();

	private final List mExtraClassPathFileLists = new ArrayList();
	private final List mExtraClassPathFileSets = new ArrayList();

	private final List mJarAttrs = new ArrayList();

	private final List mExecAttrs = new ArrayList();

	private final List mExtraClassPathAttrs = new ArrayList();
	
	private final List mHelpBooks = new ArrayList();

	private boolean mVerbose = false;
	private boolean mShowPlist = false;

	

	private File mStubFile = new File(DEFAULT_STUB);

	private Boolean mAntiAliasedGraphics = null;

	private Boolean mAntiAliasedText = null;

	private Boolean mLiveResize = null;

	private Boolean mScreenMenuBar = null;

	private Boolean mGrowbox = null;

	private Boolean mGrowboxIntrudes = null;

	
	private File bundleDir;

	
	private File mContentsDir;

	
	private File mMacOsDir;

	
	private File mResourcesDir;

	
	private File mJavaDir;

	
	
	


	private AppBundleProperties bundleProperties = new AppBundleProperties();

	

	private FileUtils mFileUtils = FileUtils.getFileUtils();

	

	
	public void setArguments(String s) {
		bundleProperties.setArguments(s);
	}

	
	public void setStubFile(File file) {
		mStubFile = (file.exists()) ? file : new File(DEFAULT_STUB);
		bundleProperties.setCFBundleExecutable(file.getName());
	}

	
	public void setDir(File f) {
		mRootDir = f;
	}

	
	public void setName(String s) {
		bundleProperties.setApplicationName(s);
	}

	
	public void setShortName(String s) {
		bundleProperties.setCFBundleName(s);
	}

	
	public void setMainClass(String s) {
		bundleProperties.setMainClass(s);
	}

	
	public void setWorkingDirectory(String s) {
		bundleProperties.setWorkingDirectory(s);
	}

	

	public void setIcon(File f) {
		mAppIcon = f;
		bundleProperties.setCFBundleIconFile(f.getName());
	}

	
	public void setBundleid(String s) {
		bundleProperties.setCFBundleIdentifier(s);
	}

	
	public void setDevelopmentregion(String s) {
		bundleProperties.setCFBundleDevelopmentRegion(s);
	}

	
	public void setAboutmenuname(String s) {
		bundleProperties.setCFBundleName(s);
	}

	
	public void setSmallTabs(boolean b) {
		bundleProperties.addJavaProperty("com.apple.smallTabs", new Boolean(b)
				.toString());
	}

	
	public void setVmoptions(String s) {
		bundleProperties.setVMOptions(s);
	}

	
	public void setAntialiasedgraphics(boolean b) {
		mAntiAliasedGraphics = new Boolean(b);
	}

	
	public void setAntialiasedtext(boolean b) {
		mAntiAliasedText = new Boolean(b);
	}

	
	public void setScreenmenu(boolean b) {
		mScreenMenuBar = new Boolean(b);
	}

	
	public void setGrowbox(boolean b) {
		mGrowbox = new Boolean(b);
	}

	
	public void setGrowboxintrudes(boolean b) {
		mGrowboxIntrudes = new Boolean(b);
	}

	
	public void setLiveresize(boolean b) {
		mLiveResize = new Boolean(b);
	}

	
	public void setType(String s) {
		bundleProperties.setCFBundlePackageType(s);
	}

	
	public void setSignature(String s) {
		bundleProperties.setCFBundleSignature(s);
	}

	
	public void setJvmversion(String s) {
		bundleProperties.setJVMVersion(s);
	}

	

	public void setInfoString(String s) {
		bundleProperties.setCFBundleGetInfoString(s);
	}

	
	public void setShortInfoString(String s) {
		setVersion(s);
	}

	
	public void setVerbose(boolean verbose) {
		this.mVerbose = verbose;
	}
	public void setShowPlist(boolean showPlist) {
		this.mShowPlist = showPlist;
	}




	
	public void setBuild(String s) {
		bundleProperties.setCFBundleVersion(s);
	}

	
	public void setVersion(String s) {
		bundleProperties.setCFBundleShortVersionString(s);
	}

	public void setHelpBookFolder(String s) {
		bundleProperties.setCFBundleHelpBookFolder(s);
	}

	public void setHelpBookName(String s) {
		bundleProperties.setCFBundleHelpBookName(s);
	}

	
	public void setJars(String s) {
		PatternSet patset = new PatternSet();
		patset.setIncludes(s);

		String[] jarNames = patset.getIncludePatterns(getProject());

		for (int i = 0; i < jarNames.length; i++)
			mJarAttrs.add(getProject().resolveFile(jarNames[i]));
	}

	
	public void setJar(File s) {
		mJarAttrs.add(s);
	}

	
	public void setExecs(String s) {
		PatternSet patset = new PatternSet();
		patset.setIncludes(s);

		String[] execNames = patset.getIncludePatterns(getProject());

		for (int i = 0; i < execNames.length; i++) {
			File f = new File(execNames[i]);
			mExecAttrs.add(f);
		}
	}

	
	public void setExtraclasspath(String s) {
		PatternSet patset = new PatternSet();
		patset.setIncludes(s);

		String[] cpNames = patset.getIncludePatterns(getProject());

		for (int i = 0; i < cpNames.length; i++) {
			File f = new File(cpNames[i]);
			mExtraClassPathAttrs.add(f);
		}
	}

	
	public void setChmod(String s) {
		log("The \"chmod\" attribute has deprecaited, using the ANT Chmod task internally");
	}

	

	public void addJarfileset(FileSet fs) {
		mJarFileSets.add(fs);
	}

	public void addJarfilelist(FileList fl) {
		mJarFileLists.add(fl);
	}

	public void addExecfileset(FileSet fs) {
		mExecFileSets.add(fs);
	}

	public void addExecfilelist(FileList fl) {
		mExecFileLists.add(fl);
	}

	public void addResourcefileset(FileSet fs) {
		mResourceFileSets.add(fs);
	}

	public void addResourcefilelist(FileList fl) {
		mResourceFileLists.add(fl);
	}

	public void addJavafileset(FileSet fs) {
		mJavaFileSets.add(fs);
	}

	public void addJavafilelist(FileList fl) {
		mJavaFileLists.add(fl);
	}

	public void addExtraclasspathfileset(FileSet fs) {
		mExtraClassPathFileSets.add(fs);
	}

	public void addExtraclasspathfilelist(FileList fl) {
		mExtraClassPathFileLists.add(fl);
	}


	


	public void addConfiguredJavaProperty(JavaProperty javaProperty)
			throws BuildException {

		String name = javaProperty.getName();
		String value = javaProperty.getValue();

		if ((name == null) || (value == null))
			throw new BuildException(
					"'<javaproperty>' must have both 'name' and 'value' attibutes");

		bundleProperties.addJavaProperty(name, value);
	}

	public void addConfiguredDocumentType(DocumentType documentType) throws BuildException {

		String name = documentType.getName();
		String role = documentType.getRole();
		List osTypes = documentType.getOSTypes();
		List extensions = documentType.getExtensions();
		List mimeTypes = documentType.getMimeTypes();

		if ((name == null) || (role == null))
			throw new BuildException(
					"'<documenttype>' must have both a 'name' and a 'role' attibute");

		if ((osTypes.isEmpty()) && (extensions.isEmpty()) && (mimeTypes.isEmpty()))
			throw new BuildException(
					"'<documenttype>' of \""
							+ name
							+ "\" must have 'osTypes' or 'extensions' or 'mimeTypes'");

		bundleProperties.addDocumentType(documentType);
	}

	public void addConfiguredService(Service service) {
	
		
		
		
		if (service.getMessage() == null)
			throw new BuildException("\"<service>\" must have a \"message\" attribute");
		
		String menuItem = service.getMenuItem();
		if (menuItem == null)
			throw new BuildException("\"<service>\" must have a \"menuItem\" attribute");
		if (!menuItems.add(menuItem))
			throw new BuildException("\"<service>\" \"menuItem\" value must be unique");
		
		if (service.getSendTypes().isEmpty() && service.getReturnTypes().isEmpty())
			throw new BuildException("\"<service>\" must have either a \"sendTypes\" attribute, a \"returnTypes\" attribute or both");
		
		String keyEquivalent = service.getKeyEquivalent();
		if ((keyEquivalent != null) && (1 != keyEquivalent.length()))
			throw new BuildException("\"<service>\" \"keyEquivalent\" must be one character if present");
		
		String timeoutString = service.getTimeout();
		if (timeoutString != null) {
			long timeout = -1;
			try {
				timeout = Long.parseLong(timeoutString);
			} catch (NumberFormatException nfe) {
				throw new BuildException("\"<service>\" \"timeout\" must be a positive integral number");
			}
			if (timeout < 0)
				throw new BuildException("\"<service>\" \"timeout\" must not be negative");
		}
		
		bundleProperties.addService(service);
	}
	
	public void addConfiguredHelpBook(HelpBook helpBook) {
	
		
		if (helpBook.getFolderName() == null) {
			if (bundleProperties.getCFBundleHelpBookFolder() == null)
				throw new BuildException("Either the '<helpbook>' attribute 'foldername' or the '<jarbundler>' attribute 'helpbookfolder' must be defined");
			helpBook.setFolderName(bundleProperties.getCFBundleHelpBookFolder());
		}

		
		if (helpBook.getName() == null) {
			if (bundleProperties.getCFBundleHelpBookName() == null)
				throw new BuildException("Either the '<helpbook>' attribute 'name' or the '<jarbundler>' attribute 'helpbookname' must be defined");
			helpBook.setName(bundleProperties.getCFBundleHelpBookName());
		}

		
		List fileLists = helpBook.getFileLists();
		List fileSets = helpBook.getFileSets();

		if ( fileLists.isEmpty() && fileSets.isEmpty() )
			throw new BuildException("The '<helpbook>' task must have either " +
			                         "'<fileset>' or  '<filelist>' nested tags");


		mHelpBooks.add(helpBook);
	}



	

	

	public void execute() throws BuildException {

		

		bundleDir = new File(mRootDir, bundleProperties.getApplicationName() + ".app");

		if (bundleDir.exists()) {
			Delete deleteTask = new Delete();
            deleteTask.setProject(getProject());
			deleteTask.setDir(bundleDir);
			deleteTask.execute();
		}

		
		

		if (mRootDir == null)
			throw new BuildException("Required attribute \"dir\" is not set.");

		if (mJarAttrs.isEmpty() && mJarFileSets.isEmpty()
				&& mJarFileLists.isEmpty())
			throw new BuildException("Either the attribute \"jar\" must "
					+ "be set, or one or more jarfilelists or "
					+ "jarfilesets must be added.");

		if (!mJarAttrs.isEmpty()
				&& (!mJarFileSets.isEmpty() || !mJarFileLists.isEmpty()))
			throw new BuildException(
					"Cannot set both the attribute "
							+ "\"jars\" and use jar filesets/filelists.  Use only one or the other.");

		if (bundleProperties.getApplicationName() == null)
			throw new BuildException("Required attribute \"name\" is not set.");

		if (bundleProperties.getMainClass() == null)
			throw new BuildException(
					"Required attribute \"mainclass\" is not set.");

		

		

		
		if (useOldPropertyNames())
			bundleProperties.addJavaProperty(ABOUTMENU_KEY, bundleProperties
					.getCFBundleName());

		
		String antiAliasedProperty = useOldPropertyNames()
				? "com.apple.macosx.AntiAliasedGraphicsOn"
				: "apple.awt.antialiasing";

		if (mAntiAliasedGraphics != null)
			bundleProperties.addJavaProperty(antiAliasedProperty,
					mAntiAliasedGraphics.toString());

		
		String antiAliasedTextProperty = useOldPropertyNames()
				? "com.apple.macosx.AntiAliasedTextOn"
				: "apple.awt.textantialiasing";

		if (mAntiAliasedText != null)
			bundleProperties.addJavaProperty(antiAliasedTextProperty,
					mAntiAliasedText.toString());

		
		if (useOldPropertyNames() && (mLiveResize != null))
			bundleProperties.addJavaProperty(
					"com.apple.mrj.application.live-resize", mLiveResize
							.toString());

		
		String screenMenuBarProperty = useOldPropertyNames()
				? "com.apple.macos.useScreenMenuBar"
				: "apple.laf.useScreenMenuBar";

		if (mScreenMenuBar != null)
			bundleProperties.addJavaProperty(screenMenuBarProperty,
					mScreenMenuBar.toString());

		
		if ((useOldPropertyNames() == false) && (mGrowbox != null))
			bundleProperties.addJavaProperty("apple.awt.showGrowBox", mGrowbox
					.toString());

		
		if (useOldPropertyNames() && (mGrowboxIntrudes != null))
			bundleProperties.addJavaProperty(
					"com.apple.mrj.application.growbox.intrudes",
					mGrowboxIntrudes.toString());

		if (!mRootDir.exists()
				|| (mRootDir.exists() && !mRootDir.isDirectory()))
			throw new BuildException(
					"Destination directory specified by \"dir\" "
							+ "attribute must already exist.");

		if (bundleDir.exists())
			throw new BuildException("The directory/bundle \""
					+ bundleDir.getName()
					+ "\" already exists, cannot continue.");

		
		log("Creating application bundle: " + bundleDir);

		if (!bundleDir.mkdir())
			throw new BuildException("Unable to create bundle: " + bundleDir);

		
		mContentsDir = new File(bundleDir, "Contents");

		if (!mContentsDir.mkdir())
			throw new BuildException("Unable to create directory "
					+ mContentsDir);

		
		mMacOsDir = new File(mContentsDir, "MacOS");

		if (!mMacOsDir.mkdir())
			throw new BuildException("Unable to create directory " + mMacOsDir);

		
		mResourcesDir = new File(mContentsDir, "Resources");

		if (!mResourcesDir.mkdir())
			throw new BuildException("Unable to create directory "
					+ mResourcesDir);

		
		mJavaDir = new File(mResourcesDir, "Java");

		if (!mJavaDir.mkdir())
			throw new BuildException("Unable to create directory " + mJavaDir);

		
		

		if (mAppIcon != null) {
		

			try {
				File dest = new File(mResourcesDir, mAppIcon.getName());

				if(mVerbose)
					log("Copying application icon file to \"" + bundlePath(dest) + "\"");

				mFileUtils.copyFile(mAppIcon, dest);
			} catch (IOException ex) {
				throw new BuildException("Cannot copy icon file: " + ex);
			}
		}

		
		try {
			Iterator itor = bundleProperties.getDocumentTypes().iterator();

			while (itor.hasNext()) {
				DocumentType documentType = (DocumentType) itor.next();
				File iconFile = documentType.getIconFile();
				if (iconFile != null) {
					File dest = new File(mResourcesDir, iconFile.getName());
					if(mVerbose)
						log("Copying document icon file to \"" + bundlePath(dest) + "\"");
					mFileUtils.copyFile(iconFile, dest);
				}
			}
		} catch (IOException ex) {
			throw new BuildException("Cannot copy document icon file: " + ex);
		}

		
		processJarAttrs();

		
		processJarFileSets();

		
		processJarFileLists();

		
		processExecAttrs();

		
		processExecFileSets();

		
		processExecFileLists();

		
		processResourceFileSets();

		
		processJavaFileSets();

		
		processResourceFileLists();

		
		processJavaFileLists();

		
		processExtraClassPathAttrs();

		
		
		processExtraClassPathFileSets();

		
		
		processExtraClassPathFileLists();

		
		copyHelpBooks();

		
		
		copyApplicationStub();

		
		writeInfoPlist();

		
		writePkgInfo();

		
	}

	

	private void setExecutable(File f) {

		Chmod chmodTask = new Chmod();
		chmodTask.setProject(getProject());
		chmodTask.setFile(f);
		chmodTask.setPerm("ugo+rx");

		if (mVerbose)
			log("Setting \"" + bundlePath(f) + "\" to executable");

		chmodTask.execute();

	}

	

	private boolean useOldPropertyNames() {
		return (bundleProperties.getJVMVersion().startsWith("1.3"));
	}

	private void processJarAttrs() throws BuildException {

		try {

			for (Iterator jarIter = mJarAttrs.iterator(); jarIter.hasNext();) {
				File src = (File) jarIter.next();
				File dest = new File(mJavaDir, src.getName());

				if (mVerbose) 
					log("Copying JAR file to \"" + bundlePath(dest) + "\"");
				

				mFileUtils.copyFile(src, dest);
				bundleProperties.addToClassPath(dest.getName());
			}
		} catch (IOException ex) {
			throw new BuildException("Cannot copy jar file: " + ex);
		}
	}

	private void processJarFileSets() throws BuildException {

		for (Iterator jarIter = mJarFileSets.iterator(); jarIter.hasNext();) {

			FileSet fs = (FileSet) jarIter.next();

			Project p = fs.getProject();
			File srcDir = fs.getDir(p);
			FileScanner ds = fs.getDirectoryScanner(p);
			fs.setupDirectoryScanner(ds, p);
			ds.scan();

			String[] files = ds.getIncludedFiles();

			try {

				for (int i = 0; i < files.length; i++) {
					String fileName = files[i];
					File src = new File(srcDir, fileName);
					File dest = new File(mJavaDir, fileName);

					if (mVerbose)
						log("Copying JAR file to \"" + bundlePath(dest) + "\"");

					mFileUtils.copyFile(src, dest);
					bundleProperties.addToClassPath(fileName);
				}

			} catch (IOException ex) {
				throw new BuildException("Cannot copy jar file: " + ex);
			}
		}
	}

	private void processJarFileLists() throws BuildException {

		for (Iterator jarIter = mJarFileLists.iterator(); jarIter.hasNext();) {
			FileList fl = (FileList) jarIter.next();
			Project p = fl.getProject();
			File srcDir = fl.getDir(p);
			String[] files = fl.getFiles(p);

			try {

				for (int i = 0; i < files.length; i++) {
					String fileName = files[i];
					File src = new File(srcDir, fileName);
					File dest = new File(mJavaDir, fileName);

					if (mVerbose) 
						log("Copying JAR file to \"" + bundlePath(dest) + "\"");
					

					mFileUtils.copyFile(src, dest);
					bundleProperties.addToClassPath(fileName);
				}
			} catch (IOException ex) {
				throw new BuildException("Cannot copy jar file: " + ex);
			}
		}
	}

	private void processExtraClassPathAttrs() throws BuildException {

		for (Iterator jarIter = mExtraClassPathAttrs.iterator(); jarIter
				.hasNext();) {
			File src = (File) jarIter.next();
			bundleProperties.addToExtraClassPath(src.getPath());
		}
	}

	private void processExtraClassPathFileSets() throws BuildException {

		for (Iterator jarIter = mExtraClassPathFileSets.iterator(); jarIter
				.hasNext();) {
			FileSet fs = (FileSet) jarIter.next();
			Project p = fs.getProject();
			File srcDir = fs.getDir(p);
			FileScanner ds = fs.getDirectoryScanner(p);
			fs.setupDirectoryScanner(ds, p);
			ds.scan();

			String[] files = ds.getIncludedFiles();

			for (int i = 0; i < files.length; i++) {
				File f = new File(srcDir, files[i]);
				bundleProperties.addToExtraClassPath(f.getPath());
			}
		}
	}

	private void processExtraClassPathFileLists() throws BuildException {

		for (Iterator jarIter = mExtraClassPathFileLists.iterator(); jarIter
				.hasNext();) {
			FileList fl = (FileList) jarIter.next();
			Project p = fl.getProject();
			File srcDir = fl.getDir(p);
			String[] files = fl.getFiles(p);

			for (int i = 0; i < files.length; i++) {
				File f = new File(srcDir, files[i]);
				bundleProperties.addToExtraClassPath(f.getPath());
			}
		}
	}

	private void processExecAttrs() throws BuildException {

		try {

			for (Iterator execIter = mExecAttrs.iterator(); execIter.hasNext();) {
				File src = (File) execIter.next();
				File dest = new File(mMacOsDir, src.getName());

				if (mVerbose) 
					log("Copying exec file to \"" + bundlePath(dest) + "\"");
				

				mFileUtils.copyFile(src, dest);
				setExecutable(dest);
			}
		} catch (IOException ex) {
			throw new BuildException("Cannot copy exec file: " + ex);
		}
	}

	

	
	private void processExecFileSets() {
		processCopyingFileSets(mExecFileSets, mMacOsDir, true);
	}

	
	private void processResourceFileSets() {
		processCopyingFileSets(mResourceFileSets, mResourcesDir, false);
	}

	
	private void processJavaFileSets() {
		processCopyingFileSets(mJavaFileSets, mJavaDir, false);
	}

	private void processCopyingFileSets(List fileSets, File targetdir, boolean setExec) {

		for (Iterator execIter = fileSets.iterator(); execIter.hasNext();) {
			FileSet fs = (FileSet) execIter.next();
			Project p = fs.getProject();
			File srcDir = fs.getDir(p);
			FileScanner ds = fs.getDirectoryScanner(p);
			fs.setupDirectoryScanner(ds, p);
			ds.scan();

			String[] files = ds.getIncludedFiles();

			if (files.length == 0) {
				
				System.err
						.println("WARNING: fileset for copying from directory "
								+ srcDir + ": no files found");
			} else {
				try {
					for (int i = 0; i < files.length; i++) {
						String fileName = files[i];
						File src = new File(srcDir, fileName);
						File dest = new File(targetdir, fileName);
						
						if (mVerbose) 
							log("Copying "
									+ (setExec ? "exec" : "resource")
									+ " file to \"" + bundlePath(dest) +"\"");
						
						mFileUtils.copyFile(src, dest);
						if (setExec)
							setExecutable(dest);
					}
				} catch (IOException ex) {
					throw new BuildException("Cannot copy file: " + ex);
				}
			}
		}
	}

	

	
	private void processExecFileLists() throws BuildException {
		processCopyingFileLists(mExecFileLists, mMacOsDir, true);
	}

	
	private void processResourceFileLists() throws BuildException {
		processCopyingFileLists(mResourceFileLists, mResourcesDir, false);
	}

	
	private void processJavaFileLists() throws BuildException {
		processCopyingFileLists(mJavaFileLists, mJavaDir, false);
	}

	private void processCopyingFileLists(List fileLists, File targetDir, boolean setExec) throws BuildException {

		for (Iterator execIter = fileLists.iterator(); execIter.hasNext();) {

			FileList fl = (FileList) execIter.next();
			Project p = fl.getProject();
			File srcDir = fl.getDir(p);
			String[] files = fl.getFiles(p);

			if (files.length == 0) {
				
				System.err.println("WARNING: filelist for copying from directory "
								+ srcDir + ": no files found");
			} else {
				try {
					for (int i = 0; i < files.length; i++) {
						String fileName = files[i];
						File src = new File(srcDir, fileName);
						File dest = new File(targetDir, fileName);
						
						if (mVerbose) 
							log("Copying "
									+ (setExec ? "exec" : "resource")
									+ " file to \"" + bundlePath(dest) +"\"");
						
						mFileUtils.copyFile(src, dest);
						if (setExec)
							setExecutable(dest);
					}
				} catch (IOException ex) {
					throw new BuildException("Cannot copy jar file: " + ex);
				}
			}
		}
	}



	private void copyHelpBooks() {

		for (Iterator itor = mHelpBooks.iterator(); itor.hasNext();) {

			HelpBook helpBook = (HelpBook)itor.next();
			
			String folderName = helpBook.getFolderName();
			String name = helpBook.getName();
			String locale = helpBook.getLocale();
			
			List fileLists = helpBook.getFileLists();
			List fileSets = helpBook.getFileSets();


			File helpBookDir = null;
			
			if (locale == null) {
			
				
				if (folderName != null)
					bundleProperties.setCFBundleHelpBookFolder(folderName);
				
				if (name != null)
					bundleProperties.setCFBundleHelpBookName(name);
				
				
				helpBookDir = new File(mResourcesDir, folderName);
				helpBookDir.mkdir();

				if(mVerbose)
					log("Creating Help Book at \"" + 
					                    bundlePath(helpBookDir) + "\"");

				
			} else {

				

				File lproj = new File(mResourcesDir, locale + ".lproj");
				lproj.mkdir();
				helpBookDir = new File(lproj, folderName);
				helpBookDir.mkdir();

				if(mVerbose)
					log("Creating Help Book for \"" + locale +
					                    "\" at \"" + bundlePath(helpBookDir)  + "\"");

				
				File infoPList = new File(lproj, "InfoPlist.strings");
				PrintWriter writer = null;
				try {
 					writer = new PrintWriter(new FileWriter(infoPList));
       				writer.println("CFBundleHelpBookFolder = \"" + folderName + "\";");
       				writer.println("CFBundleHelpBookName = \"" + name + "\";");
       				writer.println("CFBundleName = \"" + bundleProperties.getCFBundleName() + "\";");
       			} catch (IOException ioe) {
       				throw new BuildException("IOException in writing Help Book locale: " + locale);
       			} finally {
		        	mFileUtils.close(writer);
		        }
			}

			

			processCopyingFileSets(fileSets, helpBookDir, false);
			processCopyingFileLists(fileLists, helpBookDir, false);

		}
	}




	
	

	private void copyApplicationStub() throws BuildException {

		File newStubFile = new File(mMacOsDir, bundleProperties.getCFBundleExecutable());

		if (mVerbose)
			log("Copying Java application stub to \"" + bundlePath(newStubFile) + "\"");

		try {
			mFileUtils.copyFile(mStubFile, newStubFile);
		} catch (IOException ex) {
			throw new BuildException("Cannot copy Java Application Stub: " + ex);
		}

		

		setExecutable(newStubFile);
	}

	private void writeInfoPlist() throws BuildException {
		PropertyListWriter listWriter = new PropertyListWriter(bundleProperties);
		File infoPlist = new File(mContentsDir, "Info.plist");

		listWriter.writeFile(infoPlist);
		
		if (mVerbose) 
			log("Creating \"" + bundlePath(infoPlist) + "\" file");


		if (mShowPlist) {
			try {
				BufferedReader in = new BufferedReader(new FileReader(infoPlist));
				String str;
				while ((str = in.readLine()) != null) 
					log(str);
				in.close();
    		} catch (IOException e) {
    			throw new BuildException(e);
    		}			
		}
	}


	
	
	

	private void writePkgInfo() throws BuildException {
		File pkgInfo = new File(mContentsDir, "PkgInfo");
		PrintWriter writer = null;

		try {
			writer = new PrintWriter(new BufferedWriter(new FileWriter(pkgInfo)));
			writer.print(bundleProperties.getCFBundlePackageType());
			writer.println(bundleProperties.getCFBundleSignature());
			writer.flush();
		} catch (IOException ex) {
			throw new BuildException("Cannot create PkgInfo file: " + ex);
		} finally {
			mFileUtils.close(writer);
		}
	}

	private String bundlePath(File bundleFile) {
	
		String rootPath = bundleDir.getAbsolutePath();
		String thisPath = bundleFile.getAbsolutePath();
	
		return thisPath.substring(rootPath.length());
	
	}
}
