


package net.sourceforge.jarbundler;


import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.LinkedList;


import java.lang.String;

public class AppBundleProperties {

	
	private String mApplicationName;
	private String mMainClass;

	
	private String mCFBundleName = null;

	
	private String mCFBundleShortVersionString = "1.0";

	
	private String mCFBundleGetInfoString = null;

	
	private String mCFBundleVersion = null;

	
	private String mCFHelpBookFolder = null;

	
	private String mCFHelpBookName = null;

	
	private boolean mCFBundleAllowMixedLocalizations = false;

	
	private String mCFBundleExecutable = "JavaApplicationStub";

	
	private String mCFBundleDevelopmentRegion = "English";

	
	private final String mCFBundlePackageType = "APPL";

	
	private String mCFBundleSignature = "????";

	
	private String mJVMVersion = "1.3+";

	
	private final String mCFBundleInfoDictionaryVersion = "6.0";

	

	private String mCFBundleIconFile = null;
	private String mCFBundleIdentifier = null;
	private String mVMOptions = null; 
	private String mWorkingDirectory = null; 
	private String mArguments = null; 

	
	private List mClassPath = new ArrayList();
	private List mExtraClassPath = new ArrayList();

	
	private Hashtable mJavaProperties = new Hashtable();

	
	private List mDocumentTypes = new LinkedList();

	
	private List mServices = new LinkedList();
	
	

	

	public void addJavaProperty(String prop, String val) {
		mJavaProperties.put(prop, val);
	}

	public Hashtable getJavaProperties() {
		return mJavaProperties;
	}

	public void addToClassPath(String s) {
		mClassPath.add("$JAVAROOT/" + s);
	}

	public void addToExtraClassPath(String s) {
		mExtraClassPath.add(s);
	}

	public List getExtraClassPath() {
		return mExtraClassPath;
	}

	public DocumentType createDocumentType() {
		return new DocumentType();
	}

	public List getDocumentTypes() {
		return mDocumentTypes;
	}

	
	public void addDocumentType(DocumentType documentType) {
		mDocumentTypes.add(documentType);
	}

	public Service createService() {
		return new Service();
	}
	
	public List getServices() {
		return mServices;
	}
	
	
	public void addService(Service service) {
		mServices.add(service);
	}
	
	

	public void setApplicationName(String s) {
		mApplicationName = s;
	}

	public String getApplicationName() {
		return mApplicationName;
	}

	
	
	
	

	public void setCFBundleName(String s) {

		if (s.length() > 16)
			System.err
					.println("WARNING: 'shortname' is recommeded to be no more than 16 "
							+ "charaters long. See usage notes.");
		mCFBundleName = s;
	}

	public String getCFBundleName() {
		if (mCFBundleName == null)
			return getApplicationName();

		return mCFBundleName;
	}

	public void setCFBundleVersion(String s) {
		mCFBundleVersion = s;
	}

	public String getCFBundleVersion() {
		return mCFBundleVersion;
	}

	public void setCFBundleInfoDictionaryVersion(String s) {
		
	}

	public String getCFBundleInfoDictionaryVersion() {
		return mCFBundleInfoDictionaryVersion;
	}

	public void setCFBundleIdentifier(String s) {
		mCFBundleIdentifier = s;
	}

	public String getCFBundleIdentifier() {
		return mCFBundleIdentifier;
	}

	public void setCFBundleGetInfoString(String s) {
		mCFBundleGetInfoString = s;
	}

	public String getCFBundleGetInfoString() {
		if (mCFBundleGetInfoString == null)
			return getCFBundleShortVersionString();

		return mCFBundleGetInfoString;
	}

	public void setCFBundleShortVersionString(String s) {
		mCFBundleShortVersionString = s;
	}

	public String getCFBundleShortVersionString() {
		return mCFBundleShortVersionString;
	}

	public void setCFBundleIconFile(String s) {
		mCFBundleIconFile = s;
	}

	public String getCFBundleIconFile() {
		return mCFBundleIconFile;
	}

	public void setCFBundleAllowMixedLocalizations(boolean b) {
		mCFBundleAllowMixedLocalizations = b;
	}

	public boolean getCFBundleAllowMixedLocalizations() {
		return mCFBundleAllowMixedLocalizations;
	}

	public void setCFBundleExecutable(String s) {
		mCFBundleExecutable = s;
	}

	public String getCFBundleExecutable() {
		return mCFBundleExecutable;
	}

	public void setCFBundleDevelopmentRegion(String s) {
		mCFBundleDevelopmentRegion = s;
	}

	public String getCFBundleDevelopmentRegion() {
		return mCFBundleDevelopmentRegion;
	}

	public void setCFBundlePackageType(String s) {
		
	}

	public String getCFBundlePackageType() {
		return mCFBundlePackageType;
	}

	public void setCFBundleSignature(String s) {
		mCFBundleSignature = s;
	}

	public String getCFBundleSignature() {
		return mCFBundleSignature;
	}

	public void setCFBundleHelpBookFolder(String s) {
		mCFHelpBookFolder = s;
	}

	public String getCFBundleHelpBookFolder() {
		return mCFHelpBookFolder;
	}

	public void setCFBundleHelpBookName(String s) {
		mCFHelpBookName = s;
	}

	public String getCFBundleHelpBookName() {
		return mCFHelpBookName;
	}

	public void setMainClass(String s) {
		mMainClass = s;
	}

	public String getMainClass() {
		return mMainClass;
	}

	public void setJVMVersion(String s) {
		mJVMVersion = s;
	}

	public String getJVMVersion() {
		return mJVMVersion;
	}

	public void setVMOptions(String s) {
		mVMOptions = s;
	}

	public String getVMOptions() {
		return mVMOptions;
	}

	public void setWorkingDirectory(String s) {
		mWorkingDirectory = s;
	}

	public String getWorkingDirectory() {
		return mWorkingDirectory;
	}

	public void setArguments(String s) {
		mArguments = s;
	}

	public String getArguments() {
		return mArguments;
	}

	public List getClassPath() {
		return mClassPath;
	}

}
