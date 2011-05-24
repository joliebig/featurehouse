
package org.jhotdraw.util; 
import org.jhotdraw.framework.*; 
public  class  StandardVersionControlStrategy  implements VersionControlStrategy {
		private VersionRequester myVersionRequester;

		public StandardVersionControlStrategy(VersionRequester newVersionRequester) {	setVersionRequester(newVersionRequester);	}

		public void assertCompatibleVersion() {	String[] requiredVersions = getVersionRequester().getRequiredVersions();	if (requiredVersions.length == 0) {	return;	}	for (int i = 0; i < requiredVersions.length; i++) {	if (isCompatibleVersion(requiredVersions[i])) {	return;	}	}	handleIncompatibleVersions();	}

		protected void handleIncompatibleVersions() {	String[] requiredVersions = getVersionRequester().getRequiredVersions();	StringBuffer expectedVersions = new StringBuffer("[");	for (int i = 0; i < requiredVersions.length - 1; i++) {	expectedVersions.append(requiredVersions[i] + ", ");	}	if (requiredVersions.length > 0) {	expectedVersions.append(requiredVersions[requiredVersions.length - 1]);	}	expectedVersions.append("]");	throw new JHotDrawRuntimeException("Incompatible version of JHotDraw found: "	+ VersionManagement.getJHotDrawVersion()	+ " (expected: " + expectedVersions + ")");	}

		protected boolean isCompatibleVersion(String compareVersionString) {	return VersionManagement.isCompatibleVersion(compareVersionString);	}

		private void setVersionRequester(VersionRequester newVersionRequester) {	myVersionRequester = newVersionRequester;	}

		protected VersionRequester getVersionRequester() {	return myVersionRequester;	}


}
