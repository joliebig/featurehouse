////

package com.adarshr.raroscope;
class RARFile {
  /** 
 * Decodes the version information. Version number will be of the format
 * 10 * Major Version + Minor Version.
 * @param v the version number.
 * @return the decoded version in the format "major.minor".
 */
   protected String toVersion(  int v){
    return String.valueOf(v / 10F);
  }
}
