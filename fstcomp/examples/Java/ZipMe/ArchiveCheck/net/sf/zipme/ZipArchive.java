//

package net.sf.zipme;
class ZipArchive {
   private void checkZipArchive() throws ZipException {
    if (len < 4) {
      throw new ZipException("Not a valid zip archive");
    }
    int sig=buf[off] & 0xFF | ((buf[off + 1] & 0xFF) << 8) | ((buf[off + 2] & 0xFF) << 16) | ((buf[off + 3] & 0xFF) << 24);
    if (sig != LOCSIG) {
      throw new ZipException("Not a valid zip archive");
    }
  }
   public void hook1() throws ZipException {
    checkZipArchive();
    original();
  }
}
