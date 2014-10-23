package integrationtests;

import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

public class Checksum {

	/**
	 * calculate checksum of string
	 * 
	 * @param s
	 * @return checksum of s
	 */
	public static String calculateChecksum(String s) {
		MessageDigest md = getMessageDigest();
		md.update(s.getBytes());
		return toHex(md.digest());
	}

	/**
	 * calculate checksum of file
	 * 
	 * If file is a directory, the checksum is calculated over all the contents of the directory
	 * that satisfy the filter criterium.
	 * If filter is null, all non-hidden files are included.
	 * hidden files starting with . are always excluded.
	 * @param file
	 * @return checksum of file
	 */
	public static String calculateChecksum(File file, FilenameFilter filter) {
		if (file.isDirectory()) {
			return calculateChecksumOfDirectory(file, filter);
		} else {
			return calculateChecksumOfFile(file);
		}
	}	

	private static String calculateChecksumOfFile(File file) {
		MessageDigest md = getMessageDigest();
		try {
			InputStream fis = new FileInputStream(file);
			DigestInputStream dis = new DigestInputStream(fis, md);
			byte[] buffer = new byte[1024];
			while (dis.read(buffer) != -1) {}
			dis.close();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return toHex(md.digest());
	}

	private static String calculateChecksumOfDirectory(File file, FilenameFilter filter) {
		//simple hash tree
		String toHash = "[";
		String[] filelist = file.list(filter);
		Arrays.sort(filelist);
		for (String item : filelist) {
			if (!file.getName().startsWith(".")) // ignore hidden files and directories
				toHash += '#' + item + ':' + calculateChecksum(new File(file, item), filter);
		}
		toHash += ']';
		return calculateChecksum(toHash);
	}

	private static MessageDigest getMessageDigest() {
		try {
			return MessageDigest.getInstance("MD5");
		} catch (NoSuchAlgorithmException e1) {
			//md5 exists, so this will never happen
			throw new RuntimeException(e1);
		}
	}

	private static String toHex(byte[] bytes) {
		
		BigInteger bi = new BigInteger(1, bytes);
		return String.format("%0" + (bytes.length << 1) + "X", bi);

	}
	
}