package de.masters_of_disaster.ant.tasks.ar;

import java.io.File;
import java.util.Date;



public class ArEntry implements ArConstants {
    
    private StringBuffer filename;

    
    private long fileDate;

    
    private int userId;

    
    private int groupId;

    
    private int mode;

    
    private long size;

    
    private StringBuffer magic;

    
    private File file;

    
    public static final int DEFAULT_FILE_MODE = 0100644;

    
    public static final int MILLIS_PER_SECOND = 1000;

    
    private ArEntry () {
        this.magic = new StringBuffer(HEADERMAGIC);
        this.filename = new StringBuffer();
        this.userId = 0;
        this.groupId = 0;
        this.file = null;
    }

    
    public ArEntry(String name) {
        this();
        if (name.endsWith("/")) {
        	throw new IllegalArgumentException("ar archives can only contain files");
        }
        this.filename = new StringBuffer(name);
        this.mode = DEFAULT_FILE_MODE;
        this.userId = 0;
        this.groupId = 0;
        this.size = 0;
        this.fileDate = (new Date()).getTime() / MILLIS_PER_SECOND;
    }

    
    public ArEntry(File file) {
        this();
        if (file.isDirectory()) {
        	throw new IllegalArgumentException("ar archives can only contain files");
        }
        this.file = file;
        this.filename = new StringBuffer(file.getName());
        this.fileDate = file.lastModified() / MILLIS_PER_SECOND;
        this.mode = DEFAULT_FILE_MODE;
        this.size = file.length();
    }

    
    public ArEntry(byte[] headerBuf) {
        this();
        this.parseArHeader(headerBuf);
    }

    
    public boolean equals(ArEntry it) {
        return this.getFilename().equals(it.getFilename());
    }

    
    public boolean equals(Object it) {
        if (it == null || getClass() != it.getClass()) {
            return false;
        }
        return equals((ArEntry) it);
    }

    
    public int hashCode() {
        return getFilename().hashCode();
    }

    
    public String getFilename() {
        return this.filename.toString();
    }

    
    public void setFilename(String filename) {
        this.filename = new StringBuffer(filename);
    }

    
    public void setMode(int mode) {
        this.mode = mode;
    }

    
    public int getUserId() {
        return this.userId;
    }

    
    public void setUserId(int userId) {
        this.userId = userId;
    }

    
    public int getGroupId() {
        return this.groupId;
    }

    
    public void setGroupId(int groupId) {
        this.groupId = groupId;
    }

    
    public void setIds(int userId, int groupId) {
        this.setUserId(userId);
        this.setGroupId(groupId);
    }

    
    public void setFileDate(long time) {
        this.fileDate = time / MILLIS_PER_SECOND;
    }

    
    public void setFileDate(Date time) {
        this.fileDate = time.getTime() / MILLIS_PER_SECOND;
    }

    
    public Date getFileDate() {
        return new Date(this.fileDate * MILLIS_PER_SECOND);
    }

    
    public File getFile() {
        return this.file;
    }

    
    public int getMode() {
        return this.mode;
    }

    
    public long getSize() {
        return this.size;
    }

    
    public void setSize(long size) {
        this.size = size;
    }

    
    public void writeEntryHeader(byte[] outbuf) {
        int offset = 0;

        offset = ArUtils.getNameBytes(this.filename, outbuf, offset, NAMELEN);
        offset = ArUtils.getLongBytes(this.fileDate, outbuf, offset, FILEDATELEN);
        offset = ArUtils.getIntegerBytes(this.userId, outbuf, offset, UIDLEN);
        offset = ArUtils.getIntegerBytes(this.groupId, outbuf, offset, GIDLEN);
        offset = ArUtils.getOctalBytes(this.mode, outbuf, offset, MODELEN);
        offset = ArUtils.getLongBytes(this.size, outbuf, offset, SIZELEN);
        offset = ArUtils.getNameBytes(this.magic, outbuf, offset, MAGICLEN);

        while (offset < outbuf.length) {
            outbuf[offset++] = 0;
        }
    }

    
    public void parseArHeader(byte[] header) {
        throw new UnsupportedOperationException("parseArHeader(byte[]) not yet implmented");
















    }
}
