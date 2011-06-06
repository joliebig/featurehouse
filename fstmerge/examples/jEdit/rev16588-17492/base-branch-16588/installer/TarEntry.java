

package installer;

import java.io.*;
import java.util.Date;




public
class		TarEntry
extends		Object
	{
	
	protected File				file;

	
	protected TarHeader			header;

	
	public
	TarEntry( String name )
		{
		this.initialize();
		this.nameTarHeader( this.header, name );
		}

	
	public
	TarEntry( File file )
		throws InvalidHeaderException
		{
		this.initialize();
		this.getFileTarHeader( this.header, file );
		}

	
	public
	TarEntry( byte[] headerBuf )
		throws InvalidHeaderException
		{
		this.initialize();
		this.parseTarHeader( this.header, headerBuf );
		}

	
	private void
	initialize()
		{
		this.file = null;
		this.header = new TarHeader();
		}

	
	public boolean
	equals( TarEntry it )
		{
		return
			this.header.name.toString().equals
				( it.header.name.toString() );
		}

	
	public boolean
	isDescendent( TarEntry desc )
		{
		return
			desc.header.name.toString().startsWith
				( this.header.name.toString() );
		}

	
	public TarHeader
	getHeader()
		{
		return this.header;
		}

	
	public String
	getName()
		{
		return this.header.name.toString();
		}

	
	public void
	setName( String name )
		{
		this.header.name =
			new StringBuffer( name );
		}

	
	public int
	getUserId()
		{
		return this.header.userId;
		}

	
	public void
	setUserId( int userId )
		{
		this.header.userId = userId;
		}

	
	public int
	getGroupId()
		{
		return this.header.groupId;
		}

	
	public void
	setGroupId( int groupId )
		{
		this.header.groupId = groupId;
		}

	
	public String
	getUserName()
		{
		return this.header.userName.toString();
		}

	
	public void
	setUserName( String userName )
		{
		this.header.userName =
			new StringBuffer( userName );
		}

	
	public String
	getGroupName()
		{
		return this.header.groupName.toString();
		}

	
	public void
	setGroupName( String groupName )
		{
		this.header.groupName =
			new StringBuffer( groupName );
		}

	
	public void
	setIds( int userId, int groupId )
		{
		this.setUserId( userId );
		this.setGroupId( groupId );
		}

	
	public void
	setNames( String userName, String groupName )
		{
		this.setUserName( userName );
		this.setGroupName( groupName );
		}

	
	public void
	setModTime( long time )
		{
		this.header.modTime = time / 1000;
		}

	
	public void
	setModTime( Date time )
		{
		this.header.modTime = time.getTime() / 1000;
		}

	
	public Date
	getModTime()
		{
		return new Date( this.header.modTime * 1000 );
		}

	
	public File
	getFile()
		{
		return this.file;
		}

	
	public long
	getSize()
		{
		return this.header.size;
		}

	
	public void
	setSize( long size )
		{
		this.header.size = size;
		}

	
	public void
	adjustEntryName( byte[] outbuf, String newName )
		{
		int offset = 0;
		offset = TarHeader.getNameBytes
			( new StringBuffer( newName ),
				outbuf, offset, TarHeader.NAMELEN );
		}

	
	public boolean
	isDirectory()
		{
		if ( this.file != null )
			return this.file.isDirectory();

		if ( this.header != null )
			{
			if ( this.header.linkFlag == TarHeader.LF_DIR )
				return true;

			if ( this.header.name.toString().endsWith( "/" ) )
				return true;
			}

		return false;
		}

	
	public void
	getFileTarHeader( TarHeader hdr, File file )
		throws InvalidHeaderException
		{
		this.file = file;

		String name = file.getPath();
		String osname = System.getProperty( "os.name" );
		if ( osname != null )
			{
			
			

			
			
			

			

			
			String Win32Prefix = "windows";
			if ( osname.toLowerCase().startsWith( Win32Prefix ) )
				{
				if ( name.length() > 2 )
					{
					char ch1 = name.charAt(0);
					char ch2 = name.charAt(1);
					if ( ch2 == ':'
						&& ( (ch1 >= 'a' && ch1 <= 'z')
							|| (ch1 >= 'A' && ch1 <= 'Z') ) )
						{
						name = name.substring( 2 );
						}
					}
				}
			}

		name = name.replace( File.separatorChar, '/' );

		
		
		
		
		for ( ; name.startsWith( "/" ) ; )
			name = name.substring( 1 );

 		hdr.linkName = new StringBuffer( "" );

		hdr.name = new StringBuffer( name );

		if ( file.isDirectory() )
			{
			hdr.mode = 040755;
			hdr.linkFlag = TarHeader.LF_DIR;
			if ( hdr.name.charAt( hdr.name.length() - 1 ) != '/' )
				hdr.name.append( "/" );
			}
		else
			{
			hdr.mode = 0100644;
			hdr.linkFlag = TarHeader.LF_NORMAL;
			}

		

		hdr.size = file.length();
		hdr.modTime = file.lastModified() / 1000;
		hdr.checkSum = 0;
		hdr.devMajor = 0;
		hdr.devMinor = 0;
		}

	
	public TarEntry[]
	getDirectoryEntries()
		throws InvalidHeaderException
		{
		if ( this.file == null
				|| ! this.file.isDirectory() )
			{
			return new TarEntry[0];
			}

		String[] list = this.file.list();

		TarEntry[] result = new TarEntry[ list.length ];

		for ( int i = 0 ; i < list.length ; ++i )
			{
			result[i] =
				new TarEntry
					( new File( this.file, list[i] ) );
			}

		return result;
		}

	
	public long
	computeCheckSum( byte[] buf )
		{
		long sum = 0;

		for ( int i = 0 ; i < buf.length ; ++i )
			{
			sum += 255 & buf[ i ];
			}

		return sum;
		}

	
	public void
	writeEntryHeader( byte[] outbuf )
		{
		int offset = 0;

		offset = TarHeader.getNameBytes
			( this.header.name, outbuf, offset, TarHeader.NAMELEN );

		offset = TarHeader.getOctalBytes
			( this.header.mode, outbuf, offset, TarHeader.MODELEN );

		offset = TarHeader.getOctalBytes
			( this.header.userId, outbuf, offset, TarHeader.UIDLEN );

		offset = TarHeader.getOctalBytes
			( this.header.groupId, outbuf, offset, TarHeader.GIDLEN );

		long size = this.header.size;

		offset = TarHeader.getLongOctalBytes
			( size, outbuf, offset, TarHeader.SIZELEN );

		offset = TarHeader.getLongOctalBytes
			( this.header.modTime, outbuf, offset, TarHeader.MODTIMELEN );

		int csOffset = offset;
		for ( int c = 0 ; c < TarHeader.CHKSUMLEN ; ++c )
			outbuf[ offset++ ] = (byte) ' ';

		outbuf[ offset++ ] = this.header.linkFlag;

		offset = TarHeader.getNameBytes
			( this.header.linkName, outbuf, offset, TarHeader.NAMELEN );

		offset = TarHeader.getNameBytes
			( this.header.magic, outbuf, offset, TarHeader.MAGICLEN );

		offset = TarHeader.getNameBytes
			( this.header.userName, outbuf, offset, TarHeader.UNAMELEN );

		offset = TarHeader.getNameBytes
			( this.header.groupName, outbuf, offset, TarHeader.GNAMELEN );

		offset = TarHeader.getOctalBytes
			( this.header.devMajor, outbuf, offset, TarHeader.DEVLEN );

		offset = TarHeader.getOctalBytes
			( this.header.devMinor, outbuf, offset, TarHeader.DEVLEN );

		for ( ; offset < outbuf.length ; )
			outbuf[ offset++ ] = 0;

		long checkSum = this.computeCheckSum( outbuf );

		TarHeader.getCheckSumOctalBytes
			( checkSum, outbuf, csOffset, TarHeader.CHKSUMLEN );
		}

	
	public void
	parseTarHeader( TarHeader hdr, byte[] header )
		throws InvalidHeaderException
		{
		int offset = 0;

		hdr.name =
			TarHeader.parseName( header, offset, TarHeader.NAMELEN );

		offset += TarHeader.NAMELEN;

		hdr.mode = (int)
			TarHeader.parseOctal( header, offset, TarHeader.MODELEN );

		offset += TarHeader.MODELEN;

		hdr.userId = (int)
			TarHeader.parseOctal( header, offset, TarHeader.UIDLEN );

		offset += TarHeader.UIDLEN;

		hdr.groupId = (int)
			TarHeader.parseOctal( header, offset, TarHeader.GIDLEN );

		offset += TarHeader.GIDLEN;

		hdr.size =
			TarHeader.parseOctal( header, offset, TarHeader.SIZELEN );

		offset += TarHeader.SIZELEN;

		hdr.modTime =
			TarHeader.parseOctal( header, offset, TarHeader.MODTIMELEN );

		offset += TarHeader.MODTIMELEN;

		hdr.checkSum = (int)
			TarHeader.parseOctal( header, offset, TarHeader.CHKSUMLEN );

		offset += TarHeader.CHKSUMLEN;

		hdr.linkFlag = header[ offset++ ];

		hdr.linkName =
			TarHeader.parseName( header, offset, TarHeader.NAMELEN );

		offset += TarHeader.NAMELEN;

		hdr.magic =
			TarHeader.parseName( header, offset, TarHeader.MAGICLEN );

		offset += TarHeader.MAGICLEN;

		hdr.userName =
			TarHeader.parseName( header, offset, TarHeader.UNAMELEN );

		offset += TarHeader.UNAMELEN;

		hdr.groupName =
			TarHeader.parseName( header, offset, TarHeader.GNAMELEN );

		offset += TarHeader.GNAMELEN;

		hdr.devMajor = (int)
			TarHeader.parseOctal( header, offset, TarHeader.DEVLEN );

		offset += TarHeader.DEVLEN;

		hdr.devMinor = (int)
			TarHeader.parseOctal( header, offset, TarHeader.DEVLEN );
		}

	
	public void
	nameTarHeader( TarHeader hdr, String name )
		{
		boolean isDir = name.endsWith( "/" );

		hdr.checkSum = 0;
		hdr.devMajor = 0;
		hdr.devMinor = 0;

		hdr.name = new StringBuffer( name );
		hdr.mode = isDir ? 040755 : 0100644;
		hdr.userId = 0;
		hdr.groupId = 0;
		hdr.size = 0;
		hdr.checkSum = 0;

		hdr.modTime =
			(new java.util.Date()).getTime() / 1000;

		hdr.linkFlag =
			isDir ? TarHeader.LF_DIR : TarHeader.LF_NORMAL;

		hdr.linkName = new StringBuffer( "" );
		hdr.userName = new StringBuffer( "" );
		hdr.groupName = new StringBuffer( "" );

		hdr.devMajor = 0;
		hdr.devMinor = 0;
		}

	}

