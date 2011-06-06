

package installer;



public class
TarHeader extends Object
	{
	
	public static final int		NAMELEN = 100;
	
	public static final int		MODELEN = 8;
	
	public static final int		UIDLEN = 8;
	
	public static final int		GIDLEN = 8;
	
	public static final int		CHKSUMLEN = 8;
	
	public static final int		SIZELEN = 12;
	
	public static final int		MAGICLEN = 8;
	
	public static final int		MODTIMELEN = 12;
	
	public static final int		UNAMELEN = 32;
	
	public static final int		GNAMELEN = 32;
	
	public static final int		DEVLEN = 8;

	
	public static final byte	LF_OLDNORM	= 0;
	
	public static final byte	LF_NORMAL	= (byte) '0';
	
	public static final byte	LF_LINK		= (byte) '1';
	
	public static final byte	LF_SYMLINK	= (byte) '2';
	
	public static final byte	LF_CHR		= (byte) '3';
	
	public static final byte	LF_BLK		= (byte) '4';
	
	public static final byte	LF_DIR		= (byte) '5';
	
	public static final byte	LF_FIFO		= (byte) '6';
	
	public static final byte	LF_CONTIG	= (byte) '7';

	
	public static final String	TMAGIC		= "ustar";

	
	public static final String	GNU_TMAGIC	= "ustar  ";

	
	public StringBuffer		name;
	
	public int				mode;
	
	public int				userId;
	
	public int				groupId;
	
	public long				size;
	
	public long				modTime;
	
	public int				checkSum;
	
	public byte				linkFlag;
	
	public StringBuffer		linkName;
	
	public StringBuffer		magic;
	
	public StringBuffer		userName;
	
	public StringBuffer		groupName;
	
	public int				devMajor;
	
	public int				devMinor;


	public
	TarHeader()
		{
		this.magic = new StringBuffer( TarHeader.TMAGIC );

		this.name = new StringBuffer();
		this.linkName = new StringBuffer();

		String user =
			System.getProperty( "user.name", "" );

		if ( user.length() > 31 )
			user = user.substring( 0, 31 );

		this.userId = 0;
		this.groupId = 0;
		this.userName = new StringBuffer( user );
		this.groupName = new StringBuffer( "" );
		}

	
	public Object
	clone()
		{
		TarHeader hdr = null;

		try {
			hdr = (TarHeader) super.clone();

			hdr.name =
				(this.name == null ) ? null
					: new StringBuffer( this.name.toString() );
			hdr.mode = this.mode;
			hdr.userId = this.userId;
			hdr.groupId = this.groupId;
			hdr.size = this.size;
			hdr.modTime = this.modTime;
			hdr.checkSum = this.checkSum;
			hdr.linkFlag = this.linkFlag;
			hdr.linkName =
				(this.linkName == null ) ? null
					: new StringBuffer( this.linkName.toString() );
			hdr.magic =
				(this.magic == null ) ? null
					: new StringBuffer( this.magic.toString() );
			hdr.userName =
				(this.userName == null ) ? null
					: new StringBuffer( this.userName.toString() );
			hdr.groupName =
				(this.groupName == null ) ? null
					: new StringBuffer( this.groupName.toString() );
			hdr.devMajor = this.devMajor;
			hdr.devMinor = this.devMinor;
			}
		catch ( CloneNotSupportedException ex )
			{
			ex.printStackTrace();
			}

		return hdr;
		}

	
	public String
	getName()
		{
		return this.name.toString();
		}

	
	public static long
	parseOctal( byte[] header, int offset, int length )
		throws InvalidHeaderException
		{
		long result = 0;
		boolean stillPadding = true;

		int end = offset + length;
		for ( int i = offset ; i < end ; ++i )
			{
			if ( header[i] == 0 )
				break;

			if ( header[i] == (byte) ' ' || header[i] == '0' )
				{
				if ( stillPadding )
					continue;

				if ( header[i] == (byte) ' ' )
					break;
				}
			
			stillPadding = false;

			result =
				(result << 3)
					+ (header[i] - '0');
			}

		return result;
		}

	
	public static StringBuffer
	parseName( byte[] header, int offset, int length )
		throws InvalidHeaderException
		{
		StringBuffer result = new StringBuffer( length );

		int end = offset + length;
		for ( int i = offset ; i < end ; ++i )
			{
			if ( header[i] == 0 )
				break;
			result.append( (char)header[i] );
			}

		return result;
		}

	
	public static int
	getNameBytes( StringBuffer name, byte[] buf, int offset, int length )
		{
		int i;

		for ( i = 0 ; i < length && i < name.length() ; ++i )
			{
			buf[ offset + i ] = (byte) name.charAt( i );
			}

		for ( ; i < length ; ++i )
			{
			buf[ offset + i ] = 0;
			}

		return offset + length;
		}

	
	public static int
	getOctalBytes( long value, byte[] buf, int offset, int length )
		{
		byte[] result = new byte[ length ];

		int idx = length - 1;

		buf[ offset + idx ] = 0;
		--idx;
		buf[ offset + idx ] = (byte) ' ';
		--idx;

		if ( value == 0 )
			{
			buf[ offset + idx ] = (byte) '0';
			--idx;
			}
		else
			{
			for ( long val = value ; idx >= 0 && val > 0 ; --idx )
				{
				buf[ offset + idx ] = (byte)
					( (byte) '0' + (byte) (val & 7) );
				val = val >> 3;
				}
			}

		for ( ; idx >= 0 ; --idx )
			{
			buf[ offset + idx ] = (byte) ' ';
			}

		return offset + length;
		}

	
	public static int
	getLongOctalBytes( long value, byte[] buf, int offset, int length )
		{
		byte[] temp = new byte[ length + 1 ];
		TarHeader.getOctalBytes( value, temp, 0, length + 1 );
		System.arraycopy( temp, 0, buf, offset, length );
		return offset + length;
		}

	
	public static int
	getCheckSumOctalBytes( long value, byte[] buf, int offset, int length )
		{
		TarHeader.getOctalBytes( value, buf, offset, length );
		buf[ offset + length - 1 ] = (byte) ' ';
		buf[ offset + length - 2 ] = 0;
		return offset + length;
		}

	}
 
