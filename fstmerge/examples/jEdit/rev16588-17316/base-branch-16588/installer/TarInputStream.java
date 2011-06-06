

package installer;

import java.io.*;





public
class		TarInputStream
extends		FilterInputStream
	{
	protected boolean			debug;
	protected boolean			hasHitEOF;

	protected int				entrySize;
	protected int				entryOffset;

	protected byte[]			oneBuf;
	protected byte[]			readBuf;

	protected TarBuffer			buffer;

	protected TarEntry			currEntry;

	protected EntryFactory		eFactory;


	public
	TarInputStream( InputStream is )
		{
		this( is, TarBuffer.DEFAULT_BLKSIZE, TarBuffer.DEFAULT_RCDSIZE );
		}

	public
	TarInputStream( InputStream is, int blockSize )
		{
		this( is, blockSize, TarBuffer.DEFAULT_RCDSIZE );
		}

	public
	TarInputStream( InputStream is, int blockSize, int recordSize )
		{
		super( is );

		this.buffer = new TarBuffer( is, blockSize, recordSize );

		this.readBuf = null;
		this.oneBuf = new byte[1];
		this.debug = false;
		this.hasHitEOF = false;
		this.eFactory = null;
		}

	
	public void
	setDebug( boolean debugF )
		{
		this.debug = debugF;
		}

	
	public void
	setEntryFactory( EntryFactory factory )
		{
		this.eFactory = factory;
		}

	
	public void
	setBufferDebug( boolean debug )
		{
		this.buffer.setDebug( debug );
		}

	
	public void
	close()
		throws IOException
		{
		this.buffer.close();
		}

	
	public int
	getRecordSize()
		{
		return this.buffer.getRecordSize();
		}

	
	public int
	available()
		throws IOException
		{
		return this.entrySize - this.entryOffset;
		}

	
	public void
	skip( int numToSkip )
		throws IOException
		{
		
		
		
		

		byte[] skipBuf = new byte[ 8 * 1024 ];

		for ( int num = numToSkip ; num > 0 ; )
			{
			int numRead =
				this.read( skipBuf, 0,
					( num > skipBuf.length ? skipBuf.length : num ) );

			if ( numRead == -1 )
				break;

			num -= numRead;
			}
		}

	
	public boolean
	markSupported()
		{
		return false;
		}

	
	public void
	mark( int markLimit )
		{
		}

	
	public void
	reset()
		{
		}

	
	public TarEntry
	getNextEntry()
		throws IOException
		{
		if ( this.hasHitEOF )
			return null;

		if ( this.currEntry != null )
			{
			int numToSkip = this.entrySize - this.entryOffset;

			if ( this.debug )
			System.err.println
				( "TarInputStream: SKIP currENTRY '"
				+ this.currEntry.getName() + "' SZ "
				+ this.entrySize + " OFF " + this.entryOffset
				+ "  skipping " + numToSkip + " bytes" );

			if ( numToSkip > 0 )
				{
				this.skip( numToSkip );
				}

			this.readBuf = null;
			}

		byte[] headerBuf = this.buffer.readRecord();

		if ( headerBuf == null )
			{
			if ( this.debug )
				{
				System.err.println( "READ NULL RECORD" );
				}

			this.hasHitEOF = true;
			}
		else if ( this.buffer.isEOFRecord( headerBuf ) )
			{
			if ( this.debug )
				{
				System.err.println( "READ EOF RECORD" );
				}

			this.hasHitEOF = true;
			}

		if ( this.hasHitEOF )
			{
			this.currEntry = null;
			}
		else
			{
			try {
				if ( this.eFactory == null )
					{
					this.currEntry = new TarEntry( headerBuf );
					}
				else
					{
					this.currEntry =
						this.eFactory.createEntry( headerBuf );
					}

				if ( ! ( headerBuf[257] == 'u' && headerBuf[258] == 's'
						&& headerBuf[259] == 't' && headerBuf[260] == 'a'
						&& headerBuf[261] == 'r' ) )
					{
					throw new InvalidHeaderException
						( "header magic is not 'ustar', but '"
							+ headerBuf[257] + headerBuf[258] + headerBuf[259]
							+ headerBuf[260] + headerBuf[261] + "', or (dec) "
							+ ((int)headerBuf[257]) + ", "
							+ ((int)headerBuf[258]) + ", "
							+ ((int)headerBuf[259]) + ", "
							+ ((int)headerBuf[260]) + ", "
							+ ((int)headerBuf[261]) );
					}

				if ( this.debug )
				System.err.println
					( "TarInputStream: SET CURRENTRY '"
						+ this.currEntry.getName()
						+ "' size = " + this.currEntry.getSize() );

				this.entryOffset = 0;
				
				this.entrySize = (int) this.currEntry.getSize();
				}
			catch ( InvalidHeaderException ex )
				{
				this.entrySize = 0;
				this.entryOffset = 0;
				this.currEntry = null;
				throw new InvalidHeaderException
					( "bad header in block "
						+ this.buffer.getCurrentBlockNum()
						+ " record "
						+ this.buffer.getCurrentRecordNum()
						+ ", " + ex.getMessage() );
				}
			}

		return this.currEntry;
		}

	
	public int
	read()
		throws IOException
		{
		int num = this.read( this.oneBuf, 0, 1 );
		if ( num == -1 )
			return num;
		else
			return this.oneBuf[0];
		}

	
	public int
	read( byte[] buf )
		throws IOException
		{
		return this.read( buf, 0, buf.length );
		}

	
	public int
	read( byte[] buf, int offset, int numToRead )
		throws IOException
		{
		int totalRead = 0;

		if ( this.entryOffset >= this.entrySize )
			return -1;

		if ( (numToRead + this.entryOffset) > this.entrySize )
			{
			numToRead = (this.entrySize - this.entryOffset);
			}

		if ( this.readBuf != null )
			{
			int sz = ( numToRead > this.readBuf.length )
						? this.readBuf.length : numToRead;

			System.arraycopy( this.readBuf, 0, buf, offset, sz );

			if ( sz >= this.readBuf.length )
				{
				this.readBuf = null;
				}
			else
				{
				int newLen = this.readBuf.length - sz;
				byte[] newBuf = new byte[ newLen ];
				System.arraycopy( this.readBuf, sz, newBuf, 0, newLen );
				this.readBuf = newBuf;
				}

			totalRead += sz;
			numToRead -= sz;
			offset += sz;
			}

		for ( ; numToRead > 0 ; )
			{
			byte[] rec = this.buffer.readRecord();
			if ( rec == null )
				{
				
				throw new IOException
					( "unexpected EOF with " + numToRead + " bytes unread" );
				}

			int sz = numToRead;
			int recLen = rec.length;

			if ( recLen > sz )
				{
				System.arraycopy( rec, 0, buf, offset, sz );
				this.readBuf = new byte[ recLen - sz ];
				System.arraycopy( rec, sz, this.readBuf, 0, recLen - sz );
				}
			else
				{
				sz = recLen;
				System.arraycopy( rec, 0, buf, offset, recLen );
				}

			totalRead += sz;
			numToRead -= sz;
			offset += sz;
			}

		this.entryOffset += totalRead;

		return totalRead;
		}

	
	public void
	copyEntryContents( OutputStream out )
		throws IOException
		{
		byte[] buf = new byte[ 32 * 1024 ];

		for ( ; ; )
			{
			int numRead = this.read( buf, 0, buf.length );
			if ( numRead == -1 )
				break;
			out.write( buf, 0, numRead );
			}
		}

	

	public
	interface	EntryFactory
		{
		public TarEntry
			createEntry( String name );

		public TarEntry
			createEntry( File path )
				throws InvalidHeaderException;

		public TarEntry
			createEntry( byte[] headerBuf )
				throws InvalidHeaderException;
		}

	public
	class		EntryAdapter
	implements	EntryFactory
		{
		public TarEntry
		createEntry( String name )
			{
			return new TarEntry( name );
			}

		public TarEntry
		createEntry( File path )
			throws InvalidHeaderException
			{
			return new TarEntry( path );
			}

		public TarEntry
		createEntry( byte[] headerBuf )
			throws InvalidHeaderException
			{
			return new TarEntry( headerBuf );
			}
		}

	}


