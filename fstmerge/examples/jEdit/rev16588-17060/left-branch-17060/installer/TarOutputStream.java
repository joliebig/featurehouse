

package installer;

import java.io.*;





public
class		TarOutputStream
extends		FilterOutputStream
	{
	protected boolean			debug;
	protected int				currSize;
	protected int				currBytes;
	protected byte[]			oneBuf;
	protected byte[]			recordBuf;
	protected int				assemLen;
	protected byte[]			assemBuf;
	protected TarBuffer			buffer;


	public
	TarOutputStream( OutputStream os )
		{
		this( os, TarBuffer.DEFAULT_BLKSIZE, TarBuffer.DEFAULT_RCDSIZE );
		}

	public
	TarOutputStream( OutputStream os, int blockSize )
		{
		this( os, blockSize, TarBuffer.DEFAULT_RCDSIZE );
		}

	public
	TarOutputStream( OutputStream os, int blockSize, int recordSize )
		{
		super( os );

		this.buffer = new TarBuffer( os, blockSize, recordSize );
		
		this.debug = false;
		this.assemLen = 0;
		this.assemBuf = new byte[ recordSize ];
		this.recordBuf = new byte[ recordSize ];
		this.oneBuf = new byte[1];
		}

	
	public void
	setDebug( boolean debugF )
		{
		this.debug = debugF;
		}

	
	public void
	setBufferDebug( boolean debug )
		{
		this.buffer.setDebug( debug );
		}

	

	public void
	finish()
		throws IOException
		{
		this.writeEOFRecord();
		}

	

	public void
	close()
		throws IOException
		{
		this.finish();
		this.buffer.close();
		}

	
	public int
	getRecordSize()
		{
		return this.buffer.getRecordSize();
		}

	
	public void
	putNextEntry( TarEntry entry )
		throws IOException
		{
		if ( entry.getHeader().name.length() > TarHeader.NAMELEN )
			throw new InvalidHeaderException
				( "file name '" + entry.getHeader().name
					+ "' is too long ( > "
					+ TarHeader.NAMELEN + " bytes )" );

		entry.writeEntryHeader( this.recordBuf );
		this.buffer.writeRecord( this.recordBuf );

		this.currBytes = 0;

		if ( entry.isDirectory() )
			this.currSize = 0;
		else
			this.currSize = (int)entry.getSize();
		}

	
	public void
	closeEntry()
		throws IOException
		{
		if ( this.assemLen > 0 )
			{
			for ( int i = this.assemLen ; i < this.assemBuf.length ; ++i )
				this.assemBuf[i] = 0;

			this.buffer.writeRecord( this.assemBuf );

			this.currBytes += this.assemLen;
			this.assemLen = 0;
			}

		if ( this.currBytes < this.currSize )
			throw new IOException
				( "entry closed at '" + this.currBytes
					+ "' before the '" + this.currSize
					+ "' bytes specified in the header were written" );
		}

	
	public void
	write( int b )
		throws IOException
		{
		this.oneBuf[0] = (byte) b;
		this.write( this.oneBuf, 0, 1 );
		}

	
	public void
	write( byte[] wBuf )
		throws IOException
		{
		this.write( wBuf, 0, wBuf.length );
		}

	
	public void
	write( byte[] wBuf, int wOffset, int numToWrite )
		throws IOException
		{
		if ( (this.currBytes + numToWrite) > this.currSize )
			throw new IOException
				( "request to write '" + numToWrite
					+ "' bytes exceeds size in header of '"
					+ this.currSize + "' bytes" );

		
		
		
		
		
		
		
		if ( this.assemLen > 0 )
			{
			if ( (this.assemLen + numToWrite ) >= this.recordBuf.length )
				{
				int aLen = this.recordBuf.length - this.assemLen;

				System.arraycopy
					( this.assemBuf, 0, this.recordBuf, 0, this.assemLen );

				System.arraycopy
					( wBuf, wOffset, this.recordBuf, this.assemLen, aLen );

				this.buffer.writeRecord( this.recordBuf );

				this.currBytes += this.recordBuf.length;

				wOffset += aLen;
				numToWrite -= aLen;
				this.assemLen = 0;
				}
			else 
				{
				System.arraycopy
					( wBuf, wOffset, this.assemBuf,
						this.assemLen, numToWrite );
				wOffset += numToWrite;
				this.assemLen += numToWrite; 
				numToWrite -= numToWrite;
				}
			}

		
		
		
		
		

		for ( ; numToWrite > 0 ; )
			{
			if ( numToWrite < this.recordBuf.length )
				{
				System.arraycopy
					( wBuf, wOffset, this.assemBuf, this.assemLen, numToWrite );
				this.assemLen += numToWrite;
				break;
				}

			this.buffer.writeRecord( wBuf, wOffset );

			int num = this.recordBuf.length;
			this.currBytes += num;
			numToWrite -= num;
			wOffset += num;
			}
		}

	
	private void
	writeEOFRecord()
		throws IOException
		{
		for ( int i = 0 ; i < this.recordBuf.length ; ++i )
			this.recordBuf[i] = 0;
		this.buffer.writeRecord( this.recordBuf );
		}

	}

