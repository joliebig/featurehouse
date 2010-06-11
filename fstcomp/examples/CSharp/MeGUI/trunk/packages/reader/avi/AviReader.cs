// ****************************************************************************
// 
// Copyright (C) 2004-2005  Moitah (moitah@yahoo.com)
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// 
// ****************************************************************************

using System;
using System.Runtime.InteropServices;
using System.Text;
using System.Drawing;
using System.IO;

namespace MeGUI
{

    //#region helper structures
    //[StructLayout(LayoutKind.Sequential)]
    //public struct AVISTREAMINFO 
    //{ 
    //    public uint fccType;
    //    public uint fccHandler;
    //    public uint dwFlags;
    //    public uint dwCaps;
    //    public ushort wPriority;
    //    public ushort wLanguage;
    //    public uint dwScale;
    //    public uint dwRate;
    //    public uint dwStart;
    //    public uint dwLength;
    //    public uint dwInitialFrames;
    //    public uint dwSuggestedBufferSize;
    //    public uint dwQuality;
    //    public uint dwSampleSize;
    //    public RECT rcFrame;
    //    public uint dwEditCount;
    //    public uint dwFormatChangeCount;
    //    [MarshalAs(UnmanagedType.ByValArray, SizeConst=64)]
    //    public byte[] szName;
    //}

    //[StructLayout(LayoutKind.Sequential)]
    //public struct RECT 
    //{ 
    //    public int left;
    //    public int top;
    //    public int right;
    //    public int bottom;
    //}

    //[StructLayout(LayoutKind.Sequential)]
    //public struct BITMAPINFOHEADER 
    //{
    //    public uint biSize;
    //    public int biWidth;
    //    public int biHeight;
    //    public ushort biPlanes;
    //    public ushort biBitCount;
    //    public uint biCompression;
    //    public uint biSizeImage;
    //    public int biXPelsPerMeter;
    //    public int biYPelsPerMeter;
    //    public uint biClrUsed;
    //    public uint biClrImportant;
    //}

    //[StructLayout(LayoutKind.Sequential, Pack=1)]
    //public struct BITMAPFILEHEADER 
    //{ 
    //    public ushort bfType;
    //    public uint bfSize;
    //    public ushort bfReserved1;
    //    public ushort bfReserved2;
    //    public uint bfOffBits;
    //}
    //#endregion
    //[Obsolete("TODO: use AvsReader instead")]
    //public class AVIReader : VideoReader, IDisposable 
    //{
    //    #region legacy dll imports and helper methods
    //    [DllImport("avifil32.dll")]
    //    protected static extern void AVIFileInit();

    //    [DllImport("avifil32.dll")]
    //    protected static extern void AVIFileExit();

    //    [DllImport("avifil32.dll")]
    //    protected static extern int AVIFileOpen(out int ppfile, string szFile, uint mode, int pclsidHandler);

    //    [DllImport("avifil32.dll")]
    //    protected static extern uint AVIFileRelease(int pfile);

    //    [DllImport("avifil32.dll")]
    //    protected static extern int AVIFileGetStream(int pfile, out int ppavi, uint fccType, int lParam);

    //    [DllImport("avifil32.dll")]
    //    protected static extern int AVIStreamRelease(int pavi);

    //    [DllImport("avifil32.dll")]
    //    protected static extern int AVIStreamInfo(int pavi, out AVISTREAMINFO psi, int lSize);

    //    [DllImport("avifil32.dll")]
    //    protected static extern int AVIStreamGetFrameOpen(int pavi, ref BITMAPINFOHEADER lpbiWanted);

    //    [DllImport("avifil32.dll")]
    //    protected static extern int AVIStreamGetFrameOpen(int pavi, int lpbiWanted);

    //    [DllImport("avifil32.dll")]
    //    protected static extern int AVIStreamGetFrameClose(int pget);

    //    [DllImport("avifil32.dll")]
    //    protected static extern int AVIStreamGetFrame(int pgf, int lPos);

    //    protected const int OF_SHARE_DENY_WRITE = 0x20;
    //    protected const uint BI_RGB = 0;
    //    public static uint ConvertFourCC(string fourCC) 
    //    {
    //        if (fourCC.Length != 4) 
    //        {
    //            throw new Exception();
    //        }

    //        return BitConverter.ToUInt32(Encoding.ASCII.GetBytes(fourCC), 0);
    //    }

    //    public static string ConvertFourCC(uint fourCC) 
    //    {
    //        return Encoding.ASCII.GetString(BitConverter.GetBytes(fourCC));
    //    }
    //    #endregion

    //    private bool m_isDisposed = false;

    //    private string m_path;
    //    private int m_pAVIFile, m_pVideoStream, m_pGetFrame;
    //    private uint m_fourCC;
    //    private int m_frameCount;
    //    private int m_width, m_height;
    //    private double framerate; // this was missing from the original implementation

    //    #region constructor
    //    public AVIReader(string path) 
    //    {
    //        AVISTREAMINFO streamInfo = new AVISTREAMINFO();
    //        BITMAPINFOHEADER bihDesired = new BITMAPINFOHEADER();

    //        m_path = path;
    //        AVIFileInit();

    //        if (AVIFileOpen(out m_pAVIFile, path, OF_SHARE_DENY_WRITE, 0) != 0) 
    //        {
    //            m_pAVIFile = 0;
    //            throw new Exception("AVIFileOpen failed");
    //        }

    //        if (AVIFileGetStream(m_pAVIFile, out m_pVideoStream, ConvertFourCC("vids"), 0) != 0) 
    //        {
    //            m_pVideoStream = 0;
    //            throw new Exception("AVIFileGetStream failed");
    //        }

    //        if (AVIStreamInfo(m_pVideoStream, out streamInfo, Marshal.SizeOf(streamInfo)) != 0) 
    //        {
    //            throw new Exception("AVIStreamInfo failed");
    //        }

    //        m_fourCC = streamInfo.fccHandler;
    //        m_frameCount = (int)streamInfo.dwLength;
    //        m_width = streamInfo.rcFrame.right;
    //        m_height = streamInfo.rcFrame.bottom;
    //        framerate = (double)streamInfo.dwRate/(double)streamInfo.dwScale; // per the AVIFile API, dwRate/dwScale = framerate

    //        bihDesired.biSize = (uint)Marshal.SizeOf(bihDesired);
    //        bihDesired.biBitCount = 24;
    //        bihDesired.biCompression = BI_RGB;
    //        if (m_fourCC == ConvertFourCC("YV12"))
    //            m_pGetFrame = AVIStreamGetFrameOpen(m_pVideoStream, ref bihDesired);
    //        else
    //            m_pGetFrame = AVIStreamGetFrameOpen(m_pVideoStream, 0);
    //        if (m_pGetFrame == 0) 
    //        {
    //            throw new Exception("AVIStreamGetFrameOpen failed");
    //        }
    //    }
    //    #endregion
    //    #region deconstruction
    //    ~AVIReader() 
    //    {
    //        Close();
    //    }


    //    public override void Close() 
    //    {
    //        if (m_isDisposed) return;

    //        if (m_pGetFrame != 0)
    //        {
    //            AVIStreamGetFrameClose(m_pGetFrame);
    //            m_pGetFrame = 0;
    //        }
    //        if (m_pVideoStream != 0)
    //        {
    //            AVIStreamRelease(m_pVideoStream);
    //            m_pVideoStream = 0;
    //        }
    //        if (m_pAVIFile != 0)
    //        {
    //            AVIFileRelease(m_pAVIFile);
    //            m_pAVIFile = 0;
    //        }
    //        AVIFileExit();

    //        m_isDisposed = true;
    //        GC.SuppressFinalize(this);
    //    }
    //    #endregion
    //    #region properties
    //    public string Path 
    //    {
    //        get 
    //        {
    //            return m_path;
    //        }
    //    }

    //    public uint FourCC 
    //    {
    //        get 
    //        {
    //            return m_fourCC;
    //        }
    //    }

    //    public override int FrameCount 
    //    {
    //        get 
    //        {
    //            return m_frameCount;
    //        }
    //    }

    //    public override int Width 
    //    {
    //        get 
    //        {
    //            return m_width;
    //        }
    //    }

    //    public override int Height 
    //    {
    //        get 
    //        {
    //            return m_height;
    //        }
    //    }
    //    public override double Framerate 
    //    {
    //        get 
    //        {
    //            return framerate;
    //        }
    //    }
    //    #endregion
    //    #region bitmaps
    //    public void AVIStreamGetFrame(int pos)
    //    {
    //        int i = AVIStreamGetFrame(m_pGetFrame, pos);
    //    }

    //    public byte[] ReadFrame(int frame) 
    //    {
    //        int pDIB;

    //        if ((frame < 0) || (frame >= m_frameCount)) 
    //        {
    //            throw new Exception("Frame number is out of range");
    //        }
		
    //        pDIB = AVIStreamGetFrame(m_pGetFrame, frame);
    //        if (pDIB == 0) 
    //        {
    //            throw new Exception("AVIStreamGetFrame failed");
    //        }

    //        return PackedDIBToBMPBytes((IntPtr)pDIB);
    //    }

    //    public override Bitmap ReadFrameBitmap(int frame) 
    //    {
    //        return new Bitmap(new MemoryStream(ReadFrame(frame)));
    //    }
    //    private byte[] PackedDIBToBMPBytes(IntPtr pDIB) 
    //    {
    //        int fhSize, ihSize, dataSize, bmpSize;
    //        byte[] bmpBytes, fhType, fhFileSize, fhDataOffset;

    //        fhSize = 14;
    //        ihSize = Marshal.ReadInt32(pDIB, 0); // biSize
    //        dataSize = Marshal.ReadInt32(pDIB, 20); // biSizeImage

    //        bmpSize = fhSize + ihSize + dataSize;
    //        bmpBytes = new byte[bmpSize];

    //        // Build the file header
    //        fhType = BitConverter.GetBytes( (ushort)0x4D42 );
    //        fhFileSize = BitConverter.GetBytes( (uint)bmpSize );
    //        fhDataOffset = BitConverter.GetBytes( (uint)(fhSize + ihSize) );

    //        // Write the file header
    //        Array.Copy(fhType, 0, bmpBytes, 0, fhType.Length);
    //        Array.Copy(fhFileSize, 0, bmpBytes, 2, fhFileSize.Length);
    //        Array.Copy(fhDataOffset, 0, bmpBytes, 10, fhDataOffset.Length);

    //        // Copy the info header and data
    //        Marshal.Copy(pDIB, bmpBytes, fhSize, bmpBytes.Length - fhSize);

    //        return bmpBytes;
    //    }
    //    #endregion
    //}

}