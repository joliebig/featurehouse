// ****************************************************************************
// 
// Copyright (C) 2005-2009  Doom9 & al
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

namespace MeGUI
{


    [StructLayout(LayoutKind.Sequential), ComVisible(false)]
    public class DsOptInt64
    {
        public DsOptInt64(long Value)
        {
            this.Value = Value;
        }
        public long Value;
    }

    [StructLayout(LayoutKind.Sequential), ComVisible(false)]
    public class DsOptIntPtr
    {
        public IntPtr Pointer;
    }

    [ComVisible(false)]
    public enum PinDirection		// PIN_DIRECTION
    {
        Input,		// PINDIR_INPUT
        Output		// PINDIR_OUTPUT
    }

    [ComVisible(false)]
    public class DsHlp
    {
        public const int OATRUE = -1;
        public const int OAFALSE = 0;

        [DllImport("quartz.dll", CharSet = CharSet.Auto)]
        public static extern int AMGetErrorText(int hr, StringBuilder buf, int max);
    }


    [ComVisible(true), ComImport,
    Guid("56a86891-0ad4-11ce-b03a-0020af0ba770"),
    InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    public interface IPin
    {
        [PreserveSig]
        int Connect(
            [In]											IPin pReceivePin,
            [In, MarshalAs(UnmanagedType.LPStruct)]			AMMediaType pmt);

        [PreserveSig]
        int ReceiveConnection(
            [In]											IPin pReceivePin,
            [In, MarshalAs(UnmanagedType.LPStruct)]			AMMediaType pmt);

        [PreserveSig]
        int Disconnect();

        [PreserveSig]
        int ConnectedTo([Out] out IPin ppPin);

        [PreserveSig]
        int ConnectionMediaType(
            [Out, MarshalAs(UnmanagedType.LPStruct)]		AMMediaType pmt);

        [PreserveSig]
        int QueryPinInfo(IntPtr pInfo);

        [PreserveSig]
        int QueryDirection(out PinDirection pPinDir);

        [PreserveSig]
        int QueryId(
            [Out, MarshalAs(UnmanagedType.LPWStr)]		out	string Id);

        [PreserveSig]
        int QueryAccept(
            [In, MarshalAs(UnmanagedType.LPStruct)]			AMMediaType pmt);

        [PreserveSig]
        int EnumMediaTypes(IntPtr ppEnum);

        [PreserveSig]
        int QueryInternalConnections(IntPtr apPin, [In, Out] ref int nPin);

        [PreserveSig]
        int EndOfStream();

        [PreserveSig]
        int BeginFlush();

        [PreserveSig]
        int EndFlush();

        [PreserveSig]
        int NewSegment(long tStart, long tStop, double dRate);
    }

    [ComVisible(true), ComImport,
    Guid("56a8689f-0ad4-11ce-b03a-0020af0ba770"),
    InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    public interface IFilterGraph
    {
        [PreserveSig]
        int AddFilter(
            [In] IBaseFilter pFilter,
            [In, MarshalAs(UnmanagedType.LPWStr)]			string pName);

        [PreserveSig]
        int RemoveFilter([In] IBaseFilter pFilter);

        [PreserveSig]
        int EnumFilters([Out] out IEnumFilters ppEnum);

        [PreserveSig]
        int FindFilterByName(
            [In, MarshalAs(UnmanagedType.LPWStr)]			string pName,
            [Out]										out IBaseFilter ppFilter);

        [PreserveSig]
        int ConnectDirect([In] IPin ppinOut, [In] IPin ppinIn,
           [In, MarshalAs(UnmanagedType.LPStruct)]			AMMediaType pmt);

        [PreserveSig]
        int Reconnect([In] IPin ppin);

        [PreserveSig]
        int Disconnect([In] IPin ppin);

        [PreserveSig]
        int SetDefaultSyncSource();

    }

    // ---------------------------------------------------------------------------------------

    [ComVisible(true), ComImport,
    Guid("0000010c-0000-0000-C000-000000000046"),
    InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    public interface IPersist
    {
        [PreserveSig]
        int GetClassID(
            [Out]									out Guid pClassID);
    }

    // ---------------------------------------------------------------------------------------

    [ComVisible(true), ComImport,
    Guid("56a86899-0ad4-11ce-b03a-0020af0ba770"),
    InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    public interface IMediaFilter
    {

        [PreserveSig]
        int GetClassID(
            [Out]									out Guid pClassID);
        

        [PreserveSig]
        int Stop();

        [PreserveSig]
        int Pause();

        [PreserveSig]
        int Run(long tStart);

        [PreserveSig]
        int GetState(int dwMilliSecsTimeout, out int filtState);

        [PreserveSig]
        int SetSyncSource([In] IReferenceClock pClock);

        [PreserveSig]
        int GetSyncSource([Out] out IReferenceClock pClock);
    }

    // ---------------------------------------------------------------------------------------

    [ComVisible(true), ComImport,
    Guid("56a86895-0ad4-11ce-b03a-0020af0ba770"),
    InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    public interface IBaseFilter
    {

        [PreserveSig]
        int GetClassID(
            [Out]									out Guid pClassID);
        


        [PreserveSig]
        int Stop();

        [PreserveSig]
        int Pause();

        [PreserveSig]
        int Run(long tStart);

        [PreserveSig]
        int GetState(int dwMilliSecsTimeout, [Out] out int filtState);

        [PreserveSig]
        int SetSyncSource([In] IReferenceClock pClock);

        [PreserveSig]
        int GetSyncSource([Out] out IReferenceClock pClock);
        

        [PreserveSig]
        int EnumPins(
            [Out]										out IEnumPins ppEnum);

        [PreserveSig]
        int FindPin(
            [In, MarshalAs(UnmanagedType.LPWStr)]			string Id,
            [Out]										out IPin ppPin);

        [PreserveSig]
        int QueryFilterInfo(
            [Out]											FilterInfo pInfo);

        [PreserveSig]
        int JoinFilterGraph(
            [In]											IFilterGraph pGraph,
            [In, MarshalAs(UnmanagedType.LPWStr)]			string pName);

        [PreserveSig]
        int QueryVendorInfo(
            [Out, MarshalAs(UnmanagedType.LPWStr)]		out	string pVendorInfo);
    }


    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode), ComVisible(false)]
    public class FilterInfo		//  FILTER_INFO
    {
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 128)]
        public string achName;
        [MarshalAs(UnmanagedType.IUnknown)]
        public object pUnk;
    }

    // ---------------------------------------------------------------------------------------

    [ComVisible(true), ComImport,
    Guid("36b73880-c2c8-11cf-8b46-00805f6cef60"),
    InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    public interface IMediaSeeking
    {
        [PreserveSig]
        int GetCapabilities(out SeekingCapabilities pCapabilities);

        [PreserveSig]
        int CheckCapabilities([In, Out] ref SeekingCapabilities pCapabilities);

        [PreserveSig]
        int IsFormatSupported([In] ref Guid pFormat);
        [PreserveSig]
        int QueryPreferredFormat([Out] out Guid pFormat);

        [PreserveSig]
        int GetTimeFormat([Out] out Guid pFormat);
        [PreserveSig]
        int IsUsingTimeFormat([In] ref Guid pFormat);
        [PreserveSig]
        int SetTimeFormat([In] ref Guid pFormat);

        [PreserveSig]
        int GetDuration(out long pDuration);
        [PreserveSig]
        int GetStopPosition(out long pStop);
        [PreserveSig]
        int GetCurrentPosition(out long pCurrent);

        [PreserveSig]
        int ConvertTimeFormat(out long pTarget, [In] ref Guid pTargetFormat,
                                  long Source, [In] ref Guid pSourceFormat);

        [PreserveSig]
        int SetPositions(
            [In, Out, MarshalAs(UnmanagedType.LPStruct)]		DsOptInt64 pCurrent,
            SeekingFlags dwCurrentFlags,
            [In, Out, MarshalAs(UnmanagedType.LPStruct)]		DsOptInt64 pStop,
            SeekingFlags dwStopFlags);

        [PreserveSig]
        int GetPositions(out long pCurrent, out long pStop);

        [PreserveSig]
        int GetAvailable(out long pEarliest, out long pLatest);

        [PreserveSig]
        int SetRate(double dRate);
        [PreserveSig]
        int GetRate(out double pdRate);

        [PreserveSig]
        int GetPreroll(out long pllPreroll);
    }


    [Flags, ComVisible(false)]
    public enum SeekingCapabilities		// AM_SEEKING_SeekingCapabilities AM_SEEKING_SEEKING_CAPABILITIES
    {
        CanSeekAbsolute = 0x001,
        CanSeekForwards = 0x002,
        CanSeekBackwards = 0x004,
        CanGetCurrentPos = 0x008,
        CanGetStopPos = 0x010,
        CanGetDuration = 0x020,
        CanPlayBackwards = 0x040,
        CanDoSegments = 0x080,
        Source = 0x100		// Doesn't pass thru used to count segment ends
    }


    [Flags, ComVisible(false)]
    public enum SeekingFlags		// AM_SEEKING_SeekingFlags AM_SEEKING_SEEKING_FLAGS
    {
        NoPositioning = 0x00,		// No change
        AbsolutePositioning = 0x01,		// Position is supplied and is absolute
        RelativePositioning = 0x02,		// Position is supplied and is relative
        IncrementalPositioning = 0x03,		// (Stop) position relative to current, useful for seeking when paused (use +1)
        PositioningBitsMask = 0x03,		// Useful mask
        SeekToKeyFrame = 0x04,		// Just seek to key frame (performance gain)
        ReturnTime = 0x08,		// Plug the media time equivalents back into the supplied LONGLONGs
        Segment = 0x10,		// At end just do EC_ENDOFSEGMENT, don't do EndOfStream
        NoFlush = 0x20		// Don't flush
    }

    // ---------------------------------------------------------------------------------------

    [ComVisible(true), ComImport,
    Guid("56a86897-0ad4-11ce-b03a-0020af0ba770"),
    InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    public interface IReferenceClock
    {
        [PreserveSig]
        int GetTime(out long pTime);

        [PreserveSig]
        int AdviseTime(long baseTime, long streamTime, IntPtr hEvent, out int pdwAdviseCookie);

        [PreserveSig]
        int AdvisePeriodic(long startTime, long periodTime, IntPtr hSemaphore, out int pdwAdviseCookie);

        [PreserveSig]
        int Unadvise(int dwAdviseCookie);
    }

    // ---------------------------------------------------------------------------------------

    [ComVisible(true), ComImport,
    Guid("56a86893-0ad4-11ce-b03a-0020af0ba770"),
    InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    public interface IEnumFilters
    {
        [PreserveSig]
        int Next(
            [In]															int cFilters,
            [Out, MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]	out	IBaseFilter[] ppFilter,
            [Out]														out int pcFetched);

        [PreserveSig]
        int Skip([In] int cFilters);
        void Reset();
        void Clone([Out] out IEnumFilters ppEnum);
    }


    // ---------------------------------------------------------------------------------------

    [ComVisible(true), ComImport,
    Guid("56a86892-0ad4-11ce-b03a-0020af0ba770"),
    InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    public interface IEnumPins
    {
        [PreserveSig]
        int Next(
            [In]															int cPins,
            [Out, MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]	out	IPin[] ppPins,
            [Out]														out int pcFetched);

        [PreserveSig]
        int Skip([In] int cPins);
        void Reset();
        void Clone([Out] out IEnumPins ppEnum);
    }

    [StructLayout(LayoutKind.Sequential), ComVisible(false)]
    public class AMMediaType		//  AM_MEDIA_TYPE
    {
        public Guid majorType;
        public Guid subType;
        [MarshalAs(UnmanagedType.Bool)]
        public bool fixedSizeSamples;
        [MarshalAs(UnmanagedType.Bool)]
        public bool temporalCompression;
        public int sampleSize;
        public Guid formatType;
        public IntPtr unkPtr;
        public int formatSize;
        public IntPtr formatPtr;
    }

    // ---------------------------------------------------------------------------------------

    [ComVisible(true), ComImport,
    Guid("56a8689a-0ad4-11ce-b03a-0020af0ba770"),
    InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    public interface IMediaSample
    {
        [PreserveSig]
        int GetPointer(out IntPtr ppBuffer);
        [PreserveSig]
        int GetSize();

        [PreserveSig]
        int GetTime(out long pTimeStart, out long pTimeEnd);

        [PreserveSig]
        int SetTime(
            [In, MarshalAs(UnmanagedType.LPStruct)]			DsOptInt64 pTimeStart,
            [In, MarshalAs(UnmanagedType.LPStruct)]			DsOptInt64 pTimeEnd);

        [PreserveSig]
        int IsSyncPoint();
        [PreserveSig]
        int SetSyncPoint(
            [In, MarshalAs(UnmanagedType.Bool)]			bool bIsSyncPoint);

        [PreserveSig]
        int IsPreroll();
        [PreserveSig]
        int SetPreroll(
            [In, MarshalAs(UnmanagedType.Bool)]			bool bIsPreroll);

        [PreserveSig]
        int GetActualDataLength();
        [PreserveSig]
        int SetActualDataLength(int len);

        [PreserveSig]
        int GetMediaType(
            [Out, MarshalAs(UnmanagedType.LPStruct)]	out AMMediaType ppMediaType);

        [PreserveSig]
        int SetMediaType(
            [In, MarshalAs(UnmanagedType.LPStruct)]			AMMediaType pMediaType);

        [PreserveSig]
        int IsDiscontinuity();
        [PreserveSig]
        int SetDiscontinuity(
            [In, MarshalAs(UnmanagedType.Bool)]			bool bDiscontinuity);

        [PreserveSig]
        int GetMediaTime(out long pTimeStart, out long pTimeEnd);

        [PreserveSig]
        int SetMediaTime(
            [In, MarshalAs(UnmanagedType.LPStruct)]			DsOptInt64 pTimeStart,
            [In, MarshalAs(UnmanagedType.LPStruct)]			DsOptInt64 pTimeEnd);
    }
    

    [ComVisible(true), ComImport,
Guid("93E5A4E0-2D50-11d2-ABFA-00A0C9C6E38D"),
InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    public interface ICaptureGraphBuilder2
    {
        [PreserveSig]
        int SetFiltergraph([In] IGraphBuilder pfg);

        [PreserveSig]
        int GetFiltergraph([Out] out IGraphBuilder ppfg);

        [PreserveSig]
        int FindInterface(
            [In]											ref Guid pCategory,
            [In]											ref Guid pType,
            [In]											IBaseFilter pbf,
            [In]											ref Guid riid,
            [Out, MarshalAs(UnmanagedType.IUnknown)]		out	object ppint);

        [PreserveSig]
        int RenderStream(
            [In]										ref Guid pCategory,
            [In]										ref Guid pType,
            [In, MarshalAs(UnmanagedType.IUnknown)]			object pSource,
            [In]											IBaseFilter pfCompressor,
            [In]											IBaseFilter pfRenderer);

        [PreserveSig]
        int ControlStream(
            [In]										ref Guid pCategory,
            [In]										ref Guid pType,
            [In]											IBaseFilter pFilter,
            [In]											IntPtr pstart,
            [In]											IntPtr pstop,
            [In]											short wStartCookie,
            [In]											short wStopCookie);

        [PreserveSig]
        int AllocCapFile(
            [In, MarshalAs(UnmanagedType.LPWStr)]			string lpstrFile,
            [In]											long dwlSize);

        [PreserveSig]
        int FindPin(
            [In]											object pSource,
            [In]											int pindir,
            [In]										ref Guid pCategory,
            [In]										ref Guid pType,
            [In, MarshalAs(UnmanagedType.Bool)]			bool fUnconnected,
            [In]											int num,
            [Out]										out IPin ppPin);
    }

    [ComVisible(true), ComImport,
Guid("56a868a9-0ad4-11ce-b03a-0020af0ba770"),
InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    public interface IGraphBuilder
    {

        [PreserveSig]
        int AddFilter(
            [In] IBaseFilter pFilter,
            [In, MarshalAs(UnmanagedType.LPWStr)]			string pName);

        [PreserveSig]
        int RemoveFilter([In] IBaseFilter pFilter);

        [PreserveSig]
        int EnumFilters([Out] out IEnumFilters ppEnum);

        [PreserveSig]
        int FindFilterByName(
            [In, MarshalAs(UnmanagedType.LPWStr)]			string pName,
            [Out]										out IBaseFilter ppFilter);

        [PreserveSig]
        int ConnectDirect([In] IPin ppinOut, [In] IPin ppinIn,
           [In, MarshalAs(UnmanagedType.LPStruct)]			AMMediaType pmt);

        [PreserveSig]
        int Reconnect([In] IPin ppin);

        [PreserveSig]
        int Disconnect([In] IPin ppin);

        [PreserveSig]
        int SetDefaultSyncSource();
        

        [PreserveSig]
        int Connect([In] IPin ppinOut, [In] IPin ppinIn);

        [PreserveSig]
        int Render([In] IPin ppinOut);

        [PreserveSig]
        int RenderFile(
            [In, MarshalAs(UnmanagedType.LPWStr)]			string lpcwstrFile,
            [In, MarshalAs(UnmanagedType.LPWStr)]			string lpcwstrPlayList);

        [PreserveSig]
        int AddSourceFilter(
            [In, MarshalAs(UnmanagedType.LPWStr)]			string lpcwstrFileName,
            [In, MarshalAs(UnmanagedType.LPWStr)]			string lpcwstrFilterName,
            [Out]										out IBaseFilter ppFilter);

        [PreserveSig]
        int SetLogFile(IntPtr hFile);

        [PreserveSig]
        int Abort();

        [PreserveSig]
        int ShouldOperationContinue();
    }

    [ComVisible(false)]
    public class Clsid		// uuids.h  :  CLSID_*
    {
        /// <summary> CLSID_SystemDeviceEnum for ICreateDevEnum </summary>
        public static readonly Guid SystemDeviceEnum = new Guid(0x62BE5D10, 0x60EB, 0x11d0, 0xBD, 0x3B, 0x00, 0xA0, 0xC9, 0x11, 0xCE, 0x86);

        /// <summary> CLSID_FilterGraph, filter Graph </summary>
        public static readonly Guid FilterGraph = new Guid(0xe436ebb3, 0x524f, 0x11ce, 0x9f, 0x53, 0x00, 0x20, 0xaf, 0x0b, 0xa7, 0x70);

        /// <summary> CLSID_CaptureGraphBuilder2, new Capture graph building </summary>
        public static readonly Guid CaptureGraphBuilder2 = new Guid(0xBF87B6E1, 0x8C27, 0x11d0, 0xB3, 0xF0, 0x0, 0xAA, 0x00, 0x37, 0x61, 0xC5);

        /// <summary> CLSID_SampleGrabber, Sample Grabber filter </summary>
        public static readonly Guid SampleGrabber = new Guid(0xC1F400A0, 0x3F08, 0x11D3, 0x9F, 0x0B, 0x00, 0x60, 0x08, 0x03, 0x9E, 0x37);

        /// <summary> CLSID_DvdGraphBuilder,  DVD graph builder </summary>
        public static readonly Guid DvdGraphBuilder = new Guid(0xFCC152B7, 0xF372, 0x11d0, 0x8E, 0x00, 0x00, 0xC0, 0x4F, 0xD7, 0xC0, 0x8B);

    }

    public class DirectShow
    {
        private IGraphBuilder graphBuilder;

        /// <summary>
        /// Check whether DirectShow can render a video file
        /// </summary>
        /// <param name="fileName"></param>
        /// <returns>true if graphedit can render the input</returns>
        public bool checkRender(string fileName)
        {
            Type comtype = null;
            object comobj = null;
            try
            {
                comtype = Type.GetTypeFromCLSID(Clsid.FilterGraph);
                if (comtype == null)
                    throw new NotSupportedException("DirectX (8.1 or higher) not installed?");
                comobj = Activator.CreateInstance(comtype);
                graphBuilder = (IGraphBuilder)comobj; comobj = null;
                int hr = graphBuilder.RenderFile(fileName, null);
                if (hr >= 0)
                    return true;
                else
                    return false;
            }
            catch (Exception)
            {
                return false;
                //throw (e); // May add more handling here later
            }
        }
    }
}