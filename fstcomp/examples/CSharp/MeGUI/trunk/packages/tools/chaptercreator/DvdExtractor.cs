using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics;

using MeGUI.core.util;

namespace MeGUI
{
  public class DvdExtractor : ChapterExtractor
  {
    public override string[] Extensions
    {
      get { return new string[] { }; }
    }

    public override List<ChapterInfo> GetStreams(string location)
    {
      string path = Path.Combine(location, "VIDEO_TS");

      if (!Directory.Exists(path))
        throw new FileNotFoundException("The VIDEO_TS folder was not found on the DVD.");

      List<ChapterInfo> streams = new List<ChapterInfo>();

      IfoExtractor ex = new IfoExtractor();
      ex.StreamDetected += (sender, args) => OnStreamDetected(args.ProgramChain);
      ex.ChaptersLoaded += (sender, args) => OnChaptersLoaded(args.ProgramChain);


      string videoIFO = Path.Combine(path, "VIDEO_TS.IFO");
      if (File.Exists(videoIFO))
      {
        byte[] bytRead = new byte[4];
        long VMG_PTT_STPT_Position = IFOparser.ToFilePosition(IFOparser.GetFileBlock(videoIFO, 0xC4, 4));
        int titlePlayMaps = IFOparser.ToInt16(IFOparser.GetFileBlock(videoIFO, VMG_PTT_STPT_Position, 2));
        //string longestIfo = GetLongestIFO(videoTSDir);
        for (int currentTitle = 1; currentTitle <= titlePlayMaps; ++currentTitle)
        {
          long titleInfoStart = 8 + ((currentTitle - 1) * 12);
          int titleSetNumber = IFOparser.GetFileBlock(videoIFO, (VMG_PTT_STPT_Position + titleInfoStart) + 6L, 1)[0];
          int titleSetTitleNumber = IFOparser.GetFileBlock(videoIFO, (VMG_PTT_STPT_Position + titleInfoStart) + 7L, 1)[0];
          string vtsIFO = Path.Combine(path, string.Format("VTS_{0:D2}_0.IFO", titleSetNumber));
          if (!File.Exists(vtsIFO))
          {
            Trace.WriteLine(string.Format("VTS IFO file missing: {0}", Path.GetFileName(vtsIFO)));
            continue;
          }
          streams.Add(ex.GetStreams(vtsIFO)[0]);
        }
      }
      else
      {
        Trace.WriteLine("VIDEO_TS.IFO file is missing missing on the DVD.");
        int count = 1;
        //read all the ifo files
        foreach (string file in Directory.GetFiles(path, "VTS_*_0.IFO"))
        {
          ChapterInfo pgc = ex.GetStreams(file)[0];
          pgc.SourceName = "PGC " + count.ToString("D2");//Path.GetFileNameWithoutExtension(file);
          streams.Add(pgc);
          count++;
        }
      }

      streams = streams.OrderByDescending(p => p.Duration).ToList();

      OnExtractionComplete();
      return streams;
    }
  }
}
