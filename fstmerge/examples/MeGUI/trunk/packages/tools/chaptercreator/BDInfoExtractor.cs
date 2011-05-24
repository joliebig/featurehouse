// ****************************************************************************
// 
// Copyright (C) 2009  Jarrett Vance
// 
// code from http://jvance.com/pages/ChapterGrabber.xhtml
// 
// ****************************************************************************

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics;
using BDInfo;

namespace MeGUI
{
  public class BDInfoExtractor : ChapterExtractor
  {
    public override string[] Extensions
    {
      get { return new string[] { "mpls" }; }
    }

    public override bool SupportsMultipleStreams
    {
      get { return false; }
    }

    public override List<ChapterInfo> GetStreams(string location)
    {
      ChapterInfo pgc = new ChapterInfo();
      pgc.Chapters = new List<Chapter>();
      pgc.SourceHash = ChapterExtractor.ComputeMD5Sum(location);
      pgc.SourceName = location;
      pgc.Title = Path.GetFileNameWithoutExtension(location);
      pgc.SourceType = "Blu-Ray";

      FileInfo fileInfo = new FileInfo(location);
      
      OnStreamDetected(pgc);
      TSPlaylistFile mpls = new TSPlaylistFile(fileInfo);
      //Dictionary<string, TSStreamClipFile> clips = new Dictionary<string,TSStreamClipFile>();
      mpls.Scan(); int count = 1;
      foreach (double d in mpls.Chapters)
      {
        pgc.Chapters.Add(new Chapter()
          {
            Name = "Chapter " + count.ToString("D2"),
            Time = new TimeSpan((long)(d * (double)TimeSpan.TicksPerSecond))
          });
        count++;
      }

      pgc.Duration = new TimeSpan((long)(mpls.TotalLength * (double)TimeSpan.TicksPerSecond));

      foreach (TSStreamClip clip in mpls.StreamClips)
      {
        clip.StreamClipFile.Scan();
        foreach (TSStream stream in clip.StreamClipFile.Streams.Values)
        {
          if (stream.IsVideoStream)
          {
            pgc.FramesPerSecond = (double)((TSVideoStream)stream).FrameRateEnumerator /
            (double)((TSVideoStream)stream).FrameRateDenominator;
            break;
          }
        }
        if (pgc.FramesPerSecond != 0) break;
      }

      OnChaptersLoaded(pgc);
      OnExtractionComplete();
      return new List<ChapterInfo>() { pgc };
    }
  }
}
