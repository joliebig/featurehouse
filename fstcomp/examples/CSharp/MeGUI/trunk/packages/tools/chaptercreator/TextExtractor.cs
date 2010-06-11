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

namespace MeGUI
{
  public class TextExtractor : ChapterExtractor
  {
    public override bool SupportsMultipleStreams
    {
      get { return false; }
    }

    public override string[] Extensions
    {
      get { return new string[] { "txt" }; }
    }

    public override List<ChapterInfo> GetStreams(string location)
    {
      List<ChapterInfo> pgcs = new List<ChapterInfo>();

      List<Chapter> list = new List<Chapter>();

      int num = 0;
      TimeSpan ts = new TimeSpan(0);
      string time = String.Empty;
      string name = String.Empty;
      bool onTime = true;
      string[] lines = File.ReadAllLines(location);
      foreach (string line in lines)
      {
        if (onTime)
        {
          num++;
          //read time
          time = line.Replace("CHAPTER" + num.ToString("00") + "=", "");
          ts = TimeSpan.Parse(time);
        }
        else
        {
          //read name
          name = line.Replace("CHAPTER" + num.ToString("00") + "NAME=", "");
          //add it to list
          list.Add(new Chapter() { Name = name, Time = ts });
        }
        onTime = !onTime;
      }

      pgcs.Add(new ChapterInfo()
      {
        Chapters = list,
        SourceName = location,
        SourceHash = ChapterExtractor.ComputeMD5Sum(location),
        FramesPerSecond = 25.0,
        Title = Path.GetFileNameWithoutExtension(location)

      });

      OnStreamDetected(pgcs[0]);
      OnChaptersLoaded(pgcs[0]);
      OnExtractionComplete();
      return pgcs;
    }
  }
}
