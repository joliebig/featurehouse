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
  public class BlurayExtractor : ChapterExtractor
  {
    public override string[] Extensions
    {
      get { return new string[] { }; }
    }

    public override List<ChapterInfo> GetStreams(string location)
    {
        List<ChapterInfo> mpls = new List<ChapterInfo>();
        string path = Path.Combine(Path.Combine(location, "BDMV"), "PLAYLIST");
        if (!Directory.Exists(path))
            throw new FileNotFoundException("Could not find PLAYLIST folder on BluRay disc.");

        ChapterExtractor ex = new BDInfoExtractor();
        ex.StreamDetected += (sender, args) => OnStreamDetected(args.ProgramChain);
        ex.ChaptersLoaded += (sender, args) => OnChaptersLoaded(args.ProgramChain);

        foreach (string file in Directory.GetFiles(path, "*.mpls"))
        {
            ChapterInfo pl = ex.GetStreams(file)[0];
            pl.SourceName = Path.GetFileName(file);
            mpls.Add(pl);
        }

        mpls = mpls.OrderByDescending(p => p.Duration).ToList();
        OnExtractionComplete();
        return mpls;
    }

  }
}
