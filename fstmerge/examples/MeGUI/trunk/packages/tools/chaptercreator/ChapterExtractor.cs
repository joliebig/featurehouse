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
using System.Security.Cryptography;

namespace MeGUI
{
  public abstract class ChapterExtractor
  {
    public abstract string[] Extensions { get; }
    public virtual bool SupportsMultipleStreams { get { return true; } }
    public abstract List<ChapterInfo> GetStreams(string location);

    public event EventHandler<ProgramChainArg> StreamDetected;
    public event EventHandler<ProgramChainArg> ChaptersLoaded;
    public event EventHandler ExtractionComplete;

    public static string ComputeMD5Sum(string path)
    {
      return BitConverter.ToString(new
          MD5CryptoServiceProvider().ComputeHash(System.IO.File.ReadAllBytes(path)))
          .Replace("-", "").ToLower();
    }

    protected void OnExtractionComplete()
    {
      if (ExtractionComplete != null) ExtractionComplete(this, EventArgs.Empty);
    }

    protected void OnStreamDetected(ChapterInfo pgc)
    {
      if (StreamDetected != null) StreamDetected(this, new ProgramChainArg() { ProgramChain = pgc });
    }
    protected void OnChaptersLoaded(ChapterInfo pgc)
    {
      if (ChaptersLoaded != null) ChaptersLoaded(this, new ProgramChainArg() { ProgramChain = pgc });
    }
  }

  public class ProgramChainArg : EventArgs
  {
    public ChapterInfo ProgramChain { get; set; }
  }

}
