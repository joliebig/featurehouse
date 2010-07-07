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
using System.Xml;
using System.Windows.Forms;

namespace MeGUI
{
  public class ChapterInfo
  {
    public string Title { get; set; }
    public string LangCode { get; set; }
    public string SourceName { get; set; }
    public string SourceType { get; set; }
    public string SourceHash { get; set; }
    public double FramesPerSecond { get; set; }
    public TimeSpan Duration { get; set; }
    public List<Chapter> Chapters { get; set; }

    public override string ToString()
    {
        if (Chapters.Count == 0)
            return string.Empty;
        else if (Chapters.Count > 1)
            return string.Format("{0}  -  {1}  -  [{2} Chapters]", SourceName, Duration.ToString(), Chapters.Count);
        else
            return string.Format("{0}  -  {1}  -  [{2} Chapter]", SourceName, Duration.ToString(), Chapters.Count);
    }

    public void ChangeFps(double fps)
    {
      for (int i = 0; i < Chapters.Count; i++)
      {
        Chapter c = Chapters[i];
        double frames = c.Time.TotalSeconds * FramesPerSecond;
        Chapters[i] = new Chapter() { Name = c.Name, Time = new TimeSpan((long)Math.Round(frames / fps * TimeSpan.TicksPerSecond)) };
      }

      double totalFrames = Duration.TotalSeconds * FramesPerSecond;
      Duration = new TimeSpan((long)Math.Round((totalFrames / fps) * TimeSpan.TicksPerSecond));
      FramesPerSecond = fps;
    }

    public void SaveText(string filename)
    {
      List<string> lines = new List<string>();
      int i = 0;
      foreach (Chapter c in Chapters)
      {
        i++;
        if (c.Time.ToString().Length == 8)
            lines.Add("CHAPTER" + i.ToString("00") + "=" + c.Time.ToString() + ".000"); // better formating
        else if (c.Time.ToString().Length > 12)
            lines.Add("CHAPTER" + i.ToString("00") + "=" + c.Time.ToString().Substring(0, 12)); // remove some duration length too long
        else
            lines.Add("CHAPTER" + i.ToString("00") + "=" + c.Time.ToString());
        lines.Add("CHAPTER" + i.ToString("00") + "NAME=" + c.Name);
      }
      File.WriteAllLines(filename, lines.ToArray());
    }

    public void SaveQpfile(string filename)
    {
      List<string> lines = new List<string>();
      foreach (Chapter c in Chapters)
      {
        lines.Add(string.Format("{0} I -1", (long)Math.Round(c.Time.TotalSeconds * FramesPerSecond)));
      }
      File.WriteAllLines(filename, lines.ToArray());
    }

    public void SaveCelltimes(string filename)
    {
      List<string> lines = new List<string>();
      foreach (Chapter c in Chapters)
      {
        lines.Add(((long)Math.Round(c.Time.TotalSeconds * FramesPerSecond)).ToString());
      }
      File.WriteAllLines(filename, lines.ToArray());
    }

    public void SaveTsmuxerMeta(string filename)
    {
      string text = "--custom-" + Environment.NewLine + "chapters=";
      foreach (Chapter c in Chapters)
      {
        text += c.Time.ToString() + ";";
      }
      text = text.Substring(0, text.Length - 1);
      File.WriteAllText(filename, text);
    }

    public void SaveTimecodes(string filename)
    {
      List<string> lines = new List<string>();
      foreach (Chapter c in Chapters)
      {
        lines.Add(c.Time.ToString());
      }
      File.WriteAllLines(filename, lines.ToArray());
    }

 //   static readonly XNamespace cgNs = "http://jvance.com/2008/ChapterGrabber";
/*
    public static ChapterInfo Load(string filename)
    {
      ChapterInfo ci = new ChapterInfo();
      XDocument doc = XDocument.Load(filename);
      if (doc.Element(cgNs + "chapterInfo").Element(cgNs + "title") != null)
        ci.Title = (string)doc.Element(cgNs + "chapterInfo").Element(cgNs + "title");
      XElement src = doc.Element(cgNs + "chapterInfo").Element(cgNs + "source");

      ci.SourceName = (string)src.Element("name");
      if (src.Element(cgNs + "type") != null)
        ci.SourceType = (string)src.Element(cgNs + "type");
      ci.SourceHash = (string)src.Element(cgNs + "hash");
      ci.FramesPerSecond = Convert.ToDouble(src.Element(cgNs + "fps").Value, new System.Globalization.NumberFormatInfo());
      ci.Duration = TimeSpan.Parse(src.Element(cgNs + "duration").Value);
      ci.Chapters = doc.Element(cgNs + "chapterInfo").Element(cgNs + "chapters").Elements(cgNs + "chapter")
        .Select(e => new Chapter() { Name = (string)e.Attribute("name"), Time = TimeSpan.Parse((string)e.Attribute("time")) }).ToList();
      return ci;
    }

    public void Save(string filename)
    {
      new XDocument(new XElement(cgNs + "chapterInfo",
        new XAttribute(XNamespace.Xml + "lang", LangCode),
        new XAttribute("version", "1"),
        new XComment("This file was generated by ChapterGrabber " + Application.ProductVersion),
        new XComment("For more information visit http://jvance.com/pages/ChapterGrabber.xhtml"),
        Title != null ? new XElement(cgNs + "title", Title) : null,
        new XElement(cgNs + "source",
          new XElement(cgNs + "name", SourceName),
          SourceType != null ? new XElement(cgNs + "type", SourceType) : null,
          new XElement(cgNs + "hash", SourceHash),
          new XElement(cgNs + "fps", FramesPerSecond),
          new XElement(cgNs + "duration", Duration.ToString())),
        new XElement(cgNs + "chapters",
          Chapters.Select(c =>
            new XElement(cgNs + "chapter",
              new XAttribute("time", c.Time.ToString()),
              new XAttribute("name", c.Name)))))).Save(filename);
    }
*/
    public void SaveXml(string filename)
    {
        Random rndb = new Random();
        XmlTextWriter xmlchap = new XmlTextWriter(filename, Encoding.UTF8);
        xmlchap.Formatting = Formatting.Indented;
        xmlchap.WriteStartDocument();
        xmlchap.WriteComment("<!DOCTYPE Tags SYSTEM " + "\"" + "matroskatags.dtd" + "\"" + ">");
        xmlchap.WriteStartElement("Chapters");
        xmlchap.WriteStartElement("EditionEntry");
        xmlchap.WriteElementString("EditionFlagHidden", "0");
        xmlchap.WriteElementString("EditionFlagDefault", "0");
        xmlchap.WriteElementString("EditionUID", Convert.ToString(rndb.Next(1, Int32.MaxValue)));
        foreach (Chapter c in Chapters)
        {
            xmlchap.WriteStartElement("ChapterAtom");
            xmlchap.WriteStartElement("ChapterDisplay");
            xmlchap.WriteElementString("ChapterString", c.Name);
            xmlchap.WriteElementString("ChapterLanguage", LangCode == null ? "und" : LangCode);
            xmlchap.WriteEndElement();
            xmlchap.WriteElementString("ChapterUID", Convert.ToString(rndb.Next(1, Int32.MaxValue)));
            if (c.Time.ToString().Length == 8)
                xmlchap.WriteElementString("ChapterTimeStart", c.Time.ToString() + ".0000000");
            else
                xmlchap.WriteElementString("ChapterTimeStart", c.Time.ToString());
            xmlchap.WriteElementString("ChapterFlagHidden", "0");
            xmlchap.WriteElementString("ChapterFlagEnabled", "1");
            xmlchap.WriteEndElement();
        }
        xmlchap.WriteEndElement();
        xmlchap.WriteEndElement();
        xmlchap.Flush();
        xmlchap.Close();
    }                           
  }
}
