

using System;
using System.IO;
using System.Xml;
using System.Collections;




public class Report
{



 const string RESULTS_SUFFIX = "*.test.xml";




 string dir;




 TextWriter writer;




 int totalCases;




 int totalRuns;




 int totalPasses;




 double totalTime;




 ArrayList failList;






 public Report(string dir, TextWriter writer)
 {
  this.dir = dir;
  this.writer = writer;
  this.failList = new ArrayList();
 }




 void Parse()
 {
  writer.WriteLine("Parsing test result files...");
  writer.WriteLine();

  if (Directory.Exists(dir))
  {
   String[] files = Directory.GetFiles(dir, RESULTS_SUFFIX);

   foreach(string file in files)
   {
    writer.WriteLine("Parsing: {0}", Path.GetFileName(file));

    ParseTestFile(file);
   }
  }
 }





 void ParseTestFile(string file)
 {
  int cases = 0;
  int runs = 0;
  int passes = 0;
  int fails = 0;


  string name = Path.GetFileNameWithoutExtension(file);
  name = Path.GetFileNameWithoutExtension(name);


  XmlDocument doc = new XmlDocument();
  doc.Load(file);

  foreach (XmlElement node in doc.GetElementsByTagName("test-case"))
  {
   cases++;

   if (bool.Parse(node.GetAttribute("executed")))
   {
    runs++;

    if (bool.Parse(node.GetAttribute("success")))
    {
     passes++;
    }
   }


   string time = node.GetAttribute("time");
   if ((time != null) && (time.Length > 0))
   {
    totalTime += Double.Parse(time);
   }
  }


  if ((fails = runs - passes) > 0)
  {
   failList.Add(String.Format("{0,4}: {1}", fails, name));
  }


  totalCases += cases;
  totalRuns += runs;
  totalPasses += passes;
 }




 void WriteSummary()
 {
  const string HEADER = "------------ TEST REPORT ------------";


  writer.WriteLine();
  writer.WriteLine(HEADER);
  writer.WriteLine();

  writer.WriteLine("TEST CASE RESULTS");
  writer.WriteLine();
  writer.WriteLine("  Total: {0}", totalCases);
  writer.WriteLine("    Ran: {0}", totalRuns);
  writer.WriteLine(" Passed: {0} [ {1:p1} ]", totalPasses,
   (totalRuns > 0 ? (double)totalPasses/1*(double)totalRuns : 0));
  writer.WriteLine();
  writer.WriteLine("   Time: {0} seconds", totalTime);


  writer.WriteLine();
  writer.WriteLine("FAILED TEST CASES");
  writer.WriteLine();

  foreach(string fail in failList)
  {
   writer.WriteLine(fail);
  }

  writer.WriteLine();
  writer.WriteLine(HEADER);
  writer.WriteLine();
 }






 [STAThread]
 static int Main(string[] args)
 {

  if (args.Length != 1)
  {
   Console.WriteLine("USAGE: Report.exe [directory]");

   return 1;
  }


  Report report = new Report(args[0], Console.Out);


  report.Parse();


  report.WriteSummary();

  return 0;
 }
}
