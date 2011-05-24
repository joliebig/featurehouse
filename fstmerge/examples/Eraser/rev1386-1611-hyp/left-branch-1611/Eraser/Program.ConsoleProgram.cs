

using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

using ComLib.Arguments;

using Eraser.Manager;
using Eraser.Util;

namespace Eraser
{
 internal static partial class Program
 {






  class ConsoleProgram : IDisposable
  {




   public ConsoleProgram(string[] commandLine)
   {
    CommandLine = commandLine;
    Handlers = new Dictionary<string, ConsoleActionData>();


    Arguments = new ConsoleArguments();
    Args.Parse(commandLine, CommandLinePrefixes, CommandLineSeparators, Arguments);


    if (!Arguments.Quiet)
     ConsoleWindow = new ConsoleWindow();
   }


   ~ConsoleProgram()
   {
    Dispose(false);
   }

   public void Dispose()
   {
    Dispose(true);
    GC.SuppressFinalize(this);
   }

   public void Dispose(bool disposing)
   {

    Console.Out.Flush();


    if (!Arguments.Quiet)
    {
     Console.Write("\nPress any key to continue . . . ");
     Console.Out.Flush();
     Console.ReadKey(true);

     if (disposing)
      ConsoleWindow.Dispose();
    }
   }





   public void Run()
   {

    ConsoleActionData data = Handlers[Arguments.Action];
    ConsoleArguments arguments = data.Arguments;
    ComLib.BoolMessageItem<Args> parseResult = Args.Parse(CommandLine,
     CommandLinePrefixes, CommandLineSeparators, arguments);
    if (!parseResult.Success)
     throw new ArgumentException(parseResult.Message);


    System.Diagnostics.Debug.Assert(Arguments.Action == parseResult.Item.Positional[0]);
    parseResult.Item.Positional.RemoveAt(0);
    arguments.PositionalArguments = parseResult.Item.Positional;


    data.Handler(arguments);
   }







   public delegate void ActionHandler(ConsoleArguments handler);




   public Dictionary<string, ConsoleActionData> Handlers { get; private set; }




   private ConsoleWindow ConsoleWindow;




   private string[] CommandLine;




   private ConsoleArguments Arguments;
  }




  class ConsoleActionData
  {
   public ConsoleActionData(ConsoleProgram.ActionHandler handler,
    ConsoleArguments arguments)
   {
    Handler = handler;
    Arguments = arguments;
   }




   public ConsoleProgram.ActionHandler Handler { get; private set; }




   public ConsoleArguments Arguments { get; private set; }
  }
 }
}
