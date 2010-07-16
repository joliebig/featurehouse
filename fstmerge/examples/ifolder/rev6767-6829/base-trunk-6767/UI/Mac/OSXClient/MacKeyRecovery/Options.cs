

using System;
using System.IO;
using System.Collections;
using System.Reflection;
using System.Text.RegularExpressions;

namespace Novell.iFolder.Utility
{



 public class Options
 {



  private Options()
  {
  }






  public static void ParseArguments(object obj, string[] args)
  {

   Hashtable map = new Hashtable();

   Option[] options = GetOptions(obj);

   foreach(Option o in options)
   {
    foreach(string name in o.Names)
    {
     map.Add(name, o);
    }
   }


   Regex parse = new Regex(@"^-{1,2}|^/|=|:", RegexOptions.IgnoreCase | RegexOptions.Compiled);


   Regex trim = new Regex(@"^['""]?(.*?)['""]?$", RegexOptions.IgnoreCase | RegexOptions.Compiled);

   Option option = null;


   foreach(string arg in args)
   {
    string[] parts = parse.Split(arg, 3);

    switch(parts.Length)
    {

     case 1:
      if (option != null)
      {

       option.Value = trim.Replace(parts[0], "$1");


       option = null;
      }
      else
      {

       throw new BadArgumentException(arg);
      }
      break;


     case 2:

      option = (Option)map[parts[1]];

      if (option != null)
      {

       option.Value = "true";
      }
      else
      {

       throw new UnknownOptionException(arg);
      }
      break;


     case 3:

      option = (Option)map[parts[1]];

      if (option != null)
      {

       option.Value = trim.Replace(parts[2], "$1");;


       option = null;
      }
      else
      {

       throw new UnknownOptionException(arg);
      }
      break;
    }
   }
  }





  public static void CheckRequiredOptions(object obj)
  {
   Option[] options = Options.GetOptions(obj);

   foreach(Option o in options)
   {
    if (o.Required && !o.Assigned)
    {

     throw new RequiredOptionNotAssignedException(o.Name);
    }
   }
  }






  public static void WriteOptions(object obj, TextWriter writer)
  {
   writer.WriteLine("Options:");
   writer.WriteLine();

   Option[] options = Options.GetOptions(obj);

   foreach(Option o in options)
   {
    writer.WriteLine(o);
   }
  }






  public static Option[] GetOptions(object obj)
  {
   ArrayList result = new ArrayList();


   FieldInfo[] fields = obj.GetType().GetFields();

   foreach(FieldInfo field in fields)
   {

    if (field.FieldType.Equals(typeof(Option)) || field.FieldType.IsSubclassOf(typeof(Option)))
    {
     result.Add(field.GetValue(obj));
    }
   }

   return (Option[])result.ToArray(typeof(Option));
  }
 }




 public class Option
 {
  private string[] names;
  private string title;
  private string description;
  private bool required;
  private string defaultValue;
  private bool assigned;
  private string val;



  protected bool prompt = true;
  private OptionEnteredHandler onOptionEntered;





  public delegate bool OptionEnteredHandler();
  public Option(string names, string title, string description, bool required, string defaultValue)
  {
   this.names = names.Split(',');
   this.title = title;
   this.description = description;
   this.required = required;
   this.defaultValue = defaultValue;
   this.assigned = false;
   this.val = null;
  }
  public string Name
  {
   get { return names[0]; }
  }
  public string[] Names
  {
   get { return names; }
  }
  public string Title
  {
   get { return title; }
  }
  public string Description
  {
   get { return description; }
   set { description = value; }
  }
  public bool Required
  {
   get { return required; }
   set { required = value; }
  }
  public string DefaultValue
  {
   get { return defaultValue; }
   set { defaultValue = value; }
  }
  public bool Assigned
  {
   get { return assigned; }
   set { assigned = value; }
  }
  public string Value
  {
   get { return Assigned ? val : defaultValue; }
   set
   {
    val = value;
    assigned = true;
    if (onOptionEntered != null)
     onOptionEntered();
   }
  }
  internal string InternalValue
  {
   set { val = value; assigned = true; }
  }
  public bool Prompt
  {
   get { return prompt; }
   set { prompt = value; }
  }
  public OptionEnteredHandler OnOptionEntered
  {
   set { onOptionEntered = value; }
  }
  public void FromEnvironment(string variable)
  {
   string temp = Environment.GetEnvironmentVariable(variable);
   if ((temp != null) && (temp.Length > 0))
   {
    Value = temp;
   }
  }
  public override string ToString()
  {
   return String.Format("{0} {1} \"{2}\"", Name, Assigned ? "=" : "~", Value);
  }
 }
 public class UnknownOptionException : ArgumentException
 {
  public UnknownOptionException(string message) : base(message)
  {
  }
 }
 public class BadArgumentException : ArgumentException
 {
  public BadArgumentException(string message) : base(message)
  {
  }
 }
 public class RequiredOptionNotAssignedException : ArgumentException
 {
  public RequiredOptionNotAssignedException(string message) : base(message)
  {
  }
 }
 public class BoolOption : Option
 {
  public BoolOption(string names, string title, string description, bool required, bool defaultValue) :
   base (names, title, description, required, defaultValue.ToString())
  {
  }
  public new bool DefaultValue
  {
   get { return Boolean.Parse(base.DefaultValue); }
   set { base.DefaultValue = value.ToString(); }
  }
  public new bool Value
  {
   get
   {
    string result = base.Value;
    if (!Assigned)
    {
     result = base.DefaultValue;
    }
    return Boolean.Parse(result);
   }
   set
   {
    base.Value = value.ToString();
   }
  }
 }
 public class NoPromptOption : Option
 {
  public NoPromptOption(string names, string title, string description, bool required, string defaultValue) :
   base (names, title, description, required, defaultValue)
  {
   prompt = false;
  }
 }
}
