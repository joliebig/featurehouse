
using System;
using System.IO;
using System.Collections;
using Simias;
using Simias.Storage;

namespace Novell.Collaboration
{




 public class SlogEntry : Node
 {

  internal static string dateProperty = "SLOG:Date";
  internal static string titleProperty = "SLOG:Title";
  internal static string entryProperty = "SLOG:Entry";
  internal static string userIDProperty = "SLOG:UserID";
  internal static string categoryProperty = "SLOG:Category";
  internal static string commentsProperty = "SLOG:Comments";
  public string UserID
  {
   get
   {
    try
    {
     return(this.Properties.GetSingleProperty(userIDProperty).ToString());
    }
    catch{}
    return("");
   }
   set
   {
    try
    {
     if (value != null)
     {
      this.Properties.ModifyProperty(userIDProperty, (string) value);
     }
     else
     {
      this.Properties.DeleteProperties(userIDProperty);
     }
    }
    catch{}
   }
  }
  public string PublishDate
  {
   get
   {
    try
    {
     return(this.CreationTime.ToString());
    }
    catch{}
    return("");
   }
   set
   {
    try
    {
     if (value != null)
     {
     }
     else
     {
     }
    }
    catch{}
   }
  }
  public string Title
  {
   get
   {
    try
    {
     return(this.Name);
    }
    catch{}
    return("");
   }
   set
   {
    try
    {
     if (value != null)
     {
      this.Name = value;
     }
    }
    catch{}
   }
  }
  public string Description
  {
   get
   {
    try
    {
     return(this.Properties.GetSingleProperty(entryProperty).ToString());
    }
    catch{}
    return("");
   }
   set
   {
    try
    {
     if (value != null)
     {
      this.Properties.ModifyProperty(entryProperty, (string) value);
     }
     else
     {
      this.Properties.DeleteProperties(entryProperty);
     }
    }
    catch{}
   }
  }
  public string Author
  {
   get
   {
    try
    {
     return(this.Properties.GetSingleProperty(userIDProperty).ToString());
    }
    catch{}
    return("");
   }
  }
  public string Category
  {
   get
   {
    try
    {
     return(this.Properties.GetSingleProperty(categoryProperty).ToString());
    }
    catch{}
    return("");
   }
   set
   {
    try
    {
     if (value != null)
     {
      this.Properties.ModifyProperty(categoryProperty, (string) value);
     }
     else
     {
      this.Properties.DeleteProperties(categoryProperty);
     }
    }
    catch{}
   }
  }
  public string Comments
  {
   get
   {
    try
    {
     return(this.Properties.GetSingleProperty(commentsProperty).ToString());
    }
    catch{}
    return("");
   }
   set
   {
    try
    {
     if (value != null)
     {
      this.Properties.ModifyProperty(commentsProperty, (string) value);
     }
     else
     {
      this.Properties.DeleteProperties(commentsProperty);
     }
    }
    catch{}
   }
  }
  public SlogEntry(Slog slog, string name) : base(name)
  {
   slog.SetType(this, typeof(SlogEntry).Name);
  }
  public SlogEntry(Slog slog, ShallowNode sNode) : base(slog, sNode)
  {
  }
  public SlogEntry(Slog slog, Node cNode) : base(cNode)
  {
  }
 }
}
