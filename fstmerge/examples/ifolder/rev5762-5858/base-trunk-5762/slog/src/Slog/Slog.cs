
using System;
using System.Collections;
using System.IO;
using System.Text.RegularExpressions;
using Simias.Storage;

namespace Novell.Collaboration
{




 public class Slog : Collection
 {

  private static string descriptionProperty = "SLOG:Description";
  private static string linkProperty = "SLOG:Link";
  private static string languageProperty = "SLOG:Language";
  private static string copyrightProperty = "SLOG:Copyright";
  private static string editorProperty = "SLOG:Editor";
  private static string webmasterProperty = "SLOG:Webmaster";
  private static string generatorProperty = "SLOG:Generator";
  private static string cloudProperty = "SLOG:Cloud";
  private static string ttlProperty = "SLOG:TTL";
  private static string ratingProperty = "SLOG:Rating";
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
     return(this.Properties.GetSingleProperty(descriptionProperty).ToString());
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
      this.Properties.ModifyProperty(descriptionProperty, (string) value);
     }
     else
     {
      this.Properties.DeleteProperties(descriptionProperty);
     }
    }
    catch{}
   }
  }
  public string Link
  {
   get
   {
    try
    {
     return(this.Properties.GetSingleProperty(linkProperty).ToString());
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
      this.Properties.ModifyProperty(linkProperty, (string) value);
     }
     else
     {
      this.Properties.DeleteProperties(linkProperty);
     }
    }
    catch{}
   }
  }
  public string Language
  {
   get
   {
    try
    {
     return(this.Properties.GetSingleProperty(languageProperty).ToString());
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
      this.Properties.ModifyProperty(languageProperty, (string) value);
     }
     else
     {
      this.Properties.DeleteProperties(languageProperty);
     }
    }
    catch{}
   }
  }
  public string Copyright
  {
   get
   {
    try
    {
     return(this.Properties.GetSingleProperty(copyrightProperty).ToString());
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
      this.Properties.ModifyProperty(copyrightProperty, (string) value);
     }
     else
     {
      this.Properties.DeleteProperties(copyrightProperty);
     }
    }
    catch{}
   }
  }
  public string ManagingEditor
  {
   get
   {
    try
    {
     return(this.Properties.GetSingleProperty(editorProperty).ToString());
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
      this.Properties.ModifyProperty(editorProperty, (string) value);
     }
     else
     {
      this.Properties.DeleteProperties(editorProperty);
     }
    }
    catch{}
   }
  }
  public string Webmaster
  {
   get
   {
    try
    {
     return(this.Properties.GetSingleProperty(webmasterProperty).ToString());
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
      this.Properties.ModifyProperty(webmasterProperty, (string) value);
     }
     else
     {
      this.Properties.DeleteProperties(webmasterProperty);
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
     return("");
    }
    catch{}
    return("");
   }
  }
  public string LastBuildDate
  {
   get
   {
    try
    {
     return("");
    }
    catch{}
    return("");
   }
  }
  public string Generator
  {
   get
   {
    try
    {
     return(this.Properties.GetSingleProperty(generatorProperty).ToString());
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
      this.Properties.ModifyProperty(generatorProperty, (string) value);
     }
     else
     {
      this.Properties.DeleteProperties(generatorProperty);
     }
    }
    catch{}
   }
  }
  public string Cloud
  {
   get
   {
    try
    {
     return(this.Properties.GetSingleProperty(cloudProperty).ToString());
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
      this.Properties.ModifyProperty(cloudProperty, (string) value);
     }
     else
     {
      this.Properties.DeleteProperties(cloudProperty);
     }
    }
    catch{}
   }
  }
  public string Ttl
  {
   get
   {
    try
    {
     return(this.Properties.GetSingleProperty(ttlProperty).ToString());
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
      this.Properties.ModifyProperty(ttlProperty, (string) value);
     }
     else
     {
      this.Properties.DeleteProperties(ttlProperty);
     }
    }
    catch{}
   }
  }
  public string Rating
  {
   get
   {
    try
    {
     return(this.Properties.GetSingleProperty(ratingProperty).ToString());
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
      this.Properties.ModifyProperty(ratingProperty, (string) value);
     }
     else
     {
      this.Properties.DeleteProperties(ratingProperty);
     }
    }
    catch{}
   }
  }
  public Slog(Store store, string name) :
   base(store, name, store.DefaultDomain)
  {
   this.SetType(this, typeof(Slog).Name);
  }
  public Slog(Store store, Node node)
   : base(store, node)
  {
   if (!this.IsType( this, typeof(Slog).Name ) )
   {
    throw new ApplicationException( "Invalid Slog collection." );
   }
  }
  public new IEnumerator GetEnumerator()
  {
   ICSList results = this.Search( PropertyTags.Types,
           typeof(SlogEntry).Name,
           SearchOp.Equal );
   EntryEnumerator eEnumerator =
     new EntryEnumerator(this, results.GetEnumerator());
   return eEnumerator;
  }
 }
 public class EntryEnumerator : IEnumerator
 {
  private IEnumerator entryEnum = null;
  private Slog slog = null;
  internal EntryEnumerator(Slog s, IEnumerator enumerator)
  {
   this.slog = s;
   this.entryEnum = enumerator;
  }
  public void Reset()
  {
   entryEnum.Reset();
  }
  public object Current
  {
   get
   {
    try
    {
     ShallowNode sNode = (ShallowNode) entryEnum.Current;
     return(new SlogEntry(slog, sNode));
    }
    catch{}
    return(null);
   }
  }
  public bool MoveNext()
  {
   return entryEnum.MoveNext();
  }
 }
}
