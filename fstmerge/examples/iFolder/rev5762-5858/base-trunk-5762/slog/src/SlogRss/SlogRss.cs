using System;
using System.Web;
using Novell.Collaboration;
namespace Novell.Collaboration
{
    public class SlogRss : IHttpHandler
    {
       static public void ProcessChannel(HttpContext ctx, Slog slog)
       {
         Console.WriteLine("ProcessChannel entered");
   ctx.Response.Write("<title>");
   ctx.Response.Write(slog.Title);
   ctx.Response.Write("</title>");
   if (slog.Description != "")
   {
    ctx.Response.Write("<description>");
    ctx.Response.Write(slog.Description);
    ctx.Response.Write("</description>");
   }
   if (slog.Link != "")
   {
    ctx.Response.Write("<link>");
    ctx.Response.Write(slog.Link);
    ctx.Response.Write("</link>");
   }
   if (slog.Language != "")
   {
    ctx.Response.Write("<language>");
    ctx.Response.Write(slog.Language);
    ctx.Response.Write("</language>");
   }
   if (slog.Copyright != "")
   {
    ctx.Response.Write("<copyright>");
    ctx.Response.Write(slog.Copyright);
    ctx.Response.Write("</copyright>");
   }
   if (slog.ManagingEditor != "")
   {
    ctx.Response.Write("<managingEditor>");
    ctx.Response.Write(slog.ManagingEditor);
    ctx.Response.Write("</managingEditor>");
   }
   if (slog.Webmaster != "")
   {
    ctx.Response.Write("<webmaster>");
    ctx.Response.Write(slog.Webmaster);
    ctx.Response.Write("</webmaster>");
   }
   if (slog.PublishDate != "")
   {
    ctx.Response.Write("<pubDate>");
    ctx.Response.Write(slog.PublishDate);
    ctx.Response.Write("</pubDate>");
   }
   if (slog.LastBuildDate != "")
   {
    ctx.Response.Write("<lastBuildDate>");
    ctx.Response.Write(slog.LastBuildDate);
    ctx.Response.Write("</lastBuildDate>");
   }
   if (slog.Generator != "")
   {
    ctx.Response.Write("<generator>");
    ctx.Response.Write(slog.Generator);
    ctx.Response.Write("</generator>");
   }
   if (slog.Cloud != "")
   {
    ctx.Response.Write("<cloud>");
    ctx.Response.Write(slog.Cloud);
    ctx.Response.Write("</cloud>");
   }
   if (slog.Ttl != "")
   {
    ctx.Response.Write("<ttl>");
    ctx.Response.Write(slog.Ttl);
    ctx.Response.Write("</ttl>");
   }
   if (slog.Rating != "")
   {
    ctx.Response.Write("<rating>");
    ctx.Response.Write(slog.Rating);
    ctx.Response.Write("</rating>");
   }
         Console.WriteLine("ProcessChannel exit");
   }
       static public void ProcessItem(HttpContext ctx, SlogEntry entry)
       {
         Console.WriteLine("ProcessItem entered");
         if (entry.Description == "" && entry.Title == "")
         {
          return;
         }
   ctx.Response.Write("<item>");
   if (entry.Title != "")
   {
    ctx.Response.Write("<title>");
    ctx.Response.Write(entry.Title);
    ctx.Response.Write("</title>");
   }
   if (entry.Name != "")
   {
    ctx.Response.Write("<pubDate>");
    ctx.Response.Write(entry.Name);
    ctx.Response.Write("</pubDate>");
   }
   if (entry.Description != "")
   {
    ctx.Response.Write("<description>");
    ctx.Response.Write(entry.Description);
    ctx.Response.Write("</description>");
   }
   try
   {
    if (entry.Author != "")
    {
     ctx.Response.Write("<author>");
     ctx.Response.Write(entry.Author);
     ctx.Response.Write("</author>");
    }
   }
   catch{}
   try
   {
    if (entry.Category != "")
    {
     ctx.Response.Write("<category>");
     ctx.Response.Write(entry.Category);
     ctx.Response.Write("</category>");
    }
   }
   catch{}
   try
   {
    if (entry.Comments != "")
    {
     ctx.Response.Write("<comments>");
     ctx.Response.Write(entry.Comments);
     ctx.Response.Write("</comments>");
    }
   }
   catch{}
   try
   {
    ctx.Response.Write("<guid>");
    ctx.Response.Write(ctx.Request.Url);
    ctx.Response.Write("?node=");
    ctx.Response.Write(entry.ID);
    ctx.Response.Write("</guid>");
   }
   catch{}
   ctx.Response.Write("</item>");
         Console.WriteLine("ProcessItem exit");
       }
        public void ProcessRequest(HttpContext context)
        {
         Console.WriteLine("ProcessRequest entered");
         bool first = true;
   SlogManager slogManager = SlogManager.Connect();
    foreach(Slog slog in slogManager)
   {
    if (first == true)
    {
              context.Response.ContentType = "text/xml";
              context.Response.Write("<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>");
     context.Response.Write("<rss version=\"2.0\">");
     first = false;
    }
    context.Response.Write("<channel>");
    SlogRss.ProcessChannel(context, slog);
    foreach(SlogEntry slogEntry in slog)
    {
     SlogRss.ProcessItem(context, slogEntry);
    }
    context.Response.Write("</channel>");
   }
   if (first == false)
   {
    context.Response.Write("</rss>");
   }
   else
   {
   }
        }
        public bool IsReusable
        {
         get
         {
                return true;
            }
        }
    }
}
