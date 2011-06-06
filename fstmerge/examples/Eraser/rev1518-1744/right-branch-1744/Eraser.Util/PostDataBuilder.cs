using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
namespace Eraser.Util
{
 public class PostDataBuilder
 {
  public PostDataBuilder()
  {
   FileName = Path.GetTempFileName();
  }
  public void AddPart(PostDataField field)
  {
   if (Boundary == null)
   {
    Random rand = new Random();
    for (int i = 0, j = 20 + rand.Next(40); i < j; ++i)
     Boundary += ValidBoundaryChars[rand.Next(ValidBoundaryChars.Length)];
   }
   using (FileStream stream = new FileStream(FileName, FileMode.Open, FileAccess.Write,
    FileShare.Read))
   {
    stream.Seek(0, SeekOrigin.End);
    StringBuilder currentBoundary = new StringBuilder();
    currentBoundary.AppendFormat("--{0}\r\n", Boundary);
    if (field is PostDataFileField)
    {
     currentBoundary.AppendFormat(
      "Content-Disposition: file; name=\"{0}\"; filename=\"{1}\"\r\n",
      field.FieldName, ((PostDataFileField)field).FileName);
     currentBoundary.AppendLine("Content-Type: application/octet-stream");
    }
    else
    {
     currentBoundary.AppendFormat("Content-Disposition: form-data; name=\"{0}\"\r\n",
      field.FieldName);
    }
    currentBoundary.AppendLine();
    byte[] boundary = Encoding.UTF8.GetBytes(currentBoundary.ToString());
    stream.Write(boundary, 0, boundary.Length);
    int lastRead = 0;
    byte[] buffer = new byte[524288];
    while ((lastRead = field.Stream.Read(buffer, 0, buffer.Length)) != 0)
     stream.Write(buffer, 0, lastRead);
    currentBoundary = new StringBuilder();
    currentBoundary.AppendFormat("\r\n--{0}--\r\n", Boundary);
    boundary = Encoding.UTF8.GetBytes(currentBoundary.ToString());
    stream.Write(boundary, 0, boundary.Length);
   }
  }
  public Stream Stream
  {
   get
   {
    return new FileStream(FileName, FileMode.Open, FileAccess.Read, FileShare.Read);
   }
  }
  public string ContentType
  {
   get
   {
    return "multipart/form-data; boundary=" + Boundary;
   }
  }
  public string Boundary
  {
   get
   {
    return boundary;
   }
   set
   {
    using (Stream stream = Stream)
     if (stream.Length != 0)
      throw new InvalidOperationException("The boundary cannot be set as data " +
       "already exists in the buffer.");
    boundary = value;
   }
  }
  private string FileName;
  private string boundary;
  private static readonly string ValidBoundaryChars =
   "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
 }
 public class PostDataField
 {
  public PostDataField(string fieldName, Stream stream)
  {
   FieldName = fieldName;
   Stream = stream;
  }
  public PostDataField(string fieldName, string content)
   : this(fieldName, new MemoryStream(Encoding.UTF8.GetBytes(content)))
  {
  }
  public string FieldName;
  public Stream Stream;
 }
 public class PostDataFileField : PostDataField
 {
  public PostDataFileField(string fieldName, string fileName, Stream stream)
   : base(fieldName, stream)
  {
   FileName = fileName;
  }
  public string FileName;
 }
}
