using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.Serialization;
using Eraser.Util;
namespace Eraser.Manager
{
 [Serializable]
 public class FatalException : Exception
 {
  public FatalException()
  {
  }
  public FatalException(string message)
   : base(message)
  {
  }
  protected FatalException(SerializationInfo info, StreamingContext context)
   : base(info, context)
  {
  }
  public FatalException(string message, Exception innerException)
   : base(message, innerException)
  {
  }
 }
 [Serializable]
 public class EntropySourceNotFoundException : FatalException
 {
  public EntropySourceNotFoundException()
  {
  }
  public EntropySourceNotFoundException(string message)
   : base(message)
  {
  }
  public EntropySourceNotFoundException(Guid value)
   : this(S._("EntropySource GUID not found: {0}", value.ToString()))
  {
  }
  protected EntropySourceNotFoundException(SerializationInfo info, StreamingContext context)
   : base(info, context)
  {
  }
  public EntropySourceNotFoundException(Guid value, Exception innerException)
   : this(S._("EntropySource GUID not found: {0}", value.ToString()),
    innerException)
  {
  }
  public EntropySourceNotFoundException(string message, Exception innerException)
   : base(message, innerException)
  {
  }
 }
 [Serializable]
 public class ErasureMethodNotFoundException : FatalException
 {
  public ErasureMethodNotFoundException()
  {
  }
  public ErasureMethodNotFoundException(string message)
   : base(message)
  {
  }
  public ErasureMethodNotFoundException(Guid value)
   : this(S._("Erasure method not found: {0}", value.ToString()))
  {
  }
  protected ErasureMethodNotFoundException(SerializationInfo info, StreamingContext context)
   : base(info, context)
  {
  }
  public ErasureMethodNotFoundException(Guid value, Exception innerException)
   : this(S._("Erasure method not found: {0}", value.ToString()),
    innerException)
  {
  }
  public ErasureMethodNotFoundException(string message, Exception innerException)
   : base(message, innerException)
  {
  }
 }
 [Serializable]
 public class PrngNotFoundException : FatalException
 {
  public PrngNotFoundException()
  {
  }
  public PrngNotFoundException(string message)
   : base(message)
  {
  }
  public PrngNotFoundException(Guid value)
   : this(S._("PRNG not found: {0}", value.ToString()))
  {
  }
  protected PrngNotFoundException(SerializationInfo info, StreamingContext context)
   : base(info, context)
  {
  }
  public PrngNotFoundException(Guid value, Exception innerException)
   : this(S._("PRNG not found: {0}", value.ToString()),
    innerException)
  {
  }
  public PrngNotFoundException(string message, Exception innerException)
   : base(message, innerException)
  {
  }
 }
}
