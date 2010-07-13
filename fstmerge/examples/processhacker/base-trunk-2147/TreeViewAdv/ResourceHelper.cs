using System;
using System.IO;
using System.Reflection;
using System.Windows.Forms;
using System.Collections.Generic;
using System.Text;

namespace Aga.Controls
{
    public static class ResourceHelper
    {

        private static Cursor _dVSplitCursor = GetCursor(Properties.Resources.DVSplit);
        public static Cursor DVSplitCursor
        {
            get { return _dVSplitCursor; }
        }

  private static GifDecoder _loadingIcon = GetGifDecoder(Properties.Resources.loading_icon);
  public static GifDecoder LoadingIcon
  {
   get { return _loadingIcon; }
  }






        private static Cursor GetCursor(byte[] data)
        {
            using (MemoryStream s = new MemoryStream(data))
                return new Cursor(s);
        }






  private static GifDecoder GetGifDecoder(byte[] data)
  {
   using(MemoryStream ms = new MemoryStream(data))
    return new GifDecoder(ms, true);
  }

    }
}
