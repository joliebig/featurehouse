





using System;
using System.Collections;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;

namespace Aga.Controls
{
 public class GifFrame
 {
  private Image _image;
  public Image Image
  {
   get { return _image; }
  }

  private int _delay;
  public int Delay
  {
   get { return _delay; }
  }

  public GifFrame(Image im, int del)
  {
   _image = im;
   _delay = del;
  }
 }

 public class GifDecoder
 {
  public const int StatusOK = 0;
  public const int StatusFormatError = 1;
  public const int StatusOpenError = 2;

  private Stream inStream;
  private int status;

  private int width;
  private int height;
  private bool gctFlag;
  private int gctSize;
  private int loopCount = 1;

  private int[] gct;
  private int[] lct;
  private int[] act;

  private int bgIndex;
  private int bgColor;
  private int lastBgColor;
  private int pixelAspect;

  private bool lctFlag;
  private bool interlace;
  private int lctSize;

  private int ix, iy, iw, ih;
  private Rectangle lastRect;
  private Image image;
  private Bitmap bitmap;
  private Image lastImage;

  private byte[] block = new byte[256];
  private int blockSize = 0;


  private int dispose = 0;

  private int lastDispose = 0;
  private bool transparency = false;
  private int delay = 0;
  private int transIndex;

  private const int MaxStackSize = 4096;



  private short[] prefix;
  private byte[] suffix;
  private byte[] pixelStack;
  private byte[] pixels;

  private ArrayList frames;
  private int frameCount;
  private bool _makeTransparent;


  public int FrameCount
  {
   get
   {
    return frameCount;
   }
  }


  public Image Image
  {
   get
   {
    return GetFrame(0).Image;
   }
  }


  public int LoopCount
  {
   get
   {
    return loopCount;
   }
  }

  public GifDecoder(Stream stream, bool makeTransparent)
  {
   _makeTransparent = makeTransparent;
   if (Read(stream) != 0)
    throw new InvalidOperationException();
  }


  private int[] GetPixels(Bitmap bitmap)
  {
   int [] pixels = new int [ 3 * image.Width * image.Height ];
   int count = 0;
   for (int th = 0; th < image.Height; th++)
   {
    for (int tw = 0; tw < image.Width; tw++)
    {
     Color color = bitmap.GetPixel(tw, th);
     pixels[count] = color.R;
     count++;
     pixels[count] = color.G;
     count++;
     pixels[count] = color.B;
     count++;
    }
   }
   return pixels;
  }

  private void SetPixels(int[] pixels)
  {
   int count = 0;
   for (int th = 0; th < image.Height; th++)
   {
    for (int tw = 0; tw < image.Width; tw++)
    {
     Color color = Color.FromArgb( pixels[count++] );
     bitmap.SetPixel( tw, th, color );
    }
   }
   if (_makeTransparent)
    bitmap.MakeTransparent(bitmap.GetPixel(0, 0));
  }

  private void SetPixels()
  {



   int[] dest = GetPixels( bitmap );


   if (lastDispose > 0)
   {
    if (lastDispose == 3)
    {

     int n = frameCount - 2;
     if (n > 0)
     {
      lastImage = GetFrame(n - 1).Image;
     }
     else
     {
      lastImage = null;
     }
    }

    if (lastImage != null)
    {


     int[] prev = GetPixels( new Bitmap( lastImage ) );
     Array.Copy(prev, 0, dest, 0, width * height);


     if (lastDispose == 2)
     {

      Graphics g = Graphics.FromImage( image );
      Color c = Color.Empty;
      if (transparency)
      {
       c = Color.FromArgb( 0, 0, 0, 0 );
      }
      else
      {
       c = Color.FromArgb( lastBgColor ) ;

      }
      Brush brush = new SolidBrush( c );
      g.FillRectangle( brush, lastRect );
      brush.Dispose();
      g.Dispose();
     }
    }
   }


   int pass = 1;
   int inc = 8;
   int iline = 0;
   for (int i = 0; i < ih; i++)
   {
    int line = i;
    if (interlace)
    {
     if (iline >= ih)
     {
      pass++;
      switch (pass)
      {
       case 2 :
        iline = 4;
        break;
       case 3 :
        iline = 2;
        inc = 4;
        break;
       case 4 :
        iline = 1;
        inc = 2;
        break;
      }
     }
     line = iline;
     iline += inc;
    }
    line += iy;
    if (line < height)
    {
     int k = line * width;
     int dx = k + ix;
     int dlim = dx + iw;
     if ((k + width) < dlim)
     {
      dlim = k + width;
     }
     int sx = i * iw;
     while (dx < dlim)
     {

      int index = ((int) pixels[sx++]) & 0xff;
      int c = act[index];
      if (c != 0)
      {
       dest[dx] = c;
      }
      dx++;
     }
    }
   }
   SetPixels( dest );
  }


  public GifFrame GetFrame(int n)
  {
   if ((n >= 0) && (n < frameCount))
    return (GifFrame)frames[n];
   else
    throw new ArgumentOutOfRangeException();
  }


  public Size FrameSize
  {
   get
   {
    return new Size(width, height);
   }
  }


  private int Read( Stream inStream )
  {
   Init();
   if ( inStream != null)
   {
    this.inStream = inStream;
    ReadHeader();
    if (!Error())
    {
     ReadContents();
     if (frameCount < 0)
     {
      status = StatusFormatError;
     }
    }
    inStream.Close();
   }
   else
   {
    status = StatusOpenError;
   }
   return status;
  }



  private void DecodeImageData()
  {
   int NullCode = -1;
   int npix = iw * ih;
   int available,
    clear,
    code_mask,
    code_size,
    end_of_information,
    in_code,
    old_code,
    bits,
    code,
    count,
    i,
    datum,
    data_size,
    first,
    top,
    bi,
    pi;

   if ((pixels == null) || (pixels.Length < npix))
   {
    pixels = new byte[npix];
   }
   if (prefix == null) prefix = new short[MaxStackSize];
   if (suffix == null) suffix = new byte[MaxStackSize];
   if (pixelStack == null) pixelStack = new byte[MaxStackSize + 1];



   data_size = Read();
   clear = 1 << data_size;
   end_of_information = clear + 1;
   available = clear + 2;
   old_code = NullCode;
   code_size = data_size + 1;
   code_mask = (1 << code_size) - 1;
   for (code = 0; code < clear; code++)
   {
    prefix[code] = 0;
    suffix[code] = (byte) code;
   }



   datum = bits = count = first = top = pi = bi = 0;

   for (i = 0; i < npix;)
   {
    if (top == 0)
    {
     if (bits < code_size)
     {

      if (count == 0)
      {

       count = ReadBlock();
       if (count <= 0)
        break;
       bi = 0;
      }
      datum += (((int) block[bi]) & 0xff) << bits;
      bits += 8;
      bi++;
      count--;
      continue;
     }



     code = datum & code_mask;
     datum >>= code_size;
     bits -= code_size;



     if ((code > available) || (code == end_of_information))
      break;
     if (code == clear)
     {

      code_size = data_size + 1;
      code_mask = (1 << code_size) - 1;
      available = clear + 2;
      old_code = NullCode;
      continue;
     }
     if (old_code == NullCode)
     {
      pixelStack[top++] = suffix[code];
      old_code = code;
      first = code;
      continue;
     }
     in_code = code;
     if (code == available)
     {
      pixelStack[top++] = (byte) first;
      code = old_code;
     }
     while (code > clear)
     {
      pixelStack[top++] = suffix[code];
      code = prefix[code];
     }
     first = ((int) suffix[code]) & 0xff;



     if (available >= MaxStackSize)
      break;
     pixelStack[top++] = (byte) first;
     prefix[available] = (short) old_code;
     suffix[available] = (byte) first;
     available++;
     if (((available & code_mask) == 0)
      && (available < MaxStackSize))
     {
      code_size++;
      code_mask += available;
     }
     old_code = in_code;
    }



    top--;
    pixels[pi++] = pixelStack[top];
    i++;
   }

   for (i = pi; i < npix; i++)
   {
    pixels[i] = 0;
   }

  }


  private bool Error()
  {
   return status != StatusOK;
  }


  private void Init()
  {
   status = StatusOK;
   frameCount = 0;
   frames = new ArrayList();
   gct = null;
   lct = null;
  }


  private int Read()
  {
   int curByte = 0;
   try
   {
    curByte = inStream.ReadByte();
   }
   catch (IOException)
   {
    status = StatusFormatError;
   }
   return curByte;
  }


  private int ReadBlock()
  {
   blockSize = Read();
   int n = 0;
   if (blockSize > 0)
   {
    try
    {
     int count = 0;
     while (n < blockSize)
     {
      count = inStream.Read(block, n, blockSize - n);
      if (count == -1)
       break;
      n += count;
     }
    }
    catch (IOException)
    {
    }

    if (n < blockSize)
    {
     status = StatusFormatError;
    }
   }
   return n;
  }


  private int[] ReadColorTable(int ncolors)
  {
   int nbytes = 3 * ncolors;
   int[] tab = null;
   byte[] c = new byte[nbytes];
   int n = 0;
   try
   {
    n = inStream.Read(c, 0, c.Length );
   }
   catch (IOException)
   {
   }
   if (n < nbytes)
   {
    status = StatusFormatError;
   }
   else
   {
    tab = new int[256];
    int i = 0;
    int j = 0;
    while (i < ncolors)
    {
     int r = ((int) c[j++]) & 0xff;
     int g = ((int) c[j++]) & 0xff;
     int b = ((int) c[j++]) & 0xff;
     tab[i++] = ( int ) ( 0xff000000 | (r << 16) | (g << 8) | b );
    }
   }
   return tab;
  }


  private void ReadContents()
  {

   bool done = false;
   while (!(done || Error()))
   {
    int code = Read();
    switch (code)
    {

     case 0x2C :
      ReadImage();
      break;

     case 0x21 :
      code = Read();
     switch (code)
     {
      case 0xf9 :
       ReadGraphicControlExt();
       break;

      case 0xff :
       ReadBlock();
       String app = "";
       for (int i = 0; i < 11; i++)
       {
        app += (char) block[i];
       }
       if (app.Equals("NETSCAPE2.0"))
       {
        ReadNetscapeExt();
       }
       else
        Skip();
       break;

      default :
       Skip();
       break;
     }
      break;

     case 0x3b :
      done = true;
      break;

     case 0x00 :
      break;

     default :
      status = StatusFormatError;
      break;
    }
   }
  }


  private void ReadGraphicControlExt()
  {
   Read();
   int packed = Read();
   dispose = (packed & 0x1c) >> 2;
   if (dispose == 0)
   {
    dispose = 1;
   }
   transparency = (packed & 1) != 0;
   delay = ReadShort() * 10;
   transIndex = Read();
   Read();
  }


  private void ReadHeader()
  {
   String id = "";
   for (int i = 0; i < 6; i++)
   {
    id += (char) Read();
   }
   if (!id.StartsWith("GIF"))
   {
    status = StatusFormatError;
    return;
   }

   ReadLSD();
   if (gctFlag && !Error())
   {
    gct = ReadColorTable(gctSize);
    bgColor = gct[bgIndex];
   }
  }


  private void ReadImage()
  {
   ix = ReadShort();
   iy = ReadShort();
   iw = ReadShort();
   ih = ReadShort();

   int packed = Read();
   lctFlag = (packed & 0x80) != 0;
   interlace = (packed & 0x40) != 0;


   lctSize = 2 << (packed & 7);

   if (lctFlag)
   {
    lct = ReadColorTable(lctSize);
    act = lct;
   }
   else
   {
    act = gct;
    if (bgIndex == transIndex)
     bgColor = 0;
   }
   int save = 0;
   if (transparency)
   {
    save = act[transIndex];
    act[transIndex] = 0;
   }

   if (act == null)
   {
    status = StatusFormatError;
   }

   if (Error()) return;

   DecodeImageData();
   Skip();

   if (Error()) return;

   frameCount++;





   bitmap = new Bitmap( width, height );
   image = bitmap;
   SetPixels();

   frames.Add(new GifFrame(bitmap, delay));

   if (transparency)
   {
    act[transIndex] = save;
   }
   ResetFrame();

  }


  private void ReadLSD()
  {


   width = ReadShort();
   height = ReadShort();


   int packed = Read();
   gctFlag = (packed & 0x80) != 0;


   gctSize = 2 << (packed & 7);

   bgIndex = Read();
   pixelAspect = Read();
  }


  private void ReadNetscapeExt()
  {
   do
   {
    ReadBlock();
    if (block[0] == 1)
    {

     int b1 = ((int) block[1]) & 0xff;
     int b2 = ((int) block[2]) & 0xff;
     loopCount = (b2 << 8) | b1;
    }
   } while ((blockSize > 0) && !Error());
  }


  private int ReadShort()
  {

   return Read() | (Read() << 8);
  }


  private void ResetFrame()
  {
   lastDispose = dispose;
   lastRect = new Rectangle(ix, iy, iw, ih);
   lastImage = image;
   lastBgColor = bgColor;

   lct = null;
  }


  private void Skip()
  {
   do
   {
    ReadBlock();
   } while ((blockSize > 0) && !Error());
  }
 }
}
