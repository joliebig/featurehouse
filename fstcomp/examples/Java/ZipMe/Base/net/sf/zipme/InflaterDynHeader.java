//

package net.sf.zipme;

class InflaterDynHeader {
  private static final int LNUM=0;
  private static final int DNUM=1;
  private static final int BLNUM=2;
  private static final int BLLENS=3;
  private static final int LENS=4;
  private static final int REPS=5;
  private static final int repMin[]={3,3,11};
  private static final int repBits[]={2,3,7};
  private byte[] blLens;
  private byte[] litdistLens;
  private InflaterHuffmanTree blTree;
  private int mode;
  private int lnum, dnum, blnum, num;
  private int repSymbol;
  private byte lastLen;
  private int ptr;
  private static final int[] BL_ORDER={16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15};
  public InflaterDynHeader(){
  }
  public boolean decode(  StreamManipulator input) throws DataFormatException {
    decode_loop:     for (; ; ) {
switch (mode) {
case LNUM:
        lnum=input.peekBits(5);
      if (lnum < 0)       return false;
    lnum+=257;
  input.dropBits(5);
mode=DNUM;
case DNUM:
dnum=input.peekBits(5);
if (dnum < 0) return false;
dnum++;
input.dropBits(5);
num=lnum + dnum;
litdistLens=new byte[num];
mode=BLNUM;
case BLNUM:
blnum=input.peekBits(4);
if (blnum < 0) return false;
blnum+=4;
input.dropBits(4);
blLens=new byte[19];
ptr=0;
mode=BLLENS;
case BLLENS:
while (ptr < blnum) {
int len=input.peekBits(3);
if (len < 0) return false;
input.dropBits(3);
blLens[BL_ORDER[ptr]]=(byte)len;
ptr++;
}
blTree=new InflaterHuffmanTree(blLens);
blLens=null;
ptr=0;
mode=LENS;
case LENS:
{
int symbol;
while (((symbol=blTree.getSymbol(input)) & ~15) == 0) {
litdistLens[ptr++]=lastLen=(byte)symbol;
if (ptr == num) {
return true;
}
}
if (symbol < 0) return false;
if (symbol >= 17) {
lastLen=0;
}
 else {
if (ptr == 0) throw new DataFormatException();
}
repSymbol=symbol - 16;
mode=REPS;
}
case REPS:
{
int bits=repBits[repSymbol];
int count=input.peekBits(bits);
if (count < 0) return false;
input.dropBits(bits);
count+=repMin[repSymbol];
if (ptr + count > num) throw new DataFormatException();
while (count-- > 0) litdistLens[ptr++]=lastLen;
if (ptr == num) {
return true;
}
}
mode=LENS;
continue decode_loop;
}
}
}
public InflaterHuffmanTree buildLitLenTree() throws DataFormatException {
byte[] litlenLens=new byte[lnum];
System.arraycopy(litdistLens,0,litlenLens,0,lnum);
return new InflaterHuffmanTree(litlenLens);
}
public InflaterHuffmanTree buildDistTree() throws DataFormatException {
byte[] distLens=new byte[dnum];
System.arraycopy(litdistLens,lnum,distLens,0,dnum);
return new InflaterHuffmanTree(distLens);
}
}
