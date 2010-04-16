//

package net.sf.zipme;

class DeflaterEngine implements DeflaterConstants {
  private static final int TOO_FAR=4096;
  private int ins_h;
  /** 
 * Hashtable, hashing three characters to an index for window, so
 * that window[index]..window[index+2] have this hash code.  
 * Note that the array should really be unsigned short, so you need
 * to and the values with 0xffff.
 */
  private short[] head;
  /** 
 * prev[index & WMASK] points to the previous index that has the
 * same hash code as the string starting at index.  This way 
 * entries with the same hash code are in a linked list.
 * Note that the array should really be unsigned short, so you need
 * to and the values with 0xffff.
 */
  private short[] prev;
  private int matchStart, matchLen;
  private boolean prevAvailable;
  private int blockStart;
  /** 
 * strstart points to the current character in window.
 */
  private int strstart;
  /** 
 * lookahead is the number of characters starting at strstart in
 * window that are valid.
 * So window[strstart] until window[strstart+lookahead-1] are valid
 * characters.
 */
  private int lookahead;
  /** 
 * This array contains the part of the uncompressed stream that 
 * is of relevance.  The current character is indexed by strstart.
 */
  private byte[] window;
  private int strategy, max_chain, max_lazy, niceLength, goodLength;
  /** 
 * The current compression function. 
 */
  private int comprFunc;
  /** 
 * The input data for compression. 
 */
  private byte[] inputBuf;
  /** 
 * The total bytes of input read. 
 */
  private long totalIn;
  /** 
 * The offset into inputBuf, where input data starts. 
 */
  private int inputOff;
  /** 
 * The end offset of the input data. 
 */
  private int inputEnd;
  private DeflaterPending pending;
  private DeflaterHuffman huffman;
  DeflaterEngine(  DeflaterPending pending){
    this.pending=pending;
    huffman=new DeflaterHuffman(pending);
    this.hook26();
    window=new byte[2 * WSIZE];
    head=new short[HASH_SIZE];
    prev=new short[WSIZE];
    blockStart=strstart=1;
  }
  public void reset(){
    huffman.reset();
    this.hook27();
    blockStart=strstart=1;
    lookahead=0;
    totalIn=0;
    prevAvailable=false;
    matchLen=MIN_MATCH - 1;
    for (int i=0; i < HASH_SIZE; i++)     head[i]=0;
    for (int i=0; i < WSIZE; i++)     prev[i]=0;
  }
  public final long getTotalIn(){
    return totalIn;
  }
  public final void setStrategy(  int strat){
    strategy=strat;
  }
  public void setLevel(  int lvl){
    goodLength=DeflaterConstants.GOOD_LENGTH[lvl];
    max_lazy=DeflaterConstants.MAX_LAZY[lvl];
    niceLength=DeflaterConstants.NICE_LENGTH[lvl];
    max_chain=DeflaterConstants.MAX_CHAIN[lvl];
    if (DeflaterConstants.COMPR_FUNC[lvl] != comprFunc) {
      if (DeflaterConstants.DEBUGGING)       System.err.println("Change from " + comprFunc + " to "+ DeflaterConstants.COMPR_FUNC[lvl]);
switch (comprFunc) {
case DEFLATE_STORED:
        if (strstart > blockStart) {
          huffman.flushStoredBlock(window,blockStart,strstart - blockStart,false);
          blockStart=strstart;
        }
      updateHash();
    break;
case DEFLATE_FAST:
  if (strstart > blockStart) {
    huffman.flushBlock(window,blockStart,strstart - blockStart,false);
    blockStart=strstart;
  }
break;
case DEFLATE_SLOW:
if (prevAvailable) huffman.tallyLit(window[strstart - 1] & 0xff);
if (strstart > blockStart) {
huffman.flushBlock(window,blockStart,strstart - blockStart,false);
blockStart=strstart;
}
prevAvailable=false;
matchLen=MIN_MATCH - 1;
break;
}
comprFunc=COMPR_FUNC[lvl];
}
}
private void updateHash(){
if (DEBUGGING) System.err.println("updateHash: " + strstart);
ins_h=(window[strstart] << HASH_SHIFT) ^ window[strstart + 1];
}
/** 
 * Inserts the current string in the head hash and returns the previous
 * value for this hash.
 */
private int insertString(){
short match;
int hash=((ins_h << HASH_SHIFT) ^ window[strstart + (MIN_MATCH - 1)]) & HASH_MASK;
if (DEBUGGING) {
if (hash != (((window[strstart] << (2 * HASH_SHIFT)) ^ (window[strstart + 1] << HASH_SHIFT) ^ (window[strstart + 2])) & HASH_MASK)) throw new Error("hash inconsistent: " + hash + "/"+ window[strstart]+ ","+ window[strstart + 1]+ ","+ window[strstart + 2]+ ","+ HASH_SHIFT);
}
prev[strstart & WMASK]=match=head[hash];
head[hash]=(short)strstart;
ins_h=hash;
return match & 0xffff;
}
private void slideWindow(){
System.arraycopy(window,WSIZE,window,0,WSIZE);
matchStart-=WSIZE;
strstart-=WSIZE;
blockStart-=WSIZE;
for (int i=0; i < HASH_SIZE; i++) {
int m=head[i] & 0xffff;
head[i]=m >= WSIZE ? (short)(m - WSIZE) : 0;
}
for (int i=0; i < WSIZE; i++) {
int m=prev[i] & 0xffff;
prev[i]=m >= WSIZE ? (short)(m - WSIZE) : 0;
}
}
/** 
 * Fill the window when the lookahead becomes insufficient.
 * Updates strstart and lookahead.
 * OUT assertions: strstart + lookahead <= 2*WSIZE
 * lookahead >= MIN_LOOKAHEAD or inputOff == inputEnd
 */
private void fillWindow(){
if (strstart >= WSIZE + MAX_DIST) slideWindow();
while (lookahead < DeflaterConstants.MIN_LOOKAHEAD && inputOff < inputEnd) {
int more=2 * WSIZE - lookahead - strstart;
if (more > inputEnd - inputOff) more=inputEnd - inputOff;
System.arraycopy(inputBuf,inputOff,window,strstart + lookahead,more);
this.hook28(more);
inputOff+=more;
totalIn+=more;
lookahead+=more;
}
if (lookahead >= MIN_MATCH) updateHash();
}
/** 
 * Find the best (longest) string in the window matching the 
 * string starting at strstart.
 * Preconditions:
 * strstart + MAX_MATCH <= window.length.
 * @param curMatch
 */
private boolean findLongestMatch(int curMatch){
int chainLength=this.max_chain;
int niceLength=this.niceLength;
short[] prev=this.prev;
int scan=this.strstart;
int match;
int best_end=this.strstart + matchLen;
int best_len=Math.max(matchLen,MIN_MATCH - 1);
int limit=Math.max(strstart - MAX_DIST,0);
int strend=scan + MAX_MATCH - 1;
byte scan_end1=window[best_end - 1];
byte scan_end=window[best_end];
if (best_len >= this.goodLength) chainLength>>=2;
if (niceLength > lookahead) niceLength=lookahead;
if (DeflaterConstants.DEBUGGING && strstart > 2 * WSIZE - MIN_LOOKAHEAD) throw new Error("need lookahead");
do {
if (DeflaterConstants.DEBUGGING && curMatch >= strstart) throw new Error("future match");
if (window[curMatch + best_len] != scan_end || window[curMatch + best_len - 1] != scan_end1 || window[curMatch] != window[scan] || window[curMatch + 1] != window[scan + 1]) continue;
match=curMatch + 2;
scan+=2;
while (window[++scan] == window[++match] && window[++scan] == window[++match] && window[++scan] == window[++match] && window[++scan] == window[++match] && window[++scan] == window[++match] && window[++scan] == window[++match] && window[++scan] == window[++match] && window[++scan] == window[++match] && scan < strend) ;
if (scan > best_end) {
matchStart=curMatch;
best_end=scan;
best_len=scan - strstart;
if (best_len >= niceLength) break;
scan_end1=window[best_end - 1];
scan_end=window[best_end];
}
scan=strstart;
}
 while ((curMatch=(prev[curMatch & WMASK] & 0xffff)) > limit && --chainLength != 0);
matchLen=Math.min(best_len,lookahead);
return matchLen >= MIN_MATCH;
}
void setDictionary(byte[] buffer,int offset,int length){
if (DeflaterConstants.DEBUGGING && strstart != 1) throw new IllegalStateException("strstart not 1");
this.hook29(buffer,offset,length);
if (length < MIN_MATCH) return;
if (length > MAX_DIST) {
offset+=length - MAX_DIST;
length=MAX_DIST;
}
System.arraycopy(buffer,offset,window,strstart,length);
updateHash();
length--;
while (--length > 0) {
insertString();
strstart++;
}
strstart+=2;
blockStart=strstart;
}
private boolean deflateStored(boolean flush,boolean finish){
if (!flush && lookahead == 0) return false;
strstart+=lookahead;
lookahead=0;
int storedLen=strstart - blockStart;
if ((storedLen >= DeflaterConstants.MAX_BLOCK_SIZE) || (blockStart < WSIZE && storedLen >= MAX_DIST) || flush) {
boolean lastBlock=finish;
if (storedLen > DeflaterConstants.MAX_BLOCK_SIZE) {
storedLen=DeflaterConstants.MAX_BLOCK_SIZE;
lastBlock=false;
}
if (DeflaterConstants.DEBUGGING) System.err.println("storedBlock[" + storedLen + ","+ lastBlock+ "]");
huffman.flushStoredBlock(window,blockStart,storedLen,lastBlock);
blockStart+=storedLen;
return !lastBlock;
}
return true;
}
private boolean deflateFast(boolean flush,boolean finish){
if (lookahead < MIN_LOOKAHEAD && !flush) return false;
while (lookahead >= MIN_LOOKAHEAD || flush) {
if (lookahead == 0) {
huffman.flushBlock(window,blockStart,strstart - blockStart,finish);
blockStart=strstart;
return false;
}
if (strstart > 2 * WSIZE - MIN_LOOKAHEAD) {
slideWindow();
}
int hashHead;
if (lookahead >= MIN_MATCH && (hashHead=insertString()) != 0 && strategy != Deflater.HUFFMAN_ONLY && strstart - hashHead <= MAX_DIST && findLongestMatch(hashHead)) {
if (DeflaterConstants.DEBUGGING) {
for (int i=0; i < matchLen; i++) {
if (window[strstart + i] != window[matchStart + i]) throw new Error();
}
}
boolean full=huffman.tallyDist(strstart - matchStart,matchLen);
lookahead-=matchLen;
if (matchLen <= max_lazy && lookahead >= MIN_MATCH) {
while (--matchLen > 0) {
strstart++;
insertString();
}
strstart++;
}
 else {
strstart+=matchLen;
if (lookahead >= MIN_MATCH - 1) updateHash();
}
matchLen=MIN_MATCH - 1;
if (!full) continue;
}
 else {
huffman.tallyLit(window[strstart] & 0xff);
strstart++;
lookahead--;
}
if (huffman.isFull()) {
boolean lastBlock=finish && lookahead == 0;
huffman.flushBlock(window,blockStart,strstart - blockStart,lastBlock);
blockStart=strstart;
return !lastBlock;
}
}
return true;
}
private boolean deflateSlow(boolean flush,boolean finish){
if (lookahead < MIN_LOOKAHEAD && !flush) return false;
while (lookahead >= MIN_LOOKAHEAD || flush) {
if (lookahead == 0) {
if (prevAvailable) huffman.tallyLit(window[strstart - 1] & 0xff);
prevAvailable=false;
if (DeflaterConstants.DEBUGGING && !flush) throw new Error("Not flushing, but no lookahead");
huffman.flushBlock(window,blockStart,strstart - blockStart,finish);
blockStart=strstart;
return false;
}
if (strstart >= 2 * WSIZE - MIN_LOOKAHEAD) {
slideWindow();
}
int prevMatch=matchStart;
int prevLen=matchLen;
if (lookahead >= MIN_MATCH) {
int hashHead=insertString();
if (strategy != Deflater.HUFFMAN_ONLY && hashHead != 0 && strstart - hashHead <= MAX_DIST && findLongestMatch(hashHead)) {
if (matchLen <= 5 && (strategy == Deflater.FILTERED || (matchLen == MIN_MATCH && strstart - matchStart > TOO_FAR))) {
matchLen=MIN_MATCH - 1;
}
}
}
if (prevLen >= MIN_MATCH && matchLen <= prevLen) {
if (DeflaterConstants.DEBUGGING) {
for (int i=0; i < matchLen; i++) {
if (window[strstart - 1 + i] != window[prevMatch + i]) throw new Error();
}
}
huffman.tallyDist(strstart - 1 - prevMatch,prevLen);
prevLen-=2;
do {
strstart++;
lookahead--;
if (lookahead >= MIN_MATCH) insertString();
}
 while (--prevLen > 0);
strstart++;
lookahead--;
prevAvailable=false;
matchLen=MIN_MATCH - 1;
}
 else {
if (prevAvailable) huffman.tallyLit(window[strstart - 1] & 0xff);
prevAvailable=true;
strstart++;
lookahead--;
}
if (huffman.isFull()) {
int len=strstart - blockStart;
if (prevAvailable) len--;
boolean lastBlock=(finish && lookahead == 0 && !prevAvailable);
huffman.flushBlock(window,blockStart,len,lastBlock);
blockStart+=len;
return !lastBlock;
}
}
return true;
}
public boolean deflate(boolean flush,boolean finish){
boolean progress;
do {
fillWindow();
boolean canFlush=flush && inputOff == inputEnd;
if (DeflaterConstants.DEBUGGING) System.err.println("window: [" + blockStart + ","+ strstart+ ","+ lookahead+ "], "+ comprFunc+ ","+ canFlush);
switch (comprFunc) {
case DEFLATE_STORED:
progress=deflateStored(canFlush,finish);
break;
case DEFLATE_FAST:
progress=deflateFast(canFlush,finish);
break;
case DEFLATE_SLOW:
progress=deflateSlow(canFlush,finish);
break;
default :
throw new Error();
}
}
 while (pending.isFlushed() && progress);
return progress;
}
public void setInput(byte[] buf,int off,int len){
if (inputOff < inputEnd) throw new IllegalStateException("Old input was not completely processed");
int end=off + len;
if (0 > off || off > end || end > buf.length) throw new ArrayIndexOutOfBoundsException();
inputBuf=buf;
inputOff=off;
inputEnd=end;
}
public final boolean needsInput(){
return inputEnd == inputOff;
}
protected void hook26(){
}
protected void hook27(){
}
protected void hook28(int more){
}
protected void hook29(byte[] buffer,int offset,int length){
}
}
