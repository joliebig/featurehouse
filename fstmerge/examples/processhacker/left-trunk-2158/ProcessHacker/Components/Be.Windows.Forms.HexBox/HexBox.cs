using System;
using System.Drawing;
using System.Windows.Forms;
using System.Runtime.InteropServices;
using System.ComponentModel;
using System.Security.Permissions;
using System.Windows.Forms.VisualStyles;
using Be.Windows.Forms.Design;

namespace Be.Windows.Forms
{




 public enum HexCasing
 {



  Upper = 0,



  Lower = 1
 }






 struct BytePositionInfo
 {
  public BytePositionInfo(long index, int characterPosition)
  {
   _index = index;
   _characterPosition = characterPosition;
  }

  public int CharacterPosition
  {
   get { return _characterPosition; }
  } int _characterPosition;

  public long Index
  {
   get { return _index; }
  } long _index;
 }





 [ToolboxBitmap(typeof(HexBox), "HexBox.bmp")]
 public class HexBox : Control
 {




  interface IKeyInterpreter
  {



   void Activate();



   void Deactivate();





   bool PreProcessWmKeyUp(ref Message m);





   bool PreProcessWmChar(ref Message m);





   bool PreProcessWmKeyDown(ref Message m);





   PointF GetCaretPointF(long byteIndex);
  }







  class EmptyKeyInterpreter : IKeyInterpreter
  {
   HexBox _hexBox;

   public EmptyKeyInterpreter(HexBox hexBox)
   {
    _hexBox = hexBox;
   }


   public void Activate(){}
   public void Deactivate(){}

   public bool PreProcessWmKeyUp(ref Message m)
   { return _hexBox.BasePreProcessMessage(ref m); }

   public bool PreProcessWmChar(ref Message m)
   { return _hexBox.BasePreProcessMessage(ref m); }

   public bool PreProcessWmKeyDown(ref Message m)
   { return _hexBox.BasePreProcessMessage(ref m); }

   public PointF GetCaretPointF(long byteIndex)
   { return new PointF (); }


  }






  class KeyInterpreter : IKeyInterpreter
  {




   protected HexBox _hexBox;




   protected bool _shiftDown;



   bool _mouseDown;



   BytePositionInfo _bpiStart;



   BytePositionInfo _bpi;



   public KeyInterpreter(HexBox hexBox)
   {
    _hexBox = hexBox;
   }



   public virtual void Activate()
   {
    _hexBox.MouseDown += new MouseEventHandler(BeginMouseSelection);
    _hexBox.MouseMove += new MouseEventHandler(UpdateMouseSelection);
    _hexBox.MouseUp += new MouseEventHandler(EndMouseSelection);
   }

   public virtual void Deactivate()
   {
    _hexBox.MouseDown -= new MouseEventHandler(BeginMouseSelection);
    _hexBox.MouseMove -= new MouseEventHandler(UpdateMouseSelection);
    _hexBox.MouseUp -= new MouseEventHandler(EndMouseSelection);
   }



   void BeginMouseSelection(object sender, MouseEventArgs e)
   {
    System.Diagnostics.Debug.WriteLine("BeginMouseSelection()", "KeyInterpreter");

    _mouseDown = true;

    if(!_shiftDown)
    {
     _bpiStart = new BytePositionInfo(_hexBox._bytePos, _hexBox._byteCharacterPos);
     _hexBox.ReleaseSelection();
    }
    else
    {
     UpdateMouseSelection(this, e);
    }
   }

   void UpdateMouseSelection(object sender, MouseEventArgs e)
   {
    if(!_mouseDown)
     return;

    _bpi = GetBytePositionInfo(new Point(e.X, e.Y));
    long selEnd = _bpi.Index;
    long realselStart;
    long realselLength;

    if(selEnd < _bpiStart.Index)
    {
     realselStart = selEnd;
     realselLength = _bpiStart.Index - selEnd;
    }
    else if(selEnd > _bpiStart.Index)
    {
     realselStart = _bpiStart.Index;
     realselLength = selEnd - realselStart;
    }
    else
    {
     realselStart = _hexBox._bytePos;
     realselLength = 0;
    }

    if(realselStart != _hexBox._bytePos || realselLength != _hexBox._selectionLength)
    {
     _hexBox.InternalSelect(realselStart, realselLength);
    }
   }

   void EndMouseSelection(object sender, MouseEventArgs e)
   {
    _mouseDown = false;
   }



   public virtual bool PreProcessWmKeyDown(ref Message m)
   {
    System.Diagnostics.Debug.WriteLine("PreProcessWmKeyDown(ref Message m)", "KeyInterpreter");

    Keys vc = (Keys)m.WParam.ToInt32();

    Keys keyData = vc | Control.ModifierKeys;

    switch(keyData)
    {
     case Keys.Left:
     case Keys.Up:
     case Keys.Right:
     case Keys.Down:
     case Keys.PageUp:
     case Keys.PageDown:
     case Keys.Left | Keys.Shift:
     case Keys.Up | Keys.Shift:
     case Keys.Right | Keys.Shift:
     case Keys.Down | Keys.Shift:
     case Keys.Tab:
     case Keys.Back:
     case Keys.Delete:
     case Keys.Home:
     case Keys.End:
     case Keys.ShiftKey | Keys.Shift:
     case Keys.C | Keys.Control:
     case Keys.X | Keys.Control:
     case Keys.V | Keys.Control:
      if(RaiseKeyDown(keyData))
       return true;
      break;
    }

    switch(keyData)
    {
     case Keys.Left:
      return PreProcessWmKeyDown_Left(ref m);
     case Keys.Up:
      return PreProcessWmKeyDown_Up(ref m);
     case Keys.Right:
      return PreProcessWmKeyDown_Right(ref m);
     case Keys.Down:
      return PreProcessWmKeyDown_Down(ref m);
     case Keys.PageUp:
      return PreProcessWmKeyDown_PageUp(ref m);
     case Keys.PageDown:
      return PreProcessWmKeyDown_PageDown(ref m);
     case Keys.Left | Keys.Shift:
      return PreProcessWmKeyDown_ShiftLeft(ref m);
     case Keys.Up | Keys.Shift:
      return PreProcessWmKeyDown_ShiftUp(ref m);
     case Keys.Right | Keys.Shift:
      return PreProcessWmKeyDown_ShiftRight(ref m);
     case Keys.Down | Keys.Shift:
      return PreProcessWmKeyDown_ShiftDown(ref m);
     case Keys.Tab:
      return PreProcessWmKeyDown_Tab(ref m);
     case Keys.Back:
      return PreProcessWmKeyDown_Back(ref m);
     case Keys.Delete:
      return PreProcessWmKeyDown_Delete(ref m);
     case Keys.Home:
      return PreProcessWmKeyDown_Home(ref m);
     case Keys.End:
      return PreProcessWmKeyDown_End(ref m);
     case Keys.ShiftKey | Keys.Shift:
      return PreProcessWmKeyDown_ShiftShiftKey(ref m);
     case Keys.C | Keys.Control:
      return PreProcessWmKeyDown_ControlC(ref m);
     case Keys.X | Keys.Control:
      return PreProcessWmKeyDown_ControlX(ref m);
     case Keys.V | Keys.Control:
      return PreProcessWmKeyDown_ControlV(ref m);
     default:
      _hexBox.ScrollByteIntoView();
      return _hexBox.BasePreProcessMessage(ref m);
    }
   }

   protected bool RaiseKeyDown(Keys keyData)
   {
    KeyEventArgs e = new KeyEventArgs(keyData);
    _hexBox.OnKeyDown(e);
    return e.Handled;
   }

   protected virtual bool PreProcessWmKeyDown_Left(ref Message m)
   {
    return PerformPosMoveLeft();
   }

   protected virtual bool PreProcessWmKeyDown_Up(ref Message m)
   {
    long pos = _hexBox._bytePos;
    int cp = _hexBox._byteCharacterPos;

    if( !(pos == 0 && cp == 0) )
    {
     pos = Math.Max(-1, pos-_hexBox._iHexMaxHBytes);
     if(pos == -1)
      return true;

     _hexBox.SetPosition(pos);

     if(pos < _hexBox._startByte)
     {
      _hexBox.PerformScrollLineUp();
     }

     _hexBox.UpdateCaret();
     _hexBox.Invalidate();
    }

    _hexBox.ScrollByteIntoView();
    _hexBox.ReleaseSelection();

    return true;
   }

   protected virtual bool PreProcessWmKeyDown_Right(ref Message m)
   {
    return PerformPosMoveRight();
   }

   protected virtual bool PreProcessWmKeyDown_Down(ref Message m)
   {
    long pos = _hexBox._bytePos;
    int cp = _hexBox._byteCharacterPos;

    if(pos == _hexBox._byteProvider.Length && cp == 0)
     return true;

    pos = Math.Min(_hexBox._byteProvider.Length, pos+_hexBox._iHexMaxHBytes);

    if(pos == _hexBox._byteProvider.Length)
     cp = 0;

    _hexBox.SetPosition(pos, cp);

    if(pos > _hexBox._endByte-1)
    {
     _hexBox.PerformScrollLineDown();
    }

    _hexBox.UpdateCaret();
    _hexBox.ScrollByteIntoView();
    _hexBox.ReleaseSelection();
    _hexBox.Invalidate();

    return true;
   }

   protected virtual bool PreProcessWmKeyDown_PageUp(ref Message m)
   {
    long pos = _hexBox._bytePos;
    int cp = _hexBox._byteCharacterPos;

    if(pos == 0 && cp == 0)
     return true;

    pos = Math.Max(0, pos-_hexBox._iHexMaxBytes);
    if(pos == 0)
     return true;

    _hexBox.SetPosition(pos);

    if(pos < _hexBox._startByte)
    {
     _hexBox.PerformScrollPageUp();
    }

    _hexBox.ReleaseSelection();
    _hexBox.UpdateCaret();
    _hexBox.Invalidate();
    return true;
   }

   protected virtual bool PreProcessWmKeyDown_PageDown(ref Message m)
   {
    long pos = _hexBox._bytePos;
    int cp = _hexBox._byteCharacterPos;

    if(pos == _hexBox._byteProvider.Length && cp == 0)
     return true;

    pos = Math.Min(_hexBox._byteProvider.Length, pos+_hexBox._iHexMaxBytes);

    if(pos == _hexBox._byteProvider.Length)
     cp = 0;

    _hexBox.SetPosition(pos, cp);

    if(pos > _hexBox._endByte-1)
    {
     _hexBox.PerformScrollPageDown();
    }

    _hexBox.ReleaseSelection();
    _hexBox.UpdateCaret();
    _hexBox.Invalidate();

    return true;
   }

   protected virtual bool PreProcessWmKeyDown_ShiftLeft(ref Message m)
   {
    long pos = _hexBox._bytePos;
    long sel = _hexBox._selectionLength;

    if(pos + sel < 1)
     return true;

    if(pos+sel <= _bpiStart.Index)
    {
     if(pos == 0)
      return true;

     pos--;
     sel++;
    }
    else
    {
     sel = Math.Max(0, sel-1);
    }

    _hexBox.ScrollByteIntoView();
    _hexBox.InternalSelect(pos, sel);

    return true;
   }

   protected virtual bool PreProcessWmKeyDown_ShiftUp(ref Message m)
   {
    long pos = _hexBox._bytePos;
    long sel = _hexBox._selectionLength;

    if(pos-_hexBox._iHexMaxHBytes < 0 && pos <= _bpiStart.Index)
     return true;

    if(_bpiStart.Index >= pos+sel)
    {
     pos = pos - _hexBox._iHexMaxHBytes;
     sel += _hexBox._iHexMaxHBytes;
     _hexBox.InternalSelect(pos, sel);
     _hexBox.ScrollByteIntoView();
    }
    else
    {
     sel -= _hexBox._iHexMaxHBytes;
     if(sel < 0)
     {
      pos = _bpiStart.Index + sel;
      sel = -sel;
      _hexBox.InternalSelect(pos, sel);
      _hexBox.ScrollByteIntoView();
     }
     else
     {
      sel -= _hexBox._iHexMaxHBytes;
      _hexBox.InternalSelect(pos, sel);
      _hexBox.ScrollByteIntoView(pos+sel);
     }
    }

    return true;
   }

   protected virtual bool PreProcessWmKeyDown_ShiftRight(ref Message m)
   {
    long pos = _hexBox._bytePos;
    long sel = _hexBox._selectionLength;

    if(pos+sel >= _hexBox._byteProvider.Length)
     return true;

    if(_bpiStart.Index <= pos)
    {
     sel++;
     _hexBox.InternalSelect(pos, sel);
     _hexBox.ScrollByteIntoView(pos+sel);
    }
    else
    {
     pos++;
     sel = Math.Max(0, sel-1);
     _hexBox.InternalSelect(pos, sel);
     _hexBox.ScrollByteIntoView();
    }

    return true;
   }

   protected virtual bool PreProcessWmKeyDown_ShiftDown(ref Message m)
   {
    long pos = _hexBox._bytePos;
    long sel = _hexBox._selectionLength;

    long max = _hexBox._byteProvider.Length;

    if(pos+sel+_hexBox._iHexMaxHBytes > max)
     return true;

    if(_bpiStart.Index <= pos)
    {
     sel += _hexBox._iHexMaxHBytes;
     _hexBox.InternalSelect(pos, sel);
     _hexBox.ScrollByteIntoView(pos+sel);
    }
    else
    {
     sel -= _hexBox._iHexMaxHBytes;
     if(sel < 0)
     {
      pos = _bpiStart.Index;
      sel = -sel;
     }
     else
     {
      pos += _hexBox._iHexMaxHBytes;
      sel -= _hexBox._iHexMaxHBytes;
     }

     _hexBox.InternalSelect(pos, sel);
     _hexBox.ScrollByteIntoView();
    }

    return true;
   }

   protected virtual bool PreProcessWmKeyDown_Tab(ref Message m)
   {
    if(_hexBox._stringViewVisible && _hexBox._keyInterpreter.GetType() == typeof(KeyInterpreter))
    {
     _hexBox.ActivateStringKeyInterpreter();
     _hexBox.ScrollByteIntoView();
     _hexBox.ReleaseSelection();
     _hexBox.UpdateCaret();
     _hexBox.Invalidate();
     return true;
    }

    if(_hexBox.Parent == null) return true;
    _hexBox.Parent.SelectNextControl(_hexBox, true, true, true, true);
    return true;
   }

   protected virtual bool PreProcessWmKeyDown_ShiftTab(ref Message m)
   {
    if(_hexBox._keyInterpreter is StringKeyInterpreter)
    {
     _shiftDown = false;
     _hexBox.ActivateKeyInterpreter();
     _hexBox.ScrollByteIntoView();
     _hexBox.ReleaseSelection();
     _hexBox.UpdateCaret();
     _hexBox.Invalidate();
     return true;
    }

    if(_hexBox.Parent == null) return true;
    _hexBox.Parent.SelectNextControl(_hexBox, false, true, true, true);
    return true;
   }

   protected virtual bool PreProcessWmKeyDown_Back(ref Message m)
   {
    if(!_hexBox._byteProvider.SupportsDeleteBytes())
     return true;

    long pos = _hexBox._bytePos;
    long sel = _hexBox._selectionLength;
    int cp = _hexBox._byteCharacterPos;

    long startDelete = (cp == 0 && sel == 0) ? pos-1 : pos;
    if(startDelete < 0 && sel < 1)
     return true;

    long bytesToDelete = (sel > 0) ? sel : 1;
    _hexBox._byteProvider.DeleteBytes(Math.Max(0, startDelete), bytesToDelete);
    _hexBox.UpdateScrollSize();

    if(sel == 0)
     PerformPosMoveLeftByte();

    _hexBox.ReleaseSelection();
    _hexBox.Invalidate();

    return true;
   }

   protected virtual bool PreProcessWmKeyDown_Delete(ref Message m)
   {
    if(!_hexBox._byteProvider.SupportsDeleteBytes())
     return true;

    long pos = _hexBox._bytePos;
    long sel = _hexBox._selectionLength;

    if(pos >= _hexBox._byteProvider.Length)
     return true;

    long bytesToDelete = (sel > 0) ? sel : 1;
    _hexBox._byteProvider.DeleteBytes(pos, bytesToDelete);

    _hexBox.UpdateScrollSize();
    _hexBox.ReleaseSelection();
    _hexBox.Invalidate();

    return true;
   }

   protected virtual bool PreProcessWmKeyDown_Home(ref Message m)
   {
    long pos = _hexBox._bytePos;
    int cp = _hexBox._byteCharacterPos;

    if(pos < 1)
     return true;

    pos = 0;
    cp = 0;
    _hexBox.SetPosition(pos, cp);

    _hexBox.ScrollByteIntoView();
    _hexBox.UpdateCaret();
    _hexBox.ReleaseSelection();

    return true;
   }

   protected virtual bool PreProcessWmKeyDown_End(ref Message m)
   {
    long pos = _hexBox._bytePos;
    int cp = _hexBox._byteCharacterPos;

    if(pos >= _hexBox._byteProvider.Length-1)
     return true;

    pos = _hexBox._byteProvider.Length;
    cp = 0;
    _hexBox.SetPosition(pos, cp);

    _hexBox.ScrollByteIntoView();
    _hexBox.UpdateCaret();
    _hexBox.ReleaseSelection();

    return true;
   }

   protected virtual bool PreProcessWmKeyDown_ShiftShiftKey(ref Message m)
   {
    if(_mouseDown)
     return true;
    if(_shiftDown)
     return true;

    _shiftDown = true;

    if(_hexBox._selectionLength > 0)
     return true;

    _bpiStart = new BytePositionInfo(_hexBox._bytePos, _hexBox._byteCharacterPos);

    return true;
   }

   protected virtual bool PreProcessWmKeyDown_ControlC(ref Message m)
   {
    _hexBox.Copy();
    return true;
   }

   protected virtual bool PreProcessWmKeyDown_ControlX(ref Message m)
   {
    _hexBox.Cut();
    return true;
   }

   protected virtual bool PreProcessWmKeyDown_ControlV(ref Message m)
   {
    _hexBox.Paste();
    return true;
   }




   public virtual bool PreProcessWmChar(ref Message m)
   {
    if(Control.ModifierKeys == Keys.Control)
    {
     return _hexBox.BasePreProcessMessage(ref m);
    }

    bool sw = _hexBox._byteProvider.SupportsWriteByte();
    bool si = _hexBox._byteProvider.SupportsInsertBytes();
    bool sd = _hexBox._byteProvider.SupportsDeleteBytes();

    long pos = _hexBox._bytePos;
    long sel = _hexBox._selectionLength;
    int cp = _hexBox._byteCharacterPos;

    if(
     (!sw && pos != _hexBox._byteProvider.Length) ||
     (!si && pos == _hexBox._byteProvider.Length))
    {
     return _hexBox.BasePreProcessMessage(ref m);
    }

    char c = (char)m.WParam.ToInt32();

    if(Uri.IsHexDigit(c))
    {
     if(RaiseKeyPress(c))
      return true;

     if(_hexBox.ReadOnly)
      return true;

     bool isInsertMode = (pos == _hexBox._byteProvider.Length);


     if(!isInsertMode && si && _hexBox._insertActive && cp == 0)
      isInsertMode = true;

     if(sd && si && sel > 0)
     {
      _hexBox._byteProvider.DeleteBytes(pos, sel);
      isInsertMode = true;
      cp = 0;
      _hexBox.SetPosition(pos, cp);
     }

     _hexBox.ReleaseSelection();

     byte currentByte;
     if(isInsertMode)
      currentByte = 0;
     else
      currentByte = _hexBox._byteProvider.ReadByte(pos);

     string sCb = currentByte.ToString("X", System.Threading.Thread.CurrentThread.CurrentCulture);
     if(sCb.Length == 1)
      sCb = "0" + sCb;

     string sNewCb = c.ToString();
     if(cp == 0)
      sNewCb += sCb.Substring(1, 1);
     else
      sNewCb = sCb.Substring(0, 1) + sNewCb;
     byte newcb = byte.Parse(sNewCb, System.Globalization.NumberStyles.AllowHexSpecifier, System.Threading.Thread.CurrentThread.CurrentCulture);
     if(isInsertMode)
      _hexBox._byteProvider.InsertBytes(pos, new byte[]{newcb});
     else
      _hexBox._byteProvider.WriteByte(pos, newcb);

     PerformPosMoveRight();

     _hexBox.Invalidate();
     return true;
    }
    else
    {
     return _hexBox.BasePreProcessMessage(ref m);
    }
   }

   protected bool RaiseKeyPress(char keyChar)
   {
    KeyPressEventArgs e = new KeyPressEventArgs(keyChar);
    _hexBox.OnKeyPress(e);
    return e.Handled;
   }



   public virtual bool PreProcessWmKeyUp(ref Message m)
   {
    System.Diagnostics.Debug.WriteLine("PreProcessWmKeyUp(ref Message m)", "KeyInterpreter");

    Keys vc = (Keys)m.WParam.ToInt32();

    Keys keyData = vc | Control.ModifierKeys;

    switch(keyData)
    {
     case Keys.ShiftKey:
     case Keys.Insert:
      if(RaiseKeyUp(keyData))
       return true;
      break;
    }

    switch(keyData)
    {
     case Keys.ShiftKey:
      _shiftDown = false;
      return true;
     case Keys.Insert:
      return PreProcessWmKeyUp_Insert(ref m);
     default:
      return _hexBox.BasePreProcessMessage(ref m);
    }
   }

   protected virtual bool PreProcessWmKeyUp_Insert(ref Message m)
   {
    _hexBox._insertActive = !_hexBox._insertActive;
    _hexBox.OnInsertActiveChanged(EventArgs.Empty);
    return true;
   }

   protected bool RaiseKeyUp(Keys keyData)
   {
    KeyEventArgs e = new KeyEventArgs(keyData);
    _hexBox.OnKeyUp(e);
    return e.Handled;
   }



   protected virtual bool PerformPosMoveLeft()
   {
    long pos = _hexBox._bytePos;
    long sel = _hexBox._selectionLength;
    int cp = _hexBox._byteCharacterPos;

    if(sel != 0)
    {
     cp = 0;
     _hexBox.SetPosition(pos, cp);
     _hexBox.ReleaseSelection();
    }
    else
    {
     if(pos == 0 && cp == 0)
      return true;

     if(cp > 0)
     {
      cp--;
     }
     else
     {
      pos = Math.Max(0, pos-1);
      cp++;
     }

     _hexBox.SetPosition(pos, cp);

     if(pos < _hexBox._startByte)
     {
      _hexBox.PerformScrollLineUp();
     }
     _hexBox.UpdateCaret();
     _hexBox.Invalidate();
    }

    _hexBox.ScrollByteIntoView();
    return true;
   }
   protected virtual bool PerformPosMoveRight()
   {
    long pos = _hexBox._bytePos;
    int cp = _hexBox._byteCharacterPos;
    long sel = _hexBox._selectionLength;

    if(sel != 0)
    {
     pos += sel;
     cp = 0;
     _hexBox.SetPosition(pos, cp);
     _hexBox.ReleaseSelection();
    }
    else
    {
     if( !(pos == _hexBox._byteProvider.Length && cp == 0) )
     {

      if(cp > 0)
      {
       pos = Math.Min(_hexBox._byteProvider.Length, pos+1);
       cp = 0;
      }
      else
      {
       cp++;
      }

      _hexBox.SetPosition(pos, cp);

      if(pos > _hexBox._endByte-1)
      {
       _hexBox.PerformScrollLineDown();
      }
      _hexBox.UpdateCaret();
      _hexBox.Invalidate();
     }
    }

    _hexBox.ScrollByteIntoView();
    return true;
   }
   protected virtual bool PerformPosMoveLeftByte()
   {
    long pos = _hexBox._bytePos;
    int cp = _hexBox._byteCharacterPos;

    if(pos == 0)
     return true;

    pos = Math.Max(0, pos-1);
    cp = 0;

    _hexBox.SetPosition(pos, cp);

    if(pos < _hexBox._startByte)
    {
     _hexBox.PerformScrollLineUp();
    }
    _hexBox.UpdateCaret();
    _hexBox.ScrollByteIntoView();
    _hexBox.Invalidate();

    return true;
   }

   protected virtual bool PerformPosMoveRightByte()
   {
    long pos = _hexBox._bytePos;
    int cp = _hexBox._byteCharacterPos;

    if(pos == _hexBox._byteProvider.Length)
     return true;

    pos = Math.Min(_hexBox._byteProvider.Length, pos+1);
    cp = 0;

    _hexBox.SetPosition(pos, cp);

    if(pos > _hexBox._endByte-1)
    {
     _hexBox.PerformScrollLineDown();
    }
    _hexBox.UpdateCaret();
    _hexBox.ScrollByteIntoView();
    _hexBox.Invalidate();

    return true;
   }


   public virtual PointF GetCaretPointF(long byteIndex)
   {
    System.Diagnostics.Debug.WriteLine("GetCaretPointF()", "KeyInterpreter");

    return _hexBox.GetBytePointF(byteIndex);
   }

   protected virtual BytePositionInfo GetBytePositionInfo(Point p)
   {
    return _hexBox.GetHexBytePositionInfo(p);
   }

  }






  class StringKeyInterpreter : KeyInterpreter
  {

   public StringKeyInterpreter(HexBox hexBox) : base(hexBox)
   {
    _hexBox._byteCharacterPos = 0;
   }



   public override bool PreProcessWmKeyDown(ref Message m)
   {
    Keys vc = (Keys)m.WParam.ToInt32();

    Keys keyData = vc | Control.ModifierKeys;

    switch(keyData)
    {
     case Keys.Tab | Keys.Shift:
     case Keys.Tab:
      if(RaiseKeyDown(keyData))
       return true;
      break;
    }

    switch(keyData)
    {
     case Keys.Tab | Keys.Shift:
      return PreProcessWmKeyDown_ShiftTab(ref m);
     case Keys.Tab:
      return PreProcessWmKeyDown_Tab(ref m);
     default:
      return base.PreProcessWmKeyDown(ref m);
    }
   }

   protected override bool PreProcessWmKeyDown_Left(ref Message m)
   {
    return PerformPosMoveLeftByte();
   }

   protected override bool PreProcessWmKeyDown_Right(ref Message m)
   {
    return PerformPosMoveRightByte();
   }




   public override bool PreProcessWmChar(ref Message m)
   {
    if(Control.ModifierKeys == Keys.Control)
    {
     return _hexBox.BasePreProcessMessage(ref m);
    }

    bool sw = _hexBox._byteProvider.SupportsWriteByte();
    bool si = _hexBox._byteProvider.SupportsInsertBytes();
    bool sd = _hexBox._byteProvider.SupportsDeleteBytes();

    long pos = _hexBox._bytePos;
    long sel = _hexBox._selectionLength;
    int cp = _hexBox._byteCharacterPos;

    if(
     (!sw && pos != _hexBox._byteProvider.Length) ||
     (!si && pos == _hexBox._byteProvider.Length))
    {
     return _hexBox.BasePreProcessMessage(ref m);
    }

    char c = (char)m.WParam.ToInt32();

    if(RaiseKeyPress(c))
     return true;

    if(_hexBox.ReadOnly)
     return true;

    bool isInsertMode = (pos == _hexBox._byteProvider.Length);


    if(!isInsertMode && si && _hexBox._insertActive)
     isInsertMode = true;

    if(sd && si && sel > 0)
    {
     _hexBox._byteProvider.DeleteBytes(pos, sel);
     isInsertMode = true;
     cp = 0;
     _hexBox.SetPosition(pos, cp);
    }

    _hexBox.ReleaseSelection();

    if(isInsertMode)
     _hexBox._byteProvider.InsertBytes(pos, new byte[]{(byte)c});
    else
     _hexBox._byteProvider.WriteByte(pos, (byte)c);

    PerformPosMoveRightByte();
    _hexBox.Invalidate();

    return true;
   }



   public override PointF GetCaretPointF(long byteIndex)
   {
    System.Diagnostics.Debug.WriteLine("GetCaretPointF()", "StringKeyInterpreter");

    Point gp = _hexBox.GetGridBytePoint(byteIndex);
    return _hexBox.GetByteStringPointF(gp);
   }

   protected override BytePositionInfo GetBytePositionInfo(Point p)
   {
    return _hexBox.GetStringBytePositionInfo(p);
   }

  }






  Rectangle _recContent;



  Rectangle _recLineInfo;



  Rectangle _recHex;



  Rectangle _recStringView;




  StringFormat _stringFormat;



  SizeF _charSize;




  int _iHexMaxHBytes;



  int _iHexMaxVBytes;



  int _iHexMaxBytes;




  long _scrollVmin;



  long _scrollVmax;



  long _scrollVpos;



  VScrollBar _vScrollBar;



        Timer _thumbTrackTimer = new Timer();



        long _thumbTrackPosition;



        const int THUMPTRACKDELAY = 50;



        int _lastThumbtrack;



  int _recBorderLeft = SystemInformation.Border3DSize.Width;



  int _recBorderRight = SystemInformation.Border3DSize.Width;



  int _recBorderTop = SystemInformation.Border3DSize.Height;



  int _recBorderBottom = SystemInformation.Border3DSize.Height;




  long _startByte;



  long _endByte;




  long _bytePos = -1;
  int _byteCharacterPos;
  string _hexStringFormat = "X";
  IKeyInterpreter _keyInterpreter;
  EmptyKeyInterpreter _eki;
  KeyInterpreter _ki;
  StringKeyInterpreter _ski;
  bool _caretVisible;
  bool _abortFind;
  long _findingPos;
  bool _insertActive;
  [Description("Occurs, when the value of InsertActive property has changed.")]
  public event EventHandler InsertActiveChanged;
  [Description("Occurs, when the value of ReadOnly property has changed.")]
  public event EventHandler ReadOnlyChanged;
  [Description("Occurs, when the value of ByteProvider property has changed.")]
  public event EventHandler ByteProviderChanged;
  [Description("Occurs, when the value of SelectionStart property has changed.")]
  public event EventHandler SelectionStartChanged;
  [Description("Occurs, when the value of SelectionLength property has changed.")]
  public event EventHandler SelectionLengthChanged;
  [Description("Occurs, when the value of LineInfoVisible property has changed.")]
  public event EventHandler LineInfoVisibleChanged;
  [Description("Occurs, when the value of StringViewVisible property has changed.")]
  public event EventHandler StringViewVisibleChanged;
  [Description("Occurs, when the value of BorderStyle property has changed.")]
  public event EventHandler BorderStyleChanged;
  [Description("Occurs, when the value of BytesPerLine property has changed.")]
  public event EventHandler BytesPerLineChanged;
  [Description("Occurs, when the value of UseFixedBytesPerLine property has changed.")]
  public event EventHandler UseFixedBytesPerLineChanged;
  [Description("Occurs, when the value of VScrollBarVisible property has changed.")]
  public event EventHandler VScrollBarVisibleChanged;
  [Description("Occurs, when the value of HexCasing property has changed.")]
  public event EventHandler HexCasingChanged;
  [Description("Occurs, when the value of HorizontalByteCount property has changed.")]
  public event EventHandler HorizontalByteCountChanged;
  [Description("Occurs, when the value of VerticalByteCount property has changed.")]
  public event EventHandler VerticalByteCountChanged;
  [Description("Occurs, when the value of CurrentLine property has changed.")]
  public event EventHandler CurrentLineChanged;
  [Description("Occurs, when the value of CurrentPositionInLine property has changed.")]
  public event EventHandler CurrentPositionInLineChanged;
  public HexBox()
  {
   this._vScrollBar = new VScrollBar();
   this._vScrollBar.Scroll += new ScrollEventHandler(_vScrollBar_Scroll);
   BackColor = Color.White;
   Font = new Font("Courier New", 9F, FontStyle.Regular, GraphicsUnit.Point, ((byte)(0)));
   _stringFormat = new StringFormat(StringFormat.GenericTypographic);
   _stringFormat.FormatFlags = StringFormatFlags.MeasureTrailingSpaces;
   ActivateEmptyKeyInterpreter();
   SetStyle(ControlStyles.UserPaint, true);
   SetStyle(ControlStyles.DoubleBuffer, true);
   SetStyle(ControlStyles.AllPaintingInWmPaint, true);
   SetStyle(ControlStyles.ResizeRedraw, true);
            _thumbTrackTimer.Interval = 50;
            _thumbTrackTimer.Tick += new EventHandler(PerformScrollThumbTrack);
  }
  void _vScrollBar_Scroll(object sender, ScrollEventArgs e)
  {
   switch(e.Type)
   {
    case ScrollEventType.Last:
     break;
    case ScrollEventType.EndScroll:
     break;
    case ScrollEventType.SmallIncrement:
     PerformScrollLineDown();
     break;
    case ScrollEventType.SmallDecrement:
     PerformScrollLineUp();
     break;
    case ScrollEventType.LargeIncrement:
     PerformScrollPageDown();
     break;
    case ScrollEventType.LargeDecrement:
     PerformScrollPageUp();
     break;
    case ScrollEventType.ThumbPosition:
     long lPos = FromScrollPos(e.NewValue);
     PerformScrollThumpPosition(lPos);
     break;
    case ScrollEventType.ThumbTrack:
                    if (_thumbTrackTimer.Enabled)
                        _thumbTrackTimer.Enabled = false;
                    int currentThumbTrack = System.Environment.TickCount;
                    if (currentThumbTrack - _lastThumbtrack > THUMPTRACKDELAY)
                    {
                        PerformScrollThumbTrack(null, null);
                        _lastThumbtrack = currentThumbTrack;
                        break;
                    }
                    _thumbTrackPosition = FromScrollPos(e.NewValue);
                    _thumbTrackTimer.Enabled = true;
     break;
    case ScrollEventType.First:
     break;
    default:
     break;
   }
   e.NewValue = ToScrollPos(_scrollVpos);
  }
        void PerformScrollThumbTrack(object sender, EventArgs e)
        {
            _thumbTrackTimer.Enabled = false;
            PerformScrollThumpPosition(_thumbTrackPosition);
            _lastThumbtrack = Environment.TickCount;
        }
  void UpdateScrollSize()
  {
   System.Diagnostics.Debug.WriteLine("UpdateScrollSize()", "HexBox");
   if(VScrollBarVisible && _byteProvider != null && _byteProvider.Length > 0 && _iHexMaxHBytes != 0)
   {
    long scrollmax = (long)Math.Ceiling((double)_byteProvider.Length / (double)_iHexMaxHBytes - (double)_iHexMaxVBytes);
    scrollmax = Math.Max(0, scrollmax);
    long scrollpos = _startByte / _iHexMaxHBytes;
    if(scrollmax == _scrollVmax && scrollpos == _scrollVpos)
     return;
    _scrollVmin = 0;
    _scrollVmax = scrollmax;
    _scrollVpos = Math.Min(scrollpos, scrollmax);
    UpdateVScroll();
   }
   else if(VScrollBarVisible)
   {
    _scrollVmin = 0;
    _scrollVmax = 0;
    _scrollVpos = 0;
    UpdateVScroll();
   }
  }
  void UpdateVScroll()
  {
   System.Diagnostics.Debug.WriteLine("UpdateVScroll()", "HexBox");
   int max = ToScrollMax(_scrollVmax);
   if(max > 0)
   {
    _vScrollBar.Minimum = 0;
    _vScrollBar.Maximum = max;
    _vScrollBar.Value = ToScrollPos(_scrollVpos);
    _vScrollBar.Enabled = true;
   }
   else
   {
    _vScrollBar.Enabled = false;
   }
  }
  int ToScrollPos(long value)
  {
   int max = 65535;
   if(_scrollVmax < max)
    return (int)value;
   else
   {
    double valperc = (double)value / (double)_scrollVmax * (double)100;
    int res = (int)Math.Floor((double)max / (double)100 * valperc);
    res = (int)Math.Max(_scrollVmin, res);
    res = (int)Math.Min(_scrollVmax, res);
    return res;
   }
  }
  long FromScrollPos(int value)
  {
   int max = 65535;
   if(_scrollVmax < max)
   {
    return (long)value;
   }
   else
   {
    double valperc = (double)value / (double)max * (double)100;
    long res = (int)Math.Floor((double)_scrollVmax / (double)100 * valperc);
    return res;
   }
  }
  int ToScrollMax(long value)
  {
   long max = 65535;
   if(value > max)
    return (int)max;
   else
    return (int)value;
  }
  void PerformScrollToLine(long pos)
  {
   if(pos < _scrollVmin || pos > _scrollVmax || pos == _scrollVpos )
    return;
   _scrollVpos = pos;
   UpdateVScroll();
   UpdateVisibilityBytes();
   UpdateCaret();
   Invalidate();
  }
  void PerformScrollLines(int lines)
  {
   long pos;
   if(lines > 0)
   {
    pos = Math.Min(_scrollVmax, _scrollVpos+lines);
   }
   else if(lines < 0)
   {
    pos = Math.Max(_scrollVmin, _scrollVpos+lines);
   }
   else
   {
    return;
   }
   PerformScrollToLine(pos);
  }
  void PerformScrollLineDown()
  {
   this.PerformScrollLines(1);
  }
  void PerformScrollLineUp()
  {
   this.PerformScrollLines(-1);
  }
  void PerformScrollPageDown()
  {
   this.PerformScrollLines(_iHexMaxVBytes);
  }
  void PerformScrollPageUp()
  {
   this.PerformScrollLines(-_iHexMaxVBytes);
  }
  void PerformScrollThumpPosition(long pos)
  {
   int difference = (_scrollVmax > 65535) ? 10 : 9;
   if(ToScrollPos(pos) == ToScrollMax(_scrollVmax)-difference)
    pos = _scrollVmax;
   PerformScrollToLine(pos);
  }
  public void ScrollByteIntoView()
  {
   System.Diagnostics.Debug.WriteLine("ScrollByteIntoView()", "HexBox");
   ScrollByteIntoView(_bytePos);
  }
  public void ScrollByteIntoView(long index)
  {
   System.Diagnostics.Debug.WriteLine("ScrollByteIntoView(long index)", "HexBox");
   if(_byteProvider == null || _keyInterpreter == null)
    return;
   if(index < _startByte)
   {
    long line = (long)Math.Floor((double)index / (double)_iHexMaxHBytes);
    PerformScrollThumpPosition(line);
   }
   else if(index > _endByte)
   {
    long line = (long)Math.Floor((double)index / (double)_iHexMaxHBytes);
    line -= _iHexMaxVBytes-1;
    PerformScrollThumpPosition(line);
   }
  }
  void ReleaseSelection()
  {
   System.Diagnostics.Debug.WriteLine("ReleaseSelection()", "HexBox");
   if(_selectionLength == 0)
    return;
   _selectionLength = 0;
   OnSelectionLengthChanged(EventArgs.Empty);
   if(!_caretVisible)
    CreateCaret();
   else
    UpdateCaret();
   Invalidate();
  }
  public void Select(long start, long length)
  {
   InternalSelect(start, length);
   ScrollByteIntoView();
  }
  void InternalSelect(long start, long length)
  {
   long pos = start;
   long sel = length;
   int cp = 0;
   if(sel > 0 && _caretVisible)
    DestroyCaret();
   else if(sel == 0 && !_caretVisible)
    CreateCaret();
   SetPosition(pos, cp);
   SetSelectionLength(sel);
   UpdateCaret();
   Invalidate();
  }
  void ActivateEmptyKeyInterpreter()
  {
   if(_eki == null)
    _eki = new EmptyKeyInterpreter(this);
   if(_eki == _keyInterpreter)
    return;
   if(_keyInterpreter != null)
    _keyInterpreter.Deactivate();
   _keyInterpreter = _eki;
   _keyInterpreter.Activate();
  }
  void ActivateKeyInterpreter()
  {
   if(_ki == null)
    _ki = new KeyInterpreter(this);
   if(_ki == _keyInterpreter)
    return;
   if(_keyInterpreter != null)
    _keyInterpreter.Deactivate();
   _keyInterpreter = _ki;
   _keyInterpreter.Activate();
  }
  void ActivateStringKeyInterpreter()
  {
   if(_ski == null)
    _ski = new StringKeyInterpreter(this);
   if(_ski == _keyInterpreter)
    return;
   if(_keyInterpreter != null)
    _keyInterpreter.Deactivate();
   _keyInterpreter = _ski;
   _keyInterpreter.Activate();
  }
  void CreateCaret()
  {
   if(_byteProvider == null || _keyInterpreter == null || _caretVisible || !this.Focused)
    return;
   System.Diagnostics.Debug.WriteLine("CreateCaret()", "HexBox");
   NativeMethods.CreateCaret(Handle, IntPtr.Zero, 1, (int)_charSize.Height);
   UpdateCaret();
   NativeMethods.ShowCaret(Handle);
   _caretVisible = true;
  }
  void UpdateCaret()
  {
   if(_byteProvider == null || _keyInterpreter == null )
    return;
   System.Diagnostics.Debug.WriteLine("UpdateCaret()", "HexBox");
   long byteIndex =_bytePos - _startByte;
   PointF p = _keyInterpreter.GetCaretPointF(byteIndex);
   p.X += _byteCharacterPos*_charSize.Width;
   NativeMethods.SetCaretPos((int)p.X, (int)p.Y);
  }
  void DestroyCaret()
  {
   if(!_caretVisible)
    return;
   System.Diagnostics.Debug.WriteLine("DestroyCaret()", "HexBox");
   NativeMethods.DestroyCaret();
   _caretVisible = false;
  }
  void SetCaretPosition(Point p)
  {
   System.Diagnostics.Debug.WriteLine("SetCaretPosition()", "HexBox");
   if(_byteProvider == null || _keyInterpreter == null)
    return;
   long pos = _bytePos;
   int cp = _byteCharacterPos;
   if(_recHex.Contains(p))
   {
    BytePositionInfo bpi = GetHexBytePositionInfo(p);
    pos = bpi.Index;
    cp = bpi.CharacterPosition;
    SetPosition(pos, cp);
    ActivateKeyInterpreter();
    UpdateCaret();
    Invalidate();
   }
   else if(_recStringView.Contains(p))
   {
    BytePositionInfo bpi = GetStringBytePositionInfo(p);
    pos = bpi.Index;
    cp = bpi.CharacterPosition;
    SetPosition(pos, cp);
    ActivateStringKeyInterpreter();
    UpdateCaret();
    Invalidate();
   }
  }
  BytePositionInfo GetHexBytePositionInfo(Point p)
  {
   System.Diagnostics.Debug.WriteLine("GetHexBytePositionInfo()", "HexBox");
   long bytePos;
   int byteCharaterPos;
   float x = ((float)(p.X - _recHex.X) / _charSize.Width);
   float y = ((float)(p.Y - _recHex.Y) / _charSize.Height);
   int iX = (int)x;
   int iY = (int)y;
   int hPos = (iX / 3 + 1);
   bytePos = Math.Min(_byteProvider.Length,
    _startByte + (_iHexMaxHBytes * (iY+1) - _iHexMaxHBytes) + hPos - 1);
   byteCharaterPos = (iX % 3);
   if(byteCharaterPos > 1)
    byteCharaterPos = 1;
   if(bytePos == _byteProvider.Length)
    byteCharaterPos = 0;
   if(bytePos < 0)
    return new BytePositionInfo(0, 0);
   return new BytePositionInfo(bytePos, byteCharaterPos);
  }
  BytePositionInfo GetStringBytePositionInfo(Point p)
  {
   System.Diagnostics.Debug.WriteLine("GetStringBytePositionInfo()", "HexBox");
   long bytePos;
   int byteCharacterPos;
   float x = ((float)(p.X - _recStringView.X) / _charSize.Width);
   float y = ((float)(p.Y - _recStringView.Y) / _charSize.Height);
   int iX = (int)x;
   int iY = (int)y;
   int hPos = iX+1;
   bytePos = Math.Min(_byteProvider.Length,
    _startByte + (_iHexMaxHBytes * (iY+1) - _iHexMaxHBytes) + hPos - 1);
   byteCharacterPos = 0;
   if(bytePos < 0)
    return new BytePositionInfo(0, 0);
   return new BytePositionInfo(bytePos, byteCharacterPos);
  }
  [SecurityPermission(SecurityAction.LinkDemand, UnmanagedCode=true), SecurityPermission(SecurityAction.InheritanceDemand, UnmanagedCode=true)]
  public override bool PreProcessMessage(ref Message m)
  {
   switch(m.Msg)
   {
    case NativeMethods.WM_KEYDOWN:
     return _keyInterpreter.PreProcessWmKeyDown(ref m);
    case NativeMethods.WM_CHAR:
     return _keyInterpreter.PreProcessWmChar(ref m);
    case NativeMethods.WM_KEYUP:
     return _keyInterpreter.PreProcessWmKeyUp(ref m);
    default:
     return base.PreProcessMessage (ref m);
   }
  }
  bool BasePreProcessMessage(ref Message m)
  {
   return base.PreProcessMessage(ref m);
  }
  public long Find(byte[] bytes, long startIndex)
  {
   int match = 0;
   int bytesLength = bytes.Length;
   _abortFind = false;
   for(long pos = startIndex; pos < _byteProvider.Length; pos++)
   {
    if(_abortFind)
     return -2;
    if(pos % 1000 == 0)
     Application.DoEvents();
    if(_byteProvider.ReadByte(pos) != bytes[match])
    {
     pos -= match;
     match = 0;
     _findingPos = pos;
     continue;
    }
    match++;
    if(match == bytesLength)
    {
     long bytePos = pos-bytesLength+1;
     Select(bytePos, bytesLength);
     ScrollByteIntoView(_bytePos+_selectionLength);
     ScrollByteIntoView(_bytePos);
     return bytePos;
    }
   }
   return -1;
  }
  public void AbortFind()
  {
   _abortFind = true;
  }
  [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
  public long CurrentFindingPosition
  {
   get
   {
    return _findingPos;
   }
  }
  public void Copy()
  {
   if(!CanCopy()) return;
   byte[] buffer = new byte[_selectionLength];
   int id = -1;
   for(long i = _bytePos; i < _bytePos+_selectionLength; i++)
   {
    id++;
    buffer[id] = _byteProvider.ReadByte(i);
   }
   DataObject da = new DataObject();
   string sBuffer = System.Text.Encoding.ASCII.GetString(buffer, 0, buffer.Length);
   da.SetData(typeof(string), sBuffer);
   System.IO.MemoryStream ms = new System.IO.MemoryStream(buffer, 0, buffer.Length, false, true);
   da.SetData("BinaryData", ms);
   Clipboard.SetDataObject(da, true);
   UpdateCaret();
   ScrollByteIntoView();
   Invalidate();
  }
  public bool CanCopy()
  {
   if(_selectionLength < 1 || _byteProvider == null)
    return false;
   return true;
  }
  public void Cut()
  {
   if(!CanCut()) return;
   Copy();
   _byteProvider.DeleteBytes(_bytePos, _selectionLength);
   _byteCharacterPos = 0;
   UpdateCaret();
   ScrollByteIntoView();
   ReleaseSelection();
   Invalidate();
   Refresh();
  }
  public bool CanCut()
  {
   if (ReadOnly || !this.Enabled)
    return false;
   if(_byteProvider == null)
    return false;
   if(_selectionLength < 1 || !_byteProvider.SupportsDeleteBytes())
    return false;
   return true;
  }
  public void Paste()
  {
   if(!CanPaste()) return;
   if(_selectionLength > 0)
    _byteProvider.DeleteBytes(_bytePos, _selectionLength);
   byte[] buffer = null;
   IDataObject da = Clipboard.GetDataObject();
   if(da.GetDataPresent("BinaryData"))
   {
    System.IO.MemoryStream ms = (System.IO.MemoryStream)da.GetData("BinaryData");
    buffer = new byte[ms.Length];
    ms.Read(buffer, 0, buffer.Length);
   }
   else if(da.GetDataPresent(typeof(string)))
   {
    string sBuffer = (string)da.GetData(typeof(string));
    buffer = System.Text.Encoding.ASCII.GetBytes(sBuffer);
   }
   else
   {
    return;
   }
   _byteProvider.InsertBytes(_bytePos, buffer);
   SetPosition(_bytePos + buffer.Length, 0);
   ReleaseSelection();
   ScrollByteIntoView();
   UpdateCaret();
   Invalidate();
  }
  public bool CanPaste()
  {
   if (ReadOnly || !this.Enabled) return false;
   if(_byteProvider == null || !_byteProvider.SupportsInsertBytes())
    return false;
   if(!_byteProvider.SupportsDeleteBytes() && _selectionLength > 0)
    return false;
   IDataObject da = Clipboard.GetDataObject();
   if(da.GetDataPresent("BinaryData"))
    return true;
   else if(da.GetDataPresent(typeof(string)))
    return true;
   else
    return false;
  }
  protected override void OnPaintBackground(PaintEventArgs e)
  {
   switch(_borderStyle)
   {
    case BorderStyle.Fixed3D:
    {
     if(TextBoxRenderer.IsSupported)
     {
                        VisualStyleElement state = VisualStyleElement.TextBox.TextEdit.Normal;
                        Color backColor = this.BackColor;
                        if (this.Enabled)
                        {
                            if (this.ReadOnly)
                                state = VisualStyleElement.TextBox.TextEdit.ReadOnly;
                            else if (this.Focused)
                                state = VisualStyleElement.TextBox.TextEdit.Focused;
                        }
                        else
                        {
                            state = VisualStyleElement.TextBox.TextEdit.Disabled;
                            backColor = this.BackColorDisabled;
                        }
                        VisualStyleRenderer vsr = new VisualStyleRenderer(state);
                        vsr.DrawBackground(e.Graphics, this.ClientRectangle);
                        Rectangle rectContent = vsr.GetBackgroundContentRectangle(e.Graphics, this.ClientRectangle);
                        e.Graphics.FillRectangle(new SolidBrush(backColor), rectContent);
     }
     else
     {
                        e.Graphics.FillRectangle(new SolidBrush(BackColor), ClientRectangle);
      ControlPaint.DrawBorder3D(e.Graphics, ClientRectangle, Border3DStyle.Sunken);
     }
     break;
    }
    case BorderStyle.FixedSingle:
    {
                    e.Graphics.FillRectangle(new SolidBrush(BackColor), ClientRectangle);
     ControlPaint.DrawBorder(e.Graphics, ClientRectangle, Color.Black, ButtonBorderStyle.Solid);
     break;
    }
   }
  }
  protected override void OnPaint(PaintEventArgs e)
  {
            base.OnPaint(e);
   if(_byteProvider == null)
    return;
   Region r = new Region(ClientRectangle);
   r.Exclude(_recContent);
   e.Graphics.ExcludeClip(r);
   UpdateVisibilityBytes();
   if(_lineInfoVisible)
    PaintLineInfo(e.Graphics, _startByte, _endByte);
   if(!_stringViewVisible)
   {
    PaintHex(e.Graphics, _startByte, _endByte);
   }
   else
   {
    PaintHexAndStringView(e.Graphics, _startByte, _endByte);
    if(_shadowSelectionVisible)
     PaintCurrentBytesSign(e.Graphics);
   }
  }
  void PaintLineInfo(Graphics g, long startByte, long endByte)
  {
   endByte = Math.Min(_byteProvider.Length-1, endByte);
   Color lineInfoColor = (this.LineInfoForeColor != Color.Empty) ? this.LineInfoForeColor : this.ForeColor;
   Brush brush = new SolidBrush(lineInfoColor);
   int maxLine = GetGridBytePoint(endByte-startByte).Y+1;
   for(int i = 0; i < maxLine; i++)
   {
    long firstLineByte = startByte + (_iHexMaxHBytes)*i;
    PointF bytePointF = GetBytePointF(new Point(0, 0+i));
    string info = firstLineByte.ToString(_hexStringFormat, System.Threading.Thread.CurrentThread.CurrentCulture);
    int nulls = 8-info.Length;
    string formattedInfo;
    if(nulls > -1)
    {
     formattedInfo = new string('0', 8-info.Length) + info;
    }
    else
    {
     formattedInfo = new string('~', 8);
    }
    g.DrawString(formattedInfo, Font, brush, new PointF(_recLineInfo.X, bytePointF.Y), _stringFormat);
   }
  }
  void PaintHex(Graphics g, long startByte, long endByte)
  {
   Brush brush = new SolidBrush(GetDefaultForeColor());
   Brush selBrush = new SolidBrush(_selectionForeColor);
   Brush selBrushBack = new SolidBrush(_selectionBackColor);
   int counter = -1;
   long intern_endByte = Math.Min(_byteProvider.Length-1, endByte+_iHexMaxHBytes);
   bool isKeyInterpreterActive = _keyInterpreter == null || _keyInterpreter.GetType() == typeof(KeyInterpreter);
   for(long i = startByte; i < intern_endByte+1; i++)
   {
    counter++;
    Point gridPoint = GetGridBytePoint(counter);
    byte b = _byteProvider.ReadByte(i);
    bool isSelectedByte = i >= _bytePos && i <= (_bytePos + _selectionLength-1) && _selectionLength != 0;
    if(isSelectedByte && isKeyInterpreterActive)
    {
     PaintHexStringSelected(g, b, selBrush, selBrushBack, gridPoint);
    }
    else
    {
     PaintHexString(g, b, brush, gridPoint);
    }
   }
  }
  void PaintHexString(Graphics g, byte b, Brush brush, Point gridPoint)
  {
   PointF bytePointF = GetBytePointF(gridPoint);
   string sB = b.ToString(_hexStringFormat, System.Threading.Thread.CurrentThread.CurrentCulture);
   if(sB.Length == 1)
    sB = "0" + sB;
   g.DrawString(sB.Substring(0,1), Font, brush, bytePointF, _stringFormat);
   bytePointF.X += _charSize.Width;
   g.DrawString(sB.Substring(1,1), Font, brush, bytePointF, _stringFormat);
  }
  void PaintHexStringSelected(Graphics g, byte b, Brush brush, Brush brushBack, Point gridPoint)
  {
   string sB = b.ToString(_hexStringFormat, System.Threading.Thread.CurrentThread.CurrentCulture);
   if(sB.Length == 1)
    sB = "0" + sB;
   PointF bytePointF = GetBytePointF(gridPoint);
   bool isLastLineChar = (gridPoint.X+1 == _iHexMaxHBytes);
   float bcWidth = (isLastLineChar) ? _charSize.Width*2 : _charSize.Width*3;
   g.FillRectangle(brushBack, bytePointF.X, bytePointF.Y, bcWidth, _charSize.Height);
   g.DrawString(sB.Substring(0,1), Font, brush, bytePointF, _stringFormat);
   bytePointF.X += _charSize.Width;
   g.DrawString(sB.Substring(1,1), Font, brush, bytePointF, _stringFormat);
  }
  void PaintHexAndStringView(Graphics g, long startByte, long endByte)
  {
   Brush brush = new SolidBrush(GetDefaultForeColor());
   Brush selBrush = new SolidBrush(_selectionForeColor);
   Brush selBrushBack = new SolidBrush(_selectionBackColor);
   int counter = -1;
   long intern_endByte = Math.Min(_byteProvider.Length-1, endByte+_iHexMaxHBytes);
   bool isKeyInterpreterActive = _keyInterpreter == null || _keyInterpreter.GetType() == typeof(KeyInterpreter);
   bool isStringKeyInterpreterActive = _keyInterpreter != null && _keyInterpreter.GetType() == typeof(StringKeyInterpreter);
   for(long i = startByte; i < intern_endByte+1; i++)
   {
    counter++;
    Point gridPoint = GetGridBytePoint(counter);
    PointF byteStringPointF = GetByteStringPointF(gridPoint);
    byte b = _byteProvider.ReadByte(i);
    bool isSelectedByte = i >= _bytePos && i <= (_bytePos + _selectionLength-1) && _selectionLength != 0;
    if(isSelectedByte && isKeyInterpreterActive)
    {
     PaintHexStringSelected(g, b, selBrush, selBrushBack, gridPoint);
    }
    else
    {
     PaintHexString(g, b, brush, gridPoint);
    }
    string s;
    if(b > 0x1F && !(b > 0x7E && b < 0xA0) )
    {
     s = ((char)b).ToString();
    }
    else
    {
     s = ".";
    }
    if(isSelectedByte && isStringKeyInterpreterActive)
    {
     g.FillRectangle(selBrushBack, byteStringPointF.X, byteStringPointF.Y, _charSize.Width, _charSize.Height);
     g.DrawString(s, Font, selBrush, byteStringPointF, _stringFormat);
    }
    else
    {
     g.DrawString(s, Font, brush, byteStringPointF, _stringFormat);
    }
   }
  }
  void PaintCurrentBytesSign(Graphics g)
  {
   if(_keyInterpreter != null && Focused && _bytePos != -1 && Enabled)
   {
    if(_keyInterpreter.GetType() == typeof(KeyInterpreter))
    {
     if(_selectionLength == 0)
     {
      Point gp = GetGridBytePoint(_bytePos - _startByte);
      PointF pf = GetByteStringPointF(gp);
      Size s = new Size((int)_charSize.Width, (int)_charSize.Height);
      Rectangle r = new Rectangle((int)pf.X, (int)pf.Y, s.Width, s.Height);
      if(r.IntersectsWith(_recStringView))
      {
       r.Intersect(_recStringView);
       PaintCurrentByteSign(g, r);
      }
     }
     else
     {
      int lineWidth = (int)(_recStringView.Width-_charSize.Width);
      Point startSelGridPoint = GetGridBytePoint(_bytePos-_startByte);
      PointF startSelPointF = GetByteStringPointF(startSelGridPoint);
      Point endSelGridPoint = GetGridBytePoint(_bytePos-_startByte+_selectionLength-1);
      PointF endSelPointF = GetByteStringPointF(endSelGridPoint);
      int multiLine = endSelGridPoint.Y - startSelGridPoint.Y;
      if(multiLine == 0)
      {
       Rectangle singleLine = new Rectangle(
        (int)startSelPointF.X,
        (int)startSelPointF.Y,
        (int)(endSelPointF.X-startSelPointF.X+_charSize.Width),
        (int)_charSize.Height);
       if(singleLine.IntersectsWith(_recStringView))
       {
        singleLine.Intersect(_recStringView);
        PaintCurrentByteSign(g, singleLine);
       }
      }
      else
      {
       Rectangle firstLine = new Rectangle(
        (int)startSelPointF.X,
        (int)startSelPointF.Y,
        (int)(_recStringView.X+lineWidth-startSelPointF.X+_charSize.Width),
        (int)_charSize.Height);
       if(firstLine.IntersectsWith(_recStringView))
       {
        firstLine.Intersect(_recStringView);
        PaintCurrentByteSign(g, firstLine);
       }
       if(multiLine > 1)
       {
        Rectangle betweenLines = new Rectangle(
         _recStringView.X,
         (int)(startSelPointF.Y+_charSize.Height),
         (int)(_recStringView.Width),
         (int)(_charSize.Height*(multiLine-1)));
        if(betweenLines.IntersectsWith(_recStringView))
        {
         betweenLines.Intersect(_recStringView);
         PaintCurrentByteSign(g, betweenLines);
        }
       }
       Rectangle lastLine = new Rectangle(
        _recStringView.X,
        (int)endSelPointF.Y,
        (int)(endSelPointF.X-_recStringView.X+_charSize.Width),
        (int)_charSize.Height);
       if(lastLine.IntersectsWith(_recStringView))
       {
        lastLine.Intersect(_recStringView);
        PaintCurrentByteSign(g, lastLine);
       }
      }
     }
    }
    else
    {
     if(_selectionLength == 0)
     {
      Point gp = GetGridBytePoint(_bytePos - _startByte);
      PointF pf = GetBytePointF(gp);
      Size s = new Size((int)_charSize.Width * 2, (int)_charSize.Height);
      Rectangle r = new Rectangle((int)pf.X, (int)pf.Y, s.Width, s.Height);
      PaintCurrentByteSign(g, r);
     }
     else
     {
      int lineWidth = (int)(_recHex.Width-_charSize.Width*5);
      Point startSelGridPoint = GetGridBytePoint(_bytePos-_startByte);
      PointF startSelPointF = GetBytePointF(startSelGridPoint);
      Point endSelGridPoint = GetGridBytePoint(_bytePos-_startByte+_selectionLength-1);
      PointF endSelPointF = GetBytePointF(endSelGridPoint);
      int multiLine = endSelGridPoint.Y - startSelGridPoint.Y;
      if(multiLine == 0)
      {
       Rectangle singleLine = new Rectangle(
        (int)startSelPointF.X,
        (int)startSelPointF.Y,
        (int)(endSelPointF.X-startSelPointF.X+_charSize.Width*2),
        (int)_charSize.Height);
       if(singleLine.IntersectsWith(_recHex))
       {
        singleLine.Intersect(_recHex);
        PaintCurrentByteSign(g, singleLine);
       }
      }
      else
      {
       Rectangle firstLine = new Rectangle(
        (int)startSelPointF.X,
        (int)startSelPointF.Y,
        (int)(_recHex.X+lineWidth-startSelPointF.X+_charSize.Width*2),
        (int)_charSize.Height);
       if(firstLine.IntersectsWith(_recHex))
       {
        firstLine.Intersect(_recHex);
        PaintCurrentByteSign(g, firstLine);
       }
       if(multiLine > 1)
       {
        Rectangle betweenLines = new Rectangle(
         _recHex.X,
         (int)(startSelPointF.Y+_charSize.Height),
         (int)(lineWidth+_charSize.Width*2),
         (int)(_charSize.Height*(multiLine-1)));
        if(betweenLines.IntersectsWith(_recHex))
        {
         betweenLines.Intersect(_recHex);
         PaintCurrentByteSign(g, betweenLines);
        }
       }
       Rectangle lastLine = new Rectangle(
        _recHex.X,
        (int)endSelPointF.Y,
        (int)(endSelPointF.X-_recHex.X+_charSize.Width*2),
        (int)_charSize.Height);
       if(lastLine.IntersectsWith(_recHex))
       {
        lastLine.Intersect(_recHex);
        PaintCurrentByteSign(g, lastLine);
       }
      }
     }
    }
   }
  }
  void PaintCurrentByteSign(Graphics g, Rectangle rec)
  {
   if(rec.Top < 0 || rec.Left < 0 || rec.Width <= 0 || rec.Height <= 0)
    return;
   Bitmap myBitmap = new Bitmap(rec.Width, rec.Height);
   Graphics bitmapGraphics = Graphics.FromImage(myBitmap);
   SolidBrush greenBrush = new SolidBrush(_shadowSelectionColor);
   bitmapGraphics.FillRectangle(greenBrush, 0,
    0, rec.Width, rec.Height);
   g.CompositingQuality = System.Drawing.Drawing2D.CompositingQuality.GammaCorrected;
   g.DrawImage(myBitmap, rec.Left, rec.Top);
  }
  Color GetDefaultForeColor()
  {
   if(Enabled)
    return ForeColor;
   else
    return Color.Gray;
  }
  void UpdateVisibilityBytes()
  {
   if(_byteProvider == null || _byteProvider.Length == 0)
    return;
   _startByte = (_scrollVpos+1) * _iHexMaxHBytes - _iHexMaxHBytes;
   _endByte = (long)Math.Min(_byteProvider.Length - 1, _startByte + _iHexMaxBytes);
  }
  void UpdateRectanglePositioning()
  {
   SizeF charSize = this.CreateGraphics().MeasureString("A", Font, 100, _stringFormat);
   _charSize = new SizeF((float)Math.Ceiling(charSize.Width), (float)Math.Ceiling(charSize.Height));
   _recContent = ClientRectangle;
   _recContent.X += _recBorderLeft;
   _recContent.Y += _recBorderTop;
   _recContent.Width -= _recBorderRight+_recBorderLeft;
   _recContent.Height -= _recBorderBottom+_recBorderTop;
   if(_vScrollBarVisible)
   {
    _recContent.Width -= _vScrollBar.Width;
    _vScrollBar.Left = _recContent.X+_recContent.Width;
    _vScrollBar.Top = _recContent.Y;
    _vScrollBar.Height = _recContent.Height;
   }
   int marginLeft = 4;
   if(_lineInfoVisible)
   {
    _recLineInfo = new Rectangle(_recContent.X+marginLeft,
     _recContent.Y,
     (int)(_charSize.Width*10),
     _recContent.Height);
   }
   else
   {
    _recLineInfo = Rectangle.Empty;
    _recLineInfo.X = marginLeft;
   }
   _recHex = new Rectangle(_recLineInfo.X + _recLineInfo.Width,
    _recLineInfo.Y,
    _recContent.Width - _recLineInfo.Width,
    _recContent.Height);
   if(UseFixedBytesPerLine)
   {
    SetHorizontalByteCount(_bytesPerLine);
    _recHex.Width = (int)Math.Floor(((double)_iHexMaxHBytes)*_charSize.Width*3+(2*_charSize.Width));
   }
   else
   {
    int hmax = (int)Math.Floor((double)_recHex.Width/1*(double)_charSize.Width);
    if(hmax > 1)
     SetHorizontalByteCount((int)Math.Floor((double)hmax/3));
    else
     SetHorizontalByteCount(hmax);
   }
   if(_stringViewVisible)
   {
    _recStringView = new Rectangle(_recHex.X + _recHex.Width,
     _recHex.Y,
     (int)(_charSize.Width*_iHexMaxHBytes),
     _recHex.Height);
   }
   else
   {
    _recStringView = Rectangle.Empty;
   }
   int vmax = (int)Math.Floor((double)_recHex.Height/1*(double)_charSize.Height);
   SetVerticalByteCount(vmax);
   _iHexMaxBytes = _iHexMaxHBytes * _iHexMaxVBytes;
   UpdateScrollSize();
  }
  PointF GetBytePointF(long byteIndex)
  {
   Point gp = GetGridBytePoint(byteIndex);
   return GetBytePointF(gp);
  }
  PointF GetBytePointF(Point gp)
  {
   float x = (3 * _charSize.Width) * gp.X + _recHex.X;
   float y = (gp.Y+1)*_charSize.Height-_charSize.Height+_recHex.Y;
   return new PointF(x,y);
  }
  PointF GetByteStringPointF(Point gp)
  {
   float x = (_charSize.Width) * gp.X + _recStringView.X;
   float y = (gp.Y+1)*_charSize.Height-_charSize.Height+_recStringView.Y;
   return new PointF(x,y);
  }
  Point GetGridBytePoint(long byteIndex)
  {
   int row = (int)Math.Floor((double)byteIndex/1*(double)_iHexMaxHBytes);
   int column = (int)(byteIndex+_iHexMaxHBytes-_iHexMaxHBytes*(row+1));
   Point res = new Point(column, row);
   return res;
  }
  [DefaultValue(typeof(Color), "White")]
  public override Color BackColor
  {
   get
   {
    return base.BackColor;
   }
   set
   {
    base.BackColor = value;
   }
  }
  [Editor(typeof(HexFontEditor), typeof(System.Drawing.Design.UITypeEditor))]
  public override Font Font
  {
   get
   {
    return base.Font;
   }
   set
   {
    base.Font = value;
   }
  }
  [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden), EditorBrowsable(EditorBrowsableState.Never), Bindable(false)]
  public override string Text
  {
   get
   {
    return base.Text;
   }
   set
   {
    base.Text = value;
   }
  }
  [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden), EditorBrowsable(EditorBrowsableState.Never), Bindable(false)]
  public override RightToLeft RightToLeft
  {
   get
   {
    return base.RightToLeft;
   }
   set
   {
    base.RightToLeft = value;
   }
  }
        [Category("Appearance"), DefaultValue(typeof(Color), "WhiteSmoke")]
        public Color BackColorDisabled
        {
            get
            {
                return _backColorDisabled;
            }
            set
            {
                _backColorDisabled = value;
            }
        } Color _backColorDisabled = Color.FromName("WhiteSmoke");
  [DefaultValue(false), Category("Hex"), Description("Gets or sets if the count of bytes in one line is fix.")]
  public bool ReadOnly
  {
   get { return _readOnly; }
   set
   {
    if(_readOnly == value)
     return;
    _readOnly = value;
    OnReadOnlyChanged(EventArgs.Empty);
    Invalidate();
   }
  } bool _readOnly;
  [DefaultValue(16), Category("Hex"), Description("Gets or sets the maximum count of bytes in one line.")]
  public int BytesPerLine
  {
   get { return _bytesPerLine; }
   set
   {
    if(_bytesPerLine == value)
     return;
    _bytesPerLine = value;
    OnByteProviderChanged(EventArgs.Empty);
    UpdateRectanglePositioning();
    Invalidate();
   }
  } int _bytesPerLine = 16;
  [DefaultValue(false), Category("Hex"), Description("Gets or sets if the count of bytes in one line is fix.")]
  public bool UseFixedBytesPerLine
  {
   get { return _useFixedBytesPerLine; }
   set
   {
    if(_useFixedBytesPerLine == value)
     return;
    _useFixedBytesPerLine = value;
    OnUseFixedBytesPerLineChanged(EventArgs.Empty);
    UpdateRectanglePositioning();
    Invalidate();
   }
  } bool _useFixedBytesPerLine;
  [DefaultValue(false), Category("Hex"), Description("Gets or sets the visibility of a vertical scroll bar.")]
  public bool VScrollBarVisible
  {
   get { return this._vScrollBarVisible; }
   set
   {
    if(_vScrollBarVisible == value)
     return;
    _vScrollBarVisible = value;
    if(_vScrollBarVisible)
     Controls.Add(_vScrollBar);
    else
     Controls.Remove(_vScrollBar);
    UpdateRectanglePositioning();
    UpdateScrollSize();
    OnVScrollBarVisibleChanged(EventArgs.Empty);
   }
  } bool _vScrollBarVisible;
  [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
  public IByteProvider ByteProvider
  {
   get { return _byteProvider; }
   set
   {
    if(_byteProvider == value)
     return;
    if(value == null)
     ActivateEmptyKeyInterpreter();
    else
     ActivateKeyInterpreter();
    if(_byteProvider != null)
     _byteProvider.LengthChanged -= new EventHandler(_byteProvider_LengthChanged);
    _byteProvider = value;
    if(_byteProvider != null)
     _byteProvider.LengthChanged += new EventHandler(_byteProvider_LengthChanged);
    OnByteProviderChanged(EventArgs.Empty);
    if(value == null)
    {
     _bytePos = -1;
     _byteCharacterPos = 0;
     _selectionLength = 0;
     DestroyCaret();
    }
    else
    {
     SetPosition(0, 0);
     SetSelectionLength(0);
     if(_caretVisible && Focused)
      UpdateCaret();
     else
      CreateCaret();
    }
    CheckCurrentLineChanged();
    CheckCurrentPositionInLineChanged();
    _scrollVpos = 0;
    UpdateVisibilityBytes();
    UpdateRectanglePositioning();
    Invalidate();
   }
  } IByteProvider _byteProvider;
  [DefaultValue(false), Category("Hex"), Description("Gets or sets the visibility of a line info.")]
  public bool LineInfoVisible
  {
   get { return _lineInfoVisible; }
   set
   {
    if(_lineInfoVisible == value)
     return;
    _lineInfoVisible = value;
    OnLineInfoVisibleChanged(EventArgs.Empty);
    UpdateRectanglePositioning();
    Invalidate();
   }
  } bool _lineInfoVisible;
  [DefaultValue(typeof(BorderStyle), "Fixed3D"), Category("Hex"), Description("Gets or sets the hex boxs border style.")]
  public BorderStyle BorderStyle
  {
   get { return _borderStyle;}
   set
   {
    if(_borderStyle == value)
     return;
    _borderStyle = value;
    switch(_borderStyle)
    {
     case BorderStyle.None:
      _recBorderLeft = _recBorderTop = _recBorderRight = _recBorderBottom = 0;
      break;
     case BorderStyle.Fixed3D:
      _recBorderLeft = _recBorderRight = SystemInformation.Border3DSize.Width;
      _recBorderTop = _recBorderBottom = SystemInformation.Border3DSize.Height;
      break;
     case BorderStyle.FixedSingle:
      _recBorderLeft = _recBorderTop = _recBorderRight = _recBorderBottom = 1;
      break;
    }
    UpdateRectanglePositioning();
    OnBorderStyleChanged(EventArgs.Empty);
   }
  } BorderStyle _borderStyle = BorderStyle.Fixed3D;
  [DefaultValue(false), Category("Hex"), Description("Gets or sets the visibility of the string view.")]
  public bool StringViewVisible
  {
   get { return _stringViewVisible; }
   set
   {
    if(_stringViewVisible == value)
     return;
    _stringViewVisible = value;
    OnStringViewVisibleChanged(EventArgs.Empty);
    UpdateRectanglePositioning();
    Invalidate();
   }
  } bool _stringViewVisible;
  [DefaultValue(typeof(HexCasing), "Upper"), Category("Hex"), Description("Gets or sets whether the HexBox control displays the hex characters in upper or lower case.")]
  public HexCasing HexCasing
  {
   get
   {
    if(_hexStringFormat == "X")
     return HexCasing.Upper;
    else
     return HexCasing.Lower;
   }
   set
   {
    string format;
    if(value == HexCasing.Upper)
     format = "X";
    else
     format = "x";
    if(_hexStringFormat == format)
     return;
    _hexStringFormat = format;
    OnHexCasingChanged(EventArgs.Empty);
    Invalidate();
   }
  }
  [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
  public long SelectionStart
  {
   get { return _bytePos; }
   set
   {
    SetPosition(value, 0);
    ScrollByteIntoView();
    Invalidate();
   }
  }
  [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
  public long SelectionLength
  {
   get { return _selectionLength; }
   set
   {
    SetSelectionLength(value);
    ScrollByteIntoView();
    Invalidate();
   }
  } long _selectionLength;
  [DefaultValue(typeof(Color), "Empty"), Category("Hex"), Description("Gets or sets the line info color. When this property is null, then ForeColor property is used.")]
  public Color LineInfoForeColor
  {
   get { return _lineInfoForeColor; }
   set { _lineInfoForeColor = value; Invalidate(); }
  } Color _lineInfoForeColor = Color.Empty;
  [DefaultValue(typeof(Color), "Blue"), Category("Hex"), Description("Gets or sets the background color for the selected bytes.")]
  public Color SelectionBackColor
  {
   get { return _selectionBackColor; }
   set { _selectionBackColor = value; Invalidate(); }
  } Color _selectionBackColor = Color.Blue;
  [DefaultValue(typeof(Color), "White"), Category("Hex"), Description("Gets or sets the foreground color for the selected bytes.")]
  public Color SelectionForeColor
  {
   get { return _selectionForeColor; }
   set { _selectionForeColor = value; Invalidate(); }
  } Color _selectionForeColor = Color.White;
  [DefaultValue(true), Category("Hex"), Description("Gets or sets the visibility of a shadow selection.")]
  public bool ShadowSelectionVisible
  {
   get { return _shadowSelectionVisible; }
   set
   {
    if(_shadowSelectionVisible == value)
     return;
    _shadowSelectionVisible = value;
    Invalidate();
   }
  } bool _shadowSelectionVisible = true;
  [Category("Hex"), Description("Gets or sets the color of the shadow selection.")]
  public Color ShadowSelectionColor
  {
   get { return _shadowSelectionColor; }
   set { _shadowSelectionColor = value; Invalidate(); }
  } Color _shadowSelectionColor = Color.FromArgb(100, 60, 188, 255);
  [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
  public int HorizontalByteCount
  {
   get { return _iHexMaxHBytes; }
  }
  [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
  public int VerticalByteCount
  {
   get { return _iHexMaxVBytes; }
  }
  [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
  public long CurrentLine
  {
   get { return _currentLine; }
  } long _currentLine;
  [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
  public long CurrentPositionInLine
  {
   get { return _currentPositionInLine; }
  } int _currentPositionInLine;
  [Browsable(false), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
  public bool InsertActive
  {
   get { return _insertActive; }
  }
  void SetPosition(long bytePos)
  {
   SetPosition(bytePos, _byteCharacterPos);
  }
  void SetPosition(long bytePos, int byteCharacterPos)
  {
   if(_byteCharacterPos != byteCharacterPos)
   {
    _byteCharacterPos = byteCharacterPos;
   }
   if(bytePos != _bytePos)
   {
    _bytePos = bytePos;
    CheckCurrentLineChanged();
    CheckCurrentPositionInLineChanged();
    OnSelectionStartChanged(EventArgs.Empty);
   }
  }
  void SetSelectionLength(long selectionLength)
  {
   if(selectionLength != _selectionLength)
   {
    _selectionLength = selectionLength;
    OnSelectionLengthChanged(EventArgs.Empty);
   }
  }
  void SetHorizontalByteCount(int value)
  {
   if(_iHexMaxHBytes == value)
    return;
   _iHexMaxHBytes = value;
   OnHorizontalByteCountChanged(EventArgs.Empty);
  }
  void SetVerticalByteCount(int value)
  {
   if(_iHexMaxVBytes == value)
    return;
   _iHexMaxVBytes = value;
   OnVerticalByteCountChanged(EventArgs.Empty);
  }
  void CheckCurrentLineChanged()
  {
   long currentLine = (long)Math.Floor((double)_bytePos / (double)_iHexMaxHBytes) + 1;
   if(_byteProvider == null && _currentLine != 0)
   {
    _currentLine = 0;
    OnCurrentLineChanged(EventArgs.Empty);
   }
   else if(currentLine != _currentLine)
   {
    _currentLine = currentLine;
    OnCurrentLineChanged(EventArgs.Empty);
   }
  }
  void CheckCurrentPositionInLineChanged()
  {
   Point gb = GetGridBytePoint(_bytePos);
   int currentPositionInLine = gb.X + 1;
   if(_byteProvider == null && _currentPositionInLine != 0)
   {
    _currentPositionInLine = 0;
    OnCurrentPositionInLineChanged(EventArgs.Empty);
   }
   else if(currentPositionInLine != _currentPositionInLine)
   {
    _currentPositionInLine = currentPositionInLine;
    OnCurrentPositionInLineChanged(EventArgs.Empty);
   }
  }
  protected virtual void OnInsertActiveChanged(EventArgs e)
  {
   if(InsertActiveChanged != null)
    InsertActiveChanged(this, e);
  }
  protected virtual void OnReadOnlyChanged(EventArgs e)
  {
   if(ReadOnlyChanged != null)
    ReadOnlyChanged(this, e);
  }
  protected virtual void OnByteProviderChanged(EventArgs e)
  {
   if(ByteProviderChanged != null)
    ByteProviderChanged(this, e);
  }
  protected virtual void OnSelectionStartChanged(EventArgs e)
  {
   if(SelectionStartChanged != null)
    SelectionStartChanged(this, e);
  }
  protected virtual void OnSelectionLengthChanged(EventArgs e)
  {
   if(SelectionLengthChanged != null)
    SelectionLengthChanged(this, e);
  }
  protected virtual void OnLineInfoVisibleChanged(EventArgs e)
  {
   if(LineInfoVisibleChanged != null)
    LineInfoVisibleChanged(this, e);
  }
  protected virtual void OnStringViewVisibleChanged(EventArgs e)
  {
   if(StringViewVisibleChanged != null)
    StringViewVisibleChanged(this, e);
  }
  protected virtual void OnBorderStyleChanged(EventArgs e)
  {
   if(BorderStyleChanged != null)
    BorderStyleChanged(this, e);
  }
  protected virtual void OnUseFixedBytesPerLineChanged(EventArgs e)
  {
   if(UseFixedBytesPerLineChanged != null)
    UseFixedBytesPerLineChanged(this, e);
  }
  protected virtual void OnBytesPerLineChanged(EventArgs e)
  {
   if(BytesPerLineChanged != null)
    BytesPerLineChanged(this, e);
  }
  protected virtual void OnVScrollBarVisibleChanged(EventArgs e)
  {
   if(VScrollBarVisibleChanged != null)
    VScrollBarVisibleChanged(this, e);
  }
  protected virtual void OnHexCasingChanged(EventArgs e)
  {
   if(HexCasingChanged != null)
    HexCasingChanged(this, e);
  }
  protected virtual void OnHorizontalByteCountChanged(EventArgs e)
  {
   if(HorizontalByteCountChanged != null)
    HorizontalByteCountChanged(this, e);
  }
  protected virtual void OnVerticalByteCountChanged(EventArgs e)
  {
   if(VerticalByteCountChanged != null)
    VerticalByteCountChanged(this, e);
  }
  protected virtual void OnCurrentLineChanged(EventArgs e)
  {
   if(CurrentLineChanged != null)
    CurrentLineChanged(this, e);
  }
  protected virtual void OnCurrentPositionInLineChanged(EventArgs e)
  {
   if(CurrentPositionInLineChanged != null)
    CurrentPositionInLineChanged(this, e);
  }
  protected override void OnMouseDown(MouseEventArgs e)
  {
   System.Diagnostics.Debug.WriteLine("OnMouseDown()", "HexBox");
   if(!Focused)
    Focus();
   SetCaretPosition(new Point(e.X, e.Y));
   base.OnMouseDown (e);
  }
  protected override void OnMouseWheel(MouseEventArgs e)
  {
   int linesToScroll = -(e.Delta * SystemInformation.MouseWheelScrollLines / 120);
   this.PerformScrollLines(linesToScroll);
   base.OnMouseWheel (e);
  }
  protected override void OnResize(EventArgs e)
  {
   base.OnResize (e);
   UpdateRectanglePositioning();
  }
  protected override void OnGotFocus(EventArgs e)
  {
   System.Diagnostics.Debug.WriteLine("OnGotFocus()", "HexBox");
   base.OnGotFocus (e);
   CreateCaret();
  }
  protected override void OnLostFocus(EventArgs e)
  {
   System.Diagnostics.Debug.WriteLine("OnLostFocus()", "HexBox");
   base.OnLostFocus (e);
   DestroyCaret();
  }
  void _byteProvider_LengthChanged(object sender, EventArgs e)
  {
   UpdateScrollSize();
  }
 }
}
