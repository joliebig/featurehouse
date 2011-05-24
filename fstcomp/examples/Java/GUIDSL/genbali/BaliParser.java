
public class BaliParser implements BaliParserConstants {

    private static Model parseRoot = null ;

    public static Model getStartRoot () {
        return parseRoot ;
    }

    public static Model getStartRoot (BaliParser parser)
    throws ParseException {
        try {
            parseRoot = parser.Model () ;
            parser.requireEOF () ;
            return parseRoot ;
        } catch (TokenMgrError error) {
            ParseException e = new ParseException
            ("token error occurred") ;
            e.initCause (error) ;
            throw e ;
        }
    }

    // Wraps an optional node around an AstNode:
    //
    static AstOptNode opt (AstNode node) {
        return new AstOptNode () . setParms (node) ;
    }

    // Wraps an optional node around an AstToken:
    //
    static AstOptToken opt (AstToken token) {
        return new AstOptToken () . setParms (token) ;
    }

    // Forces an end-of-file check in the tokenStream:
    //
    public void requireEOF () throws ParseException {
        try {
            jj_consume_token (BaliParserConstants.EOF) ;
        } catch (TokenMgrError error) {
            ParseException e = new ParseException ("EOF error") ;
            e.initCause (error) ;
            throw e ;
        }
    }

    // Converts a JavaCC Token to a Bali AstToken:
    //
    static AstToken t2at (Token tok) {

        // Special case -- if token is optional:
        //
        if (tok == null)
            return (null) ;

        StringBuffer buffer = new StringBuffer () ;
        Token special = tok.specialToken;
        while (special != null) {
            buffer.insert (0, special.toString()) ;
            special = special.specialToken ;
        }
        String white = buffer.toString () ;

        return new AstToken().setParms (white, tok.image, tok.endLine) ;
    }

//-----------------------------------//
// JAVACODE blocks from grammar:
//-----------------------------------//

// No JAVACODE blocks in Bali grammar.

//-----------------------------------//
// Productions from Bali grammar:
//-----------------------------------//
  static final public Model Model() throws ParseException {
    Cons co0=null ;
    Prods pr0=null ;
    Vars va0=null ;
    pr0 = Prods();
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case 21:
      co0 = Cons();
      break;
    default:
      jj_la1[0] = jj_gen;
      ;
    }
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case 38:
      va0 = Vars();
      break;
    default:
      jj_la1[1] = jj_gen;
      ;
    }
     {if (true) return new MainModel().setParms (pr0, opt(co0), opt(va0)) ;}
    throw new Error("Missing return statement in function");
  }

  static final public AExpr AExpr() throws ParseException {
    AExpr ae0=null ;
    NExpr ne0=null ;
    Token to0=null ;
    // Merged productions from rule AExpr
        // (*) NExpr
        // (*) NExpr "and" AExpr :: BAnd
        // 
        ne0 = NExpr();
    if (jj_2_1(2)) {
      to0 = jj_consume_token(16);
      ae0 = AExpr();
         {if (true) return new BAnd().setParms (ne0, t2at(to0), ae0) ;}
    } else {
      ;
    }
     {if (true) return (AExpr) ne0 ;}
    throw new Error("Missing return statement in function");
  }

  static final public Avar Avar() throws ParseException {
    Opts op0=null ;
    Token to2=null, to1=null, to0=null ;
    to0 = jj_consume_token(IDENTIFIER);
    to1 = jj_consume_token(17);
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case IDENTIFIER:
      op0 = Opts();
      break;
    default:
      jj_la1[2] = jj_gen;
      ;
    }
    to2 = jj_consume_token(18);
     {if (true) return new Var().setParms (t2at(to0), t2at(to1), opt(op0), t2at(to2)) ;}
    throw new Error("Missing return statement in function");
  }

  static final public AvarList AvarList() throws ParseException {
    AvarList list = new AvarList () ;
    Avar av0=null ;
    label_1:
    while (true) {
      av0 = Avar();
         list.add (new AvarListElem().setParms (av0)) ;
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case IDENTIFIER:
        ;
        break;
      default:
        jj_la1[3] = jj_gen;
        break label_1;
      }
    }
     {if (true) return list ;}
    throw new Error("Missing return statement in function");
  }

  static final public BExpr BExpr() throws ParseException {
    Expr ex0=null ;
    Token to1=null, to0=null ;
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case IDENTIFIER:
      to0 = jj_consume_token(IDENTIFIER);
     {if (true) return new Bvar().setParms (t2at(to0)) ;}
      break;
    case 19:
      to0 = jj_consume_token(19);
      ex0 = Expr();
      to1 = jj_consume_token(20);
     {if (true) return new Paren().setParms (t2at(to0), ex0, t2at(to1)) ;}
      break;
    default:
      jj_la1[4] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    throw new Error("Missing return statement in function");
  }

  static final public Cons Cons() throws ParseException {
    ESList es0=null ;
    Token to0=null ;
    to0 = jj_consume_token(21);
    es0 = ESList();
     {if (true) return new ConsStmt().setParms (t2at(to0), es0) ;}
    throw new Error("Missing return statement in function");
  }

  static final public EExpr EExpr() throws ParseException {
    EExpr ee0=null ;
    IExpr ie0=null ;
    Token to0=null ;
    // Merged productions from rule EExpr
        // (*) IExpr
        // (*) IExpr "iff" EExpr :: BIff
        // 
        ie0 = IExpr();
    if (jj_2_2(2)) {
      to0 = jj_consume_token(22);
      ee0 = EExpr();
         {if (true) return new BIff().setParms (ie0, t2at(to0), ee0) ;}
    } else {
      ;
    }
     {if (true) return (EExpr) ie0 ;}
    throw new Error("Missing return statement in function");
  }

  static final public ESList ESList() throws ParseException {
    ESList list = new ESList () ;
    ExprStmt ex0=null ;
    label_2:
    while (true) {
      ex0 = ExprStmt();
         list.add (new ESListElem().setParms (ex0)) ;
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case 19:
      case 23:
      case 26:
      case 33:
      case IDENTIFIER:
        ;
        break;
      default:
        jj_la1[5] = jj_gen;
        break label_2;
      }
    }
     {if (true) return list ;}
    throw new Error("Missing return statement in function");
  }

  static final public Expr Expr() throws ParseException {
    EExpr ee0=null ;
    ExprList ex0=null ;
    Token to2=null, to1=null, to0=null ;
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case 23:
      to0 = jj_consume_token(23);
      to1 = jj_consume_token(19);
      ex0 = ExprList();
      to2 = jj_consume_token(20);
     {if (true) return new BChoose1().setParms (t2at(to0), t2at(to1), ex0, t2at(to2)) ;}
      break;
    case 19:
    case 33:
    case IDENTIFIER:
      ee0 = EExpr();
     {if (true) return (Expr) ee0 ;}
      break;
    default:
      jj_la1[6] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    throw new Error("Missing return statement in function");
  }

  static final public ExprList ExprList() throws ParseException {
    ExprList list = new ExprList () ;
    EExpr ee0=null ;
    Token to0=null ;
    ee0 = EExpr();
     list.add (new ExprListElem().setParms (ee0)) ;
    label_3:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case 24:
        ;
        break;
      default:
        jj_la1[7] = jj_gen;
        break label_3;
      }
      to0 = jj_consume_token(24);
      ee0 = EExpr();
         list.add (new ExprListElem().setParms (t2at(to0), ee0)) ;
    }
     {if (true) return list ;}
    throw new Error("Missing return statement in function");
  }

  static final public ExprStmt ExprStmt() throws ParseException {
    Expr ex0=null ;
    Token to3=null, to2=null, to1=null, to0=null ;
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case 19:
    case 23:
    case 33:
    case IDENTIFIER:
      ex0 = Expr();
      to0 = jj_consume_token(25);
     {if (true) return new EStmt().setParms (ex0, t2at(to0)) ;}
      break;
    case 26:
      to0 = jj_consume_token(26);
      to1 = jj_consume_token(IDENTIFIER);
      to2 = jj_consume_token(22);
      ex0 = Expr();
      to3 = jj_consume_token(25);
     {if (true) return new VarDef().setParms
    (t2at(to0), t2at(to1), t2at(to2), ex0, t2at(to3)) ;}
      break;
    default:
      jj_la1[8] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    throw new Error("Missing return statement in function");
  }

  static final public GProd GProd() throws ParseException {
    Pats pa0=null ;
    Token to2=null, to1=null, to0=null ;
    to0 = jj_consume_token(IDENTIFIER);
    to1 = jj_consume_token(27);
    pa0 = Pats();
    to2 = jj_consume_token(25);
     {if (true) return new GProduction().setParms
    (t2at(to0), t2at(to1), pa0, t2at(to2)) ;}
    throw new Error("Missing return statement in function");
  }

  static final public GTerm GTerm() throws ParseException {
    Token to2=null, to1=null, to0=null ;
    if (jj_2_3(2)) {
      to0 = jj_consume_token(IDENTIFIER);
      to1 = jj_consume_token(28);
     {if (true) return new PlusTerm().setParms (t2at(to0), t2at(to1)) ;}
    } else if (jj_2_4(2)) {
      to0 = jj_consume_token(IDENTIFIER);
      to1 = jj_consume_token(29);
     {if (true) return new StarTerm().setParms (t2at(to0), t2at(to1)) ;}
    } else {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case IDENTIFIER:
        to0 = jj_consume_token(IDENTIFIER);
     {if (true) return new TermName().setParms (t2at(to0)) ;}
        break;
      case 30:
        to0 = jj_consume_token(30);
        to1 = jj_consume_token(IDENTIFIER);
        to2 = jj_consume_token(31);
     {if (true) return new OptTerm().setParms (t2at(to0), t2at(to1), t2at(to2)) ;}
        break;
      default:
        jj_la1[9] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
    throw new Error("Missing return statement in function");
  }

  static final public IExpr IExpr() throws ParseException {
    IExpr ie0=null ;
    OExpr oe0=null ;
    Token to0=null ;
    // Merged productions from rule IExpr
        // (*) OExpr
        // (*) OExpr "implies" IExpr :: BImplies
        // 
        oe0 = OExpr();
    if (jj_2_5(2)) {
      to0 = jj_consume_token(32);
      ie0 = IExpr();
         {if (true) return new BImplies().setParms (oe0, t2at(to0), ie0) ;}
    } else {
      ;
    }
     {if (true) return (IExpr) oe0 ;}
    throw new Error("Missing return statement in function");
  }

  static final public NExpr NExpr() throws ParseException {
    BExpr be0=null ;
    NExpr ne0=null ;
    Token to0=null ;
    switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
    case 19:
    case IDENTIFIER:
      be0 = BExpr();
     {if (true) return (NExpr) be0 ;}
      break;
    case 33:
      to0 = jj_consume_token(33);
      ne0 = NExpr();
     {if (true) return new BNot().setParms (t2at(to0), ne0) ;}
      break;
    default:
      jj_la1[10] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    throw new Error("Missing return statement in function");
  }

  static final public OExpr OExpr() throws ParseException {
    AExpr ae0=null ;
    OExpr oe0=null ;
    Token to0=null ;
    // Merged productions from rule OExpr
        // (*) AExpr
        // (*) AExpr "or" OExpr :: BOr
        // 
        ae0 = AExpr();
    if (jj_2_6(2)) {
      to0 = jj_consume_token(34);
      oe0 = OExpr();
         {if (true) return new BOr().setParms (ae0, t2at(to0), oe0) ;}
    } else {
      ;
    }
     {if (true) return (OExpr) ae0 ;}
    throw new Error("Missing return statement in function");
  }

  static final public Opt Opt() throws ParseException {
    Token to2=null, to1=null, to0=null ;
    if (jj_2_7(2)) {
      to0 = jj_consume_token(IDENTIFIER);
      to1 = jj_consume_token(35);
      to2 = jj_consume_token(STRING_LITERAL);
     {if (true) return new Strlit().setParms (t2at(to0), t2at(to1), t2at(to2)) ;}
    } else {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case IDENTIFIER:
        to0 = jj_consume_token(IDENTIFIER);
     {if (true) return new Optid().setParms (t2at(to0)) ;}
        break;
      default:
        jj_la1[11] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
    throw new Error("Missing return statement in function");
  }

  static final public Opts Opts() throws ParseException {
    Opts list = new Opts () ;
    Opt op0=null ;
    label_4:
    while (true) {
      op0 = Opt();
         list.add (new OptsElem().setParms (op0)) ;
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case IDENTIFIER:
        ;
        break;
      default:
        jj_la1[12] = jj_gen;
        break label_4;
      }
    }
     {if (true) return list ;}
    throw new Error("Missing return statement in function");
  }

  static final public Pat Pat() throws ParseException {
    TermList te0=null ;
    Token to1=null, to0=null ;
    if (jj_2_8(2)) {
      te0 = TermList();
      to0 = jj_consume_token(36);
      to1 = jj_consume_token(IDENTIFIER);
     {if (true) return new GPattern().setParms (te0, t2at(to0), t2at(to1)) ;}
    } else {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case IDENTIFIER:
        to0 = jj_consume_token(IDENTIFIER);
     {if (true) return new SimplePattern().setParms (t2at(to0)) ;}
        break;
      default:
        jj_la1[13] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
    }
    throw new Error("Missing return statement in function");
  }

  static final public Pats Pats() throws ParseException {
    Pats list = new Pats () ;
    Pat pa0=null ;
    Token to0=null ;
    pa0 = Pat();
     list.add (new PatsElem().setParms (pa0)) ;
    label_5:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case 37:
        ;
        break;
      default:
        jj_la1[14] = jj_gen;
        break label_5;
      }
      to0 = jj_consume_token(37);
      pa0 = Pat();
         list.add (new PatsElem().setParms (t2at(to0), pa0)) ;
    }
     {if (true) return list ;}
    throw new Error("Missing return statement in function");
  }

  static final public Prods Prods() throws ParseException {
    Prods list = new Prods () ;
    GProd gp0=null ;
    label_6:
    while (true) {
      gp0 = GProd();
         list.add (new ProdsElem().setParms (gp0)) ;
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case IDENTIFIER:
        ;
        break;
      default:
        jj_la1[15] = jj_gen;
        break label_6;
      }
    }
     {if (true) return list ;}
    throw new Error("Missing return statement in function");
  }

  static final public TermList TermList() throws ParseException {
    TermList list = new TermList () ;
    GTerm gt0=null ;
    label_7:
    while (true) {
      gt0 = GTerm();
         list.add (new TermListElem().setParms (gt0)) ;
      switch ((jj_ntk==-1)?jj_ntk():jj_ntk) {
      case 30:
      case IDENTIFIER:
        ;
        break;
      default:
        jj_la1[16] = jj_gen;
        break label_7;
      }
    }
     {if (true) return list ;}
    throw new Error("Missing return statement in function");
  }

  static final public Vars Vars() throws ParseException {
    AvarList av0=null ;
    Token to0=null ;
    to0 = jj_consume_token(38);
    av0 = AvarList();
     {if (true) return new VarStmt().setParms (t2at(to0), av0) ;}
    throw new Error("Missing return statement in function");
  }

  static final private boolean jj_2_1(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_1(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(0, xla); }
  }

  static final private boolean jj_2_2(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_2(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(1, xla); }
  }

  static final private boolean jj_2_3(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_3(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(2, xla); }
  }

  static final private boolean jj_2_4(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_4(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(3, xla); }
  }

  static final private boolean jj_2_5(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_5(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(4, xla); }
  }

  static final private boolean jj_2_6(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_6(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(5, xla); }
  }

  static final private boolean jj_2_7(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_7(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(6, xla); }
  }

  static final private boolean jj_2_8(int xla) {
    jj_la = xla; jj_lastpos = jj_scanpos = token;
    try { return !jj_3_8(); }
    catch(LookaheadSuccess ls) { return true; }
    finally { jj_save(7, xla); }
  }

  static final private boolean jj_3R_22() {
    if (jj_scan_token(19)) return true;
    return false;
  }

  static final private boolean jj_3_5() {
    if (jj_scan_token(32)) return true;
    if (jj_3R_10()) return true;
    return false;
  }

  static final private boolean jj_3R_21() {
    if (jj_scan_token(IDENTIFIER)) return true;
    return false;
  }

  static final private boolean jj_3R_18() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_21()) {
    jj_scanpos = xsp;
    if (jj_3R_22()) return true;
    }
    return false;
  }

  static final private boolean jj_3R_10() {
    if (jj_3R_11()) return true;
    return false;
  }

  static final private boolean jj_3_7() {
    if (jj_scan_token(IDENTIFIER)) return true;
    if (jj_scan_token(35)) return true;
    return false;
  }

  static final private boolean jj_3_6() {
    if (jj_scan_token(34)) return true;
    if (jj_3R_11()) return true;
    return false;
  }

  static final private boolean jj_3_2() {
    if (jj_scan_token(22)) return true;
    if (jj_3R_9()) return true;
    return false;
  }

  static final private boolean jj_3R_20() {
    if (jj_scan_token(30)) return true;
    if (jj_scan_token(IDENTIFIER)) return true;
    return false;
  }

  static final private boolean jj_3R_11() {
    if (jj_3R_8()) return true;
    return false;
  }

  static final private boolean jj_3R_9() {
    if (jj_3R_10()) return true;
    return false;
  }

  static final private boolean jj_3R_19() {
    if (jj_scan_token(IDENTIFIER)) return true;
    return false;
  }

  static final private boolean jj_3_4() {
    if (jj_scan_token(IDENTIFIER)) return true;
    if (jj_scan_token(29)) return true;
    return false;
  }

  static final private boolean jj_3_8() {
    if (jj_3R_12()) return true;
    if (jj_scan_token(36)) return true;
    return false;
  }

  static final private boolean jj_3R_14() {
    if (jj_3R_17()) return true;
    return false;
  }

  static final private boolean jj_3_1() {
    if (jj_scan_token(16)) return true;
    if (jj_3R_8()) return true;
    return false;
  }

  static final private boolean jj_3R_17() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3_3()) {
    jj_scanpos = xsp;
    if (jj_3_4()) {
    jj_scanpos = xsp;
    if (jj_3R_19()) {
    jj_scanpos = xsp;
    if (jj_3R_20()) return true;
    }
    }
    }
    return false;
  }

  static final private boolean jj_3_3() {
    if (jj_scan_token(IDENTIFIER)) return true;
    if (jj_scan_token(28)) return true;
    return false;
  }

  static final private boolean jj_3R_16() {
    if (jj_scan_token(33)) return true;
    return false;
  }

  static final private boolean jj_3R_12() {
    Token xsp;
    if (jj_3R_14()) return true;
    while (true) {
      xsp = jj_scanpos;
      if (jj_3R_14()) { jj_scanpos = xsp; break; }
    }
    return false;
  }

  static final private boolean jj_3R_13() {
    Token xsp;
    xsp = jj_scanpos;
    if (jj_3R_15()) {
    jj_scanpos = xsp;
    if (jj_3R_16()) return true;
    }
    return false;
  }

  static final private boolean jj_3R_15() {
    if (jj_3R_18()) return true;
    return false;
  }

  static final private boolean jj_3R_8() {
    if (jj_3R_13()) return true;
    return false;
  }

  static private boolean jj_initialized_once = false;
  static public BaliParserTokenManager token_source;
  static SimpleCharStream jj_input_stream;
  static public Token token, jj_nt;
  static private int jj_ntk;
  static private Token jj_scanpos, jj_lastpos;
  static private int jj_la;
  static public boolean lookingAhead = false;
  static private boolean jj_semLA;
  static private int jj_gen;
  static final private int[] jj_la1 = new int[17];
  static private int[] jj_la1_0;
  static private int[] jj_la1_1;
  static {
      jj_la1_0();
      jj_la1_1();
   }
   private static void jj_la1_0() {
      jj_la1_0 = new int[] {0x200000,0x0,0x0,0x0,0x80000,0x4880000,0x880000,0x1000000,0x4880000,0x40000000,0x80000,0x0,0x0,0x0,0x0,0x0,0x40000000,};
   }
   private static void jj_la1_1() {
      jj_la1_1 = new int[] {0x0,0x40,0x80,0x80,0x80,0x82,0x82,0x0,0x82,0x80,0x82,0x80,0x80,0x80,0x20,0x80,0x80,};
   }
  static final private JJCalls[] jj_2_rtns = new JJCalls[8];
  static private boolean jj_rescan = false;
  static private int jj_gc = 0;

  public BaliParser(java.io.InputStream stream) {
     this(stream, null);
  }
  public BaliParser(java.io.InputStream stream, String encoding) {
    if (jj_initialized_once) {
      System.out.println("ERROR: Second call to constructor of static parser.  You must");
      System.out.println("       either use ReInit() or set the JavaCC option STATIC to false");
      System.out.println("       during parser generation.");
      throw new Error();
    }
    jj_initialized_once = true;
    try { jj_input_stream = new SimpleCharStream(stream, encoding, 1, 1); } catch(java.io.UnsupportedEncodingException e) { throw new RuntimeException(e); }
    token_source = new BaliParserTokenManager(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 17; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  static public void ReInit(java.io.InputStream stream) {
     ReInit(stream, null);
  }
  static public void ReInit(java.io.InputStream stream, String encoding) {
    try { jj_input_stream.ReInit(stream, encoding, 1, 1); } catch(java.io.UnsupportedEncodingException e) { throw new RuntimeException(e); }
    token_source.ReInit(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 17; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public BaliParser(java.io.Reader stream) {
    if (jj_initialized_once) {
      System.out.println("ERROR: Second call to constructor of static parser.  You must");
      System.out.println("       either use ReInit() or set the JavaCC option STATIC to false");
      System.out.println("       during parser generation.");
      throw new Error();
    }
    jj_initialized_once = true;
    jj_input_stream = new SimpleCharStream(stream, 1, 1);
    token_source = new BaliParserTokenManager(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 17; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  static public void ReInit(java.io.Reader stream) {
    jj_input_stream.ReInit(stream, 1, 1);
    token_source.ReInit(jj_input_stream);
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 17; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public BaliParser(BaliParserTokenManager tm) {
    if (jj_initialized_once) {
      System.out.println("ERROR: Second call to constructor of static parser.  You must");
      System.out.println("       either use ReInit() or set the JavaCC option STATIC to false");
      System.out.println("       during parser generation.");
      throw new Error();
    }
    jj_initialized_once = true;
    token_source = tm;
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 17; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  public void ReInit(BaliParserTokenManager tm) {
    token_source = tm;
    token = new Token();
    jj_ntk = -1;
    jj_gen = 0;
    for (int i = 0; i < 17; i++) jj_la1[i] = -1;
    for (int i = 0; i < jj_2_rtns.length; i++) jj_2_rtns[i] = new JJCalls();
  }

  static final private Token jj_consume_token(int kind) throws ParseException {
    Token oldToken;
    if ((oldToken = token).next != null) token = token.next;
    else token = token.next = token_source.getNextToken();
    jj_ntk = -1;
    if (token.kind == kind) {
      jj_gen++;
      if (++jj_gc > 100) {
        jj_gc = 0;
        for (int i = 0; i < jj_2_rtns.length; i++) {
          JJCalls c = jj_2_rtns[i];
          while (c != null) {
            if (c.gen < jj_gen) c.first = null;
            c = c.next;
          }
        }
      }
      return token;
    }
    token = oldToken;
    jj_kind = kind;
    throw generateParseException();
  }

  static final private LookaheadSuccess jj_ls = new LookaheadSuccess();
  static final private boolean jj_scan_token(int kind) {
    if (jj_scanpos == jj_lastpos) {
      jj_la--;
      if (jj_scanpos.next == null) {
        jj_lastpos = jj_scanpos = jj_scanpos.next = token_source.getNextToken();
      } else {
        jj_lastpos = jj_scanpos = jj_scanpos.next;
      }
    } else {
      jj_scanpos = jj_scanpos.next;
    }
    if (jj_rescan) {
      int i = 0; Token tok = token;
      while (tok != null && tok != jj_scanpos) { i++; tok = tok.next; }
      if (tok != null) jj_add_error_token(kind, i);
    }
    if (jj_scanpos.kind != kind) return true;
    if (jj_la == 0 && jj_scanpos == jj_lastpos) throw jj_ls;
    return false;
  }

  static final public Token getNextToken() {
    if (token.next != null) token = token.next;
    else token = token.next = token_source.getNextToken();
    jj_ntk = -1;
    jj_gen++;
    return token;
  }

  static final public Token getToken(int index) {
    Token t = lookingAhead ? jj_scanpos : token;
    for (int i = 0; i < index; i++) {
      if (t.next != null) t = t.next;
      else t = t.next = token_source.getNextToken();
    }
    return t;
  }

  static final private int jj_ntk() {
    if ((jj_nt=token.next) == null)
      return (jj_ntk = (token.next=token_source.getNextToken()).kind);
    else
      return (jj_ntk = jj_nt.kind);
  }

  static private java.util.Vector jj_expentries = new java.util.Vector();
  static private int[] jj_expentry;
  static private int jj_kind = -1;
  static private int[] jj_lasttokens = new int[100];
  static private int jj_endpos;

  static private void jj_add_error_token(int kind, int pos) {
    if (pos >= 100) return;
    if (pos == jj_endpos + 1) {
      jj_lasttokens[jj_endpos++] = kind;
    } else if (jj_endpos != 0) {
      jj_expentry = new int[jj_endpos];
      for (int i = 0; i < jj_endpos; i++) {
        jj_expentry[i] = jj_lasttokens[i];
      }
      boolean exists = false;
      for (java.util.Enumeration e = jj_expentries.elements(); e.hasMoreElements();) {
        int[] oldentry = (int[])(e.nextElement());
        if (oldentry.length == jj_expentry.length) {
          exists = true;
          for (int i = 0; i < jj_expentry.length; i++) {
            if (oldentry[i] != jj_expentry[i]) {
              exists = false;
              break;
            }
          }
          if (exists) break;
        }
      }
      if (!exists) jj_expentries.addElement(jj_expentry);
      if (pos != 0) jj_lasttokens[(jj_endpos = pos) - 1] = kind;
    }
  }

  static public ParseException generateParseException() {
    jj_expentries.removeAllElements();
    boolean[] la1tokens = new boolean[41];
    for (int i = 0; i < 41; i++) {
      la1tokens[i] = false;
    }
    if (jj_kind >= 0) {
      la1tokens[jj_kind] = true;
      jj_kind = -1;
    }
    for (int i = 0; i < 17; i++) {
      if (jj_la1[i] == jj_gen) {
        for (int j = 0; j < 32; j++) {
          if ((jj_la1_0[i] & (1<<j)) != 0) {
            la1tokens[j] = true;
          }
          if ((jj_la1_1[i] & (1<<j)) != 0) {
            la1tokens[32+j] = true;
          }
        }
      }
    }
    for (int i = 0; i < 41; i++) {
      if (la1tokens[i]) {
        jj_expentry = new int[1];
        jj_expentry[0] = i;
        jj_expentries.addElement(jj_expentry);
      }
    }
    jj_endpos = 0;
    jj_rescan_token();
    jj_add_error_token(0, 0);
    int[][] exptokseq = new int[jj_expentries.size()][];
    for (int i = 0; i < jj_expentries.size(); i++) {
      exptokseq[i] = (int[])jj_expentries.elementAt(i);
    }
    return new ParseException(token, exptokseq, tokenImage);
  }

  static final public void enable_tracing() {
  }

  static final public void disable_tracing() {
  }

  static final private void jj_rescan_token() {
    jj_rescan = true;
    for (int i = 0; i < 8; i++) {
    try {
      JJCalls p = jj_2_rtns[i];
      do {
        if (p.gen > jj_gen) {
          jj_la = p.arg; jj_lastpos = jj_scanpos = p.first;
          switch (i) {
            case 0: jj_3_1(); break;
            case 1: jj_3_2(); break;
            case 2: jj_3_3(); break;
            case 3: jj_3_4(); break;
            case 4: jj_3_5(); break;
            case 5: jj_3_6(); break;
            case 6: jj_3_7(); break;
            case 7: jj_3_8(); break;
          }
        }
        p = p.next;
      } while (p != null);
      } catch(LookaheadSuccess ls) { }
    }
    jj_rescan = false;
  }

  static final private void jj_save(int index, int xla) {
    JJCalls p = jj_2_rtns[index];
    while (p.gen > jj_gen) {
      if (p.next == null) { p = p.next = new JJCalls(); break; }
      p = p.next;
    }
    p.gen = jj_gen + xla - jj_la; p.first = token; p.arg = xla;
  }

}
