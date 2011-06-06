
package com.lowagie.text.pdf.codec;

public class TIFFFaxDecoder {
    
    private int bitPointer, bytePointer;
    private byte[] data;
    private int w, h;
    private int fillOrder;
    
    
    
    private int changingElemSize = 0;
    private int prevChangingElems[];
    private int currChangingElems[];
    
    
    private int lastChangingElement = 0;
    
    private int compression = 2;
    
    
    private int uncompressedMode = 0;
    private int fillBits = 0;
    private int oneD;
    
    static int table1[] = {
        0x00, 
        0x01, 
        0x03, 
        0x07, 
        0x0f, 
        0x1f, 
        0x3f, 
        0x7f, 
        0xff  
    };
    
    static int table2[] = {
        0x00, 
        0x80, 
        0xc0, 
        0xe0, 
        0xf0, 
        0xf8, 
        0xfc, 
        0xfe, 
        0xff  
    };
    
    
    static byte flipTable[] = {
        0,  -128,    64,   -64,    32,   -96,    96,   -32,
        16,  -112,    80,   -48,    48,   -80,   112,   -16,
        8,  -120,    72,   -56,    40,   -88,   104,   -24,
        24,  -104,    88,   -40,    56,   -72,   120,    -8,
        4,  -124,    68,   -60,    36,   -92,   100,   -28,
        20,  -108,    84,   -44,    52,   -76,   116,   -12,
        12,  -116,    76,   -52,    44,   -84,   108,   -20,
        28,  -100,    92,   -36,    60,   -68,   124,    -4,
        2,  -126,    66,   -62,    34,   -94,    98,   -30,
        18,  -110,    82,   -46,    50,   -78,   114,   -14,
        10,  -118,    74,   -54,    42,   -86,   106,   -22,
        26,  -102,    90,   -38,    58,   -70,   122,    -6,
        6,  -122,    70,   -58,    38,   -90,   102,   -26,
        22,  -106,    86,   -42,    54,   -74,   118,   -10,
        14,  -114,    78,   -50,    46,   -82,   110,   -18,
        30,   -98,    94,   -34,    62,   -66,   126,    -2,
        1,  -127,    65,   -63,    33,   -95,    97,   -31,
        17,  -111,    81,   -47,    49,   -79,   113,   -15,
        9,  -119,    73,   -55,    41,   -87,   105,   -23,
        25,  -103,    89,   -39,    57,   -71,   121,    -7,
        5,  -123,    69,   -59,    37,   -91,   101,   -27,
        21,  -107,    85,   -43,    53,   -75,   117,   -11,
        13,  -115,    77,   -51,    45,   -83,   109,   -19,
        29,   -99,    93,   -35,    61,   -67,   125,    -3,
        3,  -125,    67,   -61,    35,   -93,    99,   -29,
        19,  -109,    83,   -45,    51,   -77,   115,   -13,
        11,  -117,    75,   -53,    43,   -85,   107,   -21,
        27,  -101,    91,   -37,    59,   -69,   123,    -5,
        7,  -121,    71,   -57,    39,   -89,   103,   -25,
        23,  -105,    87,   -41,    55,   -73,   119,    -9,
        15,  -113,    79,   -49,    47,   -81,   111,   -17,
        31,   -97,    95,   -33,    63,   -65,   127,    -1,
    };
    
    
    static short white[] = {
        
        6430,   6400,   6400,   6400,   3225,   3225,   3225,   3225,
        
        944,    944,    944,    944,    976,    976,    976,    976,
        
        1456,   1456,   1456,   1456,   1488,   1488,   1488,   1488,
        
        718,    718,    718,    718,    718,    718,    718,    718,
        
        750,    750,    750,    750,    750,    750,    750,    750,
        
        1520,   1520,   1520,   1520,   1552,   1552,   1552,   1552,
        
        428,    428,    428,    428,    428,    428,    428,    428,
        
        428,    428,    428,    428,    428,    428,    428,    428,
        
        654,    654,    654,    654,    654,    654,    654,    654,
        
        1072,   1072,   1072,   1072,   1104,   1104,   1104,   1104,
        
        1136,   1136,   1136,   1136,   1168,   1168,   1168,   1168,
        
        1200,   1200,   1200,   1200,   1232,   1232,   1232,   1232,
        
        622,    622,    622,    622,    622,    622,    622,    622,
        
        1008,   1008,   1008,   1008,   1040,   1040,   1040,   1040,
        
        44,     44,     44,     44,     44,     44,     44,     44,
        
        44,     44,     44,     44,     44,     44,     44,     44,
        
        396,    396,    396,    396,    396,    396,    396,    396,
        
        396,    396,    396,    396,    396,    396,    396,    396,
        
        1712,   1712,   1712,   1712,   1744,   1744,   1744,   1744,
        
        846,    846,    846,    846,    846,    846,    846,    846,
        
        1264,   1264,   1264,   1264,   1296,   1296,   1296,   1296,
        
        1328,   1328,   1328,   1328,   1360,   1360,   1360,   1360,
        
        1392,   1392,   1392,   1392,   1424,   1424,   1424,   1424,
        
        686,    686,    686,    686,    686,    686,    686,    686,
        
        910,    910,    910,    910,    910,    910,    910,    910,
        
        1968,   1968,   1968,   1968,   2000,   2000,   2000,   2000,
        
        2032,   2032,   2032,   2032,     16,     16,     16,     16,
        
        10257,  10257,  10257,  10257,  12305,  12305,  12305,  12305,
        
        330,    330,    330,    330,    330,    330,    330,    330,
        
        330,    330,    330,    330,    330,    330,    330,    330,
        
        330,    330,    330,    330,    330,    330,    330,    330,
        
        330,    330,    330,    330,    330,    330,    330,    330,
        
        362,    362,    362,    362,    362,    362,    362,    362,
        
        362,    362,    362,    362,    362,    362,    362,    362,
        
        362,    362,    362,    362,    362,    362,    362,    362,
        
        362,    362,    362,    362,    362,    362,    362,    362,
        
        878,    878,    878,    878,    878,    878,    878,    878,
        
        1904,   1904,   1904,   1904,   1936,   1936,   1936,   1936,
        
        -18413, -18413, -16365, -16365, -14317, -14317, -10221, -10221,
        
        590,    590,    590,    590,    590,    590,    590,    590,
        
        782,    782,    782,    782,    782,    782,    782,    782,
        
        1584,   1584,   1584,   1584,   1616,   1616,   1616,   1616,
        
        1648,   1648,   1648,   1648,   1680,   1680,   1680,   1680,
        
        814,    814,    814,    814,    814,    814,    814,    814,
        
        1776,   1776,   1776,   1776,   1808,   1808,   1808,   1808,
        
        1840,   1840,   1840,   1840,   1872,   1872,   1872,   1872,
        
        6157,   6157,   6157,   6157,   6157,   6157,   6157,   6157,
        
        6157,   6157,   6157,   6157,   6157,   6157,   6157,   6157,
        
        -12275, -12275, -12275, -12275, -12275, -12275, -12275, -12275,
        
        -12275, -12275, -12275, -12275, -12275, -12275, -12275, -12275,
        
        14353,  14353,  14353,  14353,  16401,  16401,  16401,  16401,
        
        22547,  22547,  24595,  24595,  20497,  20497,  20497,  20497,
        
        18449,  18449,  18449,  18449,  26643,  26643,  28691,  28691,
        
        30739,  30739, -32749, -32749, -30701, -30701, -28653, -28653,
        
        -26605, -26605, -24557, -24557, -22509, -22509, -20461, -20461,
        
        8207,   8207,   8207,   8207,   8207,   8207,   8207,   8207,
        
        72,     72,     72,     72,     72,     72,     72,     72,
        
        72,     72,     72,     72,     72,     72,     72,     72,
        
        72,     72,     72,     72,     72,     72,     72,     72,
        
        72,     72,     72,     72,     72,     72,     72,     72,
        
        72,     72,     72,     72,     72,     72,     72,     72,
        
        72,     72,     72,     72,     72,     72,     72,     72,
        
        72,     72,     72,     72,     72,     72,     72,     72,
        
        72,     72,     72,     72,     72,     72,     72,     72,
        
        104,    104,    104,    104,    104,    104,    104,    104,
        
        104,    104,    104,    104,    104,    104,    104,    104,
        
        104,    104,    104,    104,    104,    104,    104,    104,
        
        104,    104,    104,    104,    104,    104,    104,    104,
        
        104,    104,    104,    104,    104,    104,    104,    104,
        
        104,    104,    104,    104,    104,    104,    104,    104,
        
        104,    104,    104,    104,    104,    104,    104,    104,
        
        104,    104,    104,    104,    104,    104,    104,    104,
        
        4107,   4107,   4107,   4107,   4107,   4107,   4107,   4107,
        
        4107,   4107,   4107,   4107,   4107,   4107,   4107,   4107,
        
        4107,   4107,   4107,   4107,   4107,   4107,   4107,   4107,
        
        4107,   4107,   4107,   4107,   4107,   4107,   4107,   4107,
        
        266,    266,    266,    266,    266,    266,    266,    266,
        
        266,    266,    266,    266,    266,    266,    266,    266,
        
        266,    266,    266,    266,    266,    266,    266,    266,
        
        266,    266,    266,    266,    266,    266,    266,    266,
        
        298,    298,    298,    298,    298,    298,    298,    298,
        
        298,    298,    298,    298,    298,    298,    298,    298,
        
        298,    298,    298,    298,    298,    298,    298,    298,
        
        298,    298,    298,    298,    298,    298,    298,    298,
        
        524,    524,    524,    524,    524,    524,    524,    524,
        
        524,    524,    524,    524,    524,    524,    524,    524,
        
        556,    556,    556,    556,    556,    556,    556,    556,
        
        556,    556,    556,    556,    556,    556,    556,    556,
        
        136,    136,    136,    136,    136,    136,    136,    136,
        
        136,    136,    136,    136,    136,    136,    136,    136,
        
        136,    136,    136,    136,    136,    136,    136,    136,
        
        136,    136,    136,    136,    136,    136,    136,    136,
        
        136,    136,    136,    136,    136,    136,    136,    136,
        
        136,    136,    136,    136,    136,    136,    136,    136,
        
        136,    136,    136,    136,    136,    136,    136,    136,
        
        136,    136,    136,    136,    136,    136,    136,    136,
        
        168,    168,    168,    168,    168,    168,    168,    168,
        
        168,    168,    168,    168,    168,    168,    168,    168,
        
        168,    168,    168,    168,    168,    168,    168,    168,
        
        168,    168,    168,    168,    168,    168,    168,    168,
        
        168,    168,    168,    168,    168,    168,    168,    168,
        
        168,    168,    168,    168,    168,    168,    168,    168,
        
        168,    168,    168,    168,    168,    168,    168,    168,
        
        168,    168,    168,    168,    168,    168,    168,    168,
        
        460,    460,    460,    460,    460,    460,    460,    460,
        
        460,    460,    460,    460,    460,    460,    460,    460,
        
        492,    492,    492,    492,    492,    492,    492,    492,
        
        492,    492,    492,    492,    492,    492,    492,    492,
        
        2059,   2059,   2059,   2059,   2059,   2059,   2059,   2059,
        
        2059,   2059,   2059,   2059,   2059,   2059,   2059,   2059,
        
        2059,   2059,   2059,   2059,   2059,   2059,   2059,   2059,
        
        2059,   2059,   2059,   2059,   2059,   2059,   2059,   2059,
        
        200,    200,    200,    200,    200,    200,    200,    200,
        
        200,    200,    200,    200,    200,    200,    200,    200,
        
        200,    200,    200,    200,    200,    200,    200,    200,
        
        200,    200,    200,    200,    200,    200,    200,    200,
        
        200,    200,    200,    200,    200,    200,    200,    200,
        
        200,    200,    200,    200,    200,    200,    200,    200,
        
        200,    200,    200,    200,    200,    200,    200,    200,
        
        200,    200,    200,    200,    200,    200,    200,    200,
        
        232,    232,    232,    232,    232,    232,    232,    232,
        
        232,    232,    232,    232,    232,    232,    232,    232,
        
        232,    232,    232,    232,    232,    232,    232,    232,
        
        232,    232,    232,    232,    232,    232,    232,    232,
        
        232,    232,    232,    232,    232,    232,    232,    232,
        
        232,    232,    232,    232,    232,    232,    232,    232,
        
        232,    232,    232,    232,    232,    232,    232,    232,
        
        232,    232,    232,    232,    232,    232,    232,    232,
    };
    
    
    static short additionalMakeup[] = {
        28679,  28679,  31752,  (short)32777,
        (short)33801,  (short)34825,  (short)35849,  (short)36873,
        (short)29703,  (short)29703,  (short)30727,  (short)30727,
        (short)37897,  (short)38921,  (short)39945,  (short)40969
    };
    
    
    static short initBlack[] = {
        
        3226,  6412,    200,    168,    38,     38,    134,    134,
        
        100,    100,    100,    100,    68,     68,     68,     68
    };
    
    
    static short twoBitBlack[] = {292, 260, 226, 226};   
    
    
    static short black[] = {
        
        62,     62,     30,     30,     0,      0,      0,      0,
        
        0,      0,      0,      0,      0,      0,      0,      0,
        
        0,      0,      0,      0,      0,      0,      0,      0,
        
        0,      0,      0,      0,      0,      0,      0,      0,
        
        3225,   3225,   3225,   3225,   3225,   3225,   3225,   3225,
        
        3225,   3225,   3225,   3225,   3225,   3225,   3225,   3225,
        
        3225,   3225,   3225,   3225,   3225,   3225,   3225,   3225,
        
        3225,   3225,   3225,   3225,   3225,   3225,   3225,   3225,
        
        588,    588,    588,    588,    588,    588,    588,    588,
        
        1680,   1680,  20499,  22547,  24595,  26643,   1776,   1776,
        
        1808,   1808, -24557, -22509, -20461, -18413,   1904,   1904,
        
        1936,   1936, -16365, -14317,    782,    782,    782,    782,
        
        814,    814,    814,    814, -12269, -10221,  10257,  10257,
        
        12305,  12305,  14353,  14353,  16403,  18451,   1712,   1712,
        
        1744,   1744,  28691,  30739, -32749, -30701, -28653, -26605,
        
        2061,   2061,   2061,   2061,   2061,   2061,   2061,   2061,
        
        424,    424,    424,    424,    424,    424,    424,    424,
        
        424,    424,    424,    424,    424,    424,    424,    424,
        
        424,    424,    424,    424,    424,    424,    424,    424,
        
        424,    424,    424,    424,    424,    424,    424,    424,
        
        750,    750,    750,    750,   1616,   1616,   1648,   1648,
        
        1424,   1424,   1456,   1456,   1488,   1488,   1520,   1520,
        
        1840,   1840,   1872,   1872,   1968,   1968,   8209,   8209,
        
        524,    524,    524,    524,    524,    524,    524,    524,
        
        556,    556,    556,    556,    556,    556,    556,    556,
        
        1552,   1552,   1584,   1584,   2000,   2000,   2032,   2032,
        
        976,    976,   1008,   1008,   1040,   1040,   1072,   1072,
        
        1296,   1296,   1328,   1328,    718,    718,    718,    718,
        
        456,    456,    456,    456,    456,    456,    456,    456,
        
        456,    456,    456,    456,    456,    456,    456,    456,
        
        456,    456,    456,    456,    456,    456,    456,    456,
        
        456,    456,    456,    456,    456,    456,    456,    456,
        
        326,    326,    326,    326,    326,    326,    326,    326,
        
        326,    326,    326,    326,    326,    326,    326,    326,
        
        326,    326,    326,    326,    326,    326,    326,    326,
        
        326,    326,    326,    326,    326,    326,    326,    326,
        
        326,    326,    326,    326,    326,    326,    326,    326,
        
        326,    326,    326,    326,    326,    326,    326,    326,
        
        326,    326,    326,    326,    326,    326,    326,    326,
        
        326,    326,    326,    326,    326,    326,    326,    326,
        
        358,    358,    358,    358,    358,    358,    358,    358,
        
        358,    358,    358,    358,    358,    358,    358,    358,
        
        358,    358,    358,    358,    358,    358,    358,    358,
        
        358,    358,    358,    358,    358,    358,    358,    358,
        
        358,    358,    358,    358,    358,    358,    358,    358,
        
        358,    358,    358,    358,    358,    358,    358,    358,
        
        358,    358,    358,    358,    358,    358,    358,    358,
        
        358,    358,    358,    358,    358,    358,    358,    358,
        
        490,    490,    490,    490,    490,    490,    490,    490,
        
        490,    490,    490,    490,    490,    490,    490,    490,
        
        4113,   4113,   6161,   6161,    848,    848,    880,    880,
        
        912,    912,    944,    944,    622,    622,    622,    622,
        
        654,    654,    654,    654,   1104,   1104,   1136,   1136,
        
        1168,   1168,   1200,   1200,   1232,   1232,   1264,   1264,
        
        686,    686,    686,    686,   1360,   1360,   1392,   1392,
        
        12,     12,     12,     12,     12,     12,     12,     12,
        
        390,    390,    390,    390,    390,    390,    390,    390,
        
        390,    390,    390,    390,    390,    390,    390,    390,
        
        390,    390,    390,    390,    390,    390,    390,    390,
        
        390,    390,    390,    390,    390,    390,    390,    390,
        
        390,    390,    390,    390,    390,    390,    390,    390,
        
        390,    390,    390,    390,    390,    390,    390,    390,
        
        390,    390,    390,    390,    390,    390,    390,    390,
        
        390,    390,    390,    390,    390,    390,    390,    390,
    };
    
    static byte twoDCodes[] = {
        
        80,     88,     23,     71,     30,     30,     62,     62,
        
        4,      4,      4,      4,      4,      4,      4,      4,
        
        11,     11,     11,     11,     11,     11,     11,     11,
        
        11,     11,     11,     11,     11,     11,     11,     11,
        
        35,     35,     35,     35,     35,     35,     35,     35,
        
        35,     35,     35,     35,     35,     35,     35,     35,
        
        51,     51,     51,     51,     51,     51,     51,     51,
        
        51,     51,     51,     51,     51,     51,     51,     51,
        
        41,     41,     41,     41,     41,     41,     41,     41,
        
        41,     41,     41,     41,     41,     41,     41,     41,
        
        41,     41,     41,     41,     41,     41,     41,     41,
        
        41,     41,     41,     41,     41,     41,     41,     41,
        
        41,     41,     41,     41,     41,     41,     41,     41,
        
        41,     41,     41,     41,     41,     41,     41,     41,
        
        41,     41,     41,     41,     41,     41,     41,     41,
        
        41,     41,     41,     41,     41,     41,     41,     41,
    };
    
    
    public TIFFFaxDecoder(int fillOrder, int w, int h) {
        this.fillOrder = fillOrder;
        this.w = w;
        this.h = h;
        
        this.bitPointer = 0;
        this.bytePointer = 0;
        this.prevChangingElems = new int[w];
        this.currChangingElems = new int[w];
    }
    
    
    
    public void decode1D(byte[] buffer, byte[] compData,
    int startX, int height) {
        this.data = compData;
        
        int lineOffset = 0;
        int scanlineStride = (w + 7)/8;
        
        bitPointer = 0;
        bytePointer = 0;
        
        for (int i = 0; i < height; i++) {
            decodeNextScanline(buffer, lineOffset, startX);
            lineOffset += scanlineStride;
        }
    }
    
    public void decodeNextScanline(byte[] buffer,
    int lineOffset, int bitOffset) {
        int bits = 0, code = 0, isT = 0;
        int current, entry, twoBits;
        boolean isWhite = true;
        
        
        changingElemSize = 0;
        
        
        while (bitOffset < w) {
            while (isWhite) {
                
                current = nextNBits(10);
                entry = white[current];
                
                
                isT = entry & 0x0001;
                bits = (entry >>> 1) & 0x0f;
                
                if (bits == 12) {          
                    
                    twoBits = nextLesserThan8Bits(2);
                    
                    current = ((current << 2) & 0x000c) | twoBits;
                    entry = additionalMakeup[current];
                    bits = (entry >>> 1) & 0x07;     
                    code  = (entry >>> 4) & 0x0fff;  
                    bitOffset += code; 
                    
                    updatePointer(4 - bits);
                } else if (bits == 0) {     
                    throw new RuntimeException("Invalid code encountered.");
                } else if (bits == 15) {    
                    throw new RuntimeException("EOL code word encountered in White run.");
                } else {
                    
                    code = (entry >>> 5) & 0x07ff;
                    bitOffset += code;
                    
                    updatePointer(10 - bits);
                    if (isT == 0) {
                        isWhite = false;
                        currChangingElems[changingElemSize++] = bitOffset;
                    }
                }
            }
            
            
            
            if (bitOffset == w) {
                if (compression == 2) {
                    advancePointer();
                }
                break;
            }
            
            while (!isWhite) {
                
                current = nextLesserThan8Bits(4);
                entry = initBlack[current];
                
                
                isT = entry & 0x0001;
                bits = (entry >>> 1) & 0x000f;
                code = (entry >>> 5) & 0x07ff;
                
                if (code == 100) {
                    current = nextNBits(9);
                    entry = black[current];
                    
                    
                    isT = entry & 0x0001;
                    bits = (entry >>> 1) & 0x000f;
                    code = (entry >>> 5) & 0x07ff;
                    
                    if (bits == 12) {
                        
                        updatePointer(5);
                        current = nextLesserThan8Bits(4);
                        entry = additionalMakeup[current];
                        bits = (entry >>> 1) & 0x07;     
                        code  = (entry >>> 4) & 0x0fff;  
                        
                        setToBlack(buffer, lineOffset, bitOffset, code);
                        bitOffset += code;
                        
                        updatePointer(4 - bits);
                    } else if (bits == 15) {
                        
                        throw new RuntimeException("EOL code word encountered in Black run.");
                    } else {
                        setToBlack(buffer, lineOffset, bitOffset, code);
                        bitOffset += code;
                        
                        updatePointer(9 - bits);
                        if (isT == 0) {
                            isWhite = true;
                            currChangingElems[changingElemSize++] = bitOffset;
                        }
                    }
                } else if (code == 200) {
                    
                    current = nextLesserThan8Bits(2);
                    entry = twoBitBlack[current];
                    code = (entry >>> 5) & 0x07ff;
                    bits = (entry >>> 1) & 0x0f;
                    
                    setToBlack(buffer, lineOffset, bitOffset, code);
                    bitOffset += code;
                    
                    updatePointer(2 - bits);
                    isWhite = true;
                    currChangingElems[changingElemSize++] = bitOffset;
                } else {
                    
                    setToBlack(buffer, lineOffset, bitOffset, code);
                    bitOffset += code;
                    
                    updatePointer(4 - bits);
                    isWhite = true;
                    currChangingElems[changingElemSize++] = bitOffset;
                }
            }
            
            
            if (bitOffset == w) {
                if (compression == 2) {
                    advancePointer();
                }
                break;
            }
        }
        
        currChangingElems[changingElemSize++] = bitOffset;
    }
    
    
    
    public void decode2D(byte[] buffer,
    byte compData[],
    int startX,
    int height,
    long tiffT4Options) {
        this.data = compData;
        compression = 3;
        
        bitPointer = 0;
        bytePointer = 0;
        
        int scanlineStride = (w + 7)/8;
        
        int a0, a1, b1, b2;
        int[] b = new int[2];
        int entry, code, bits;
        boolean isWhite;
        int currIndex = 0;
        int temp[];
        
        
        
        
        
        
        
        oneD = (int)(tiffT4Options & 0x01);
        uncompressedMode = (int)((tiffT4Options & 0x02) >> 1);
        fillBits = (int)((tiffT4Options & 0x04) >> 2);
        
        
        if (readEOL(true) != 1) {
            throw new RuntimeException("First scanline must be 1D encoded.");
        }
        
        int lineOffset = 0;
        int bitOffset;
        
        
        
        decodeNextScanline(buffer, lineOffset, startX);
        lineOffset += scanlineStride;
        
        for (int lines = 1; lines < height; lines++) {
            
            
            
            if (readEOL(false) == 0) {
                
                
                
                
                temp = prevChangingElems;
                prevChangingElems = currChangingElems;
                currChangingElems = temp;
                currIndex = 0;
                
                
                a0 = -1;
                isWhite = true;
                bitOffset = startX;
                
                lastChangingElement = 0;
                
                while (bitOffset < w) {
                    
                    getNextChangingElement(a0, isWhite, b);
                    
                    b1 = b[0];
                    b2 = b[1];
                    
                    
                    entry = nextLesserThan8Bits(7);
                    
                    
                    entry = twoDCodes[entry] & 0xff;
                    
                    
                    code = (entry & 0x78) >>> 3;
                    bits = entry & 0x07;
                    
                    if (code == 0) {
                        if (!isWhite) {
                            setToBlack(buffer, lineOffset, bitOffset,
                            b2 - bitOffset);
                        }
                        bitOffset = a0 = b2;
                        
                        
                        updatePointer(7 - bits);
                    } else if (code == 1) {
                        
                        updatePointer(7 - bits);
                        
                        
                        int number;
                        if (isWhite) {
                            number = decodeWhiteCodeWord();
                            bitOffset += number;
                            currChangingElems[currIndex++] = bitOffset;
                            
                            number = decodeBlackCodeWord();
                            setToBlack(buffer, lineOffset, bitOffset, number);
                            bitOffset += number;
                            currChangingElems[currIndex++] = bitOffset;
                        } else {
                            number = decodeBlackCodeWord();
                            setToBlack(buffer, lineOffset, bitOffset, number);
                            bitOffset += number;
                            currChangingElems[currIndex++] = bitOffset;
                            
                            number = decodeWhiteCodeWord();
                            bitOffset += number;
                            currChangingElems[currIndex++] = bitOffset;
                        }
                        
                        a0 = bitOffset;
                    } else if (code <= 8) {
                        
                        a1 = b1 + (code - 5);
                        
                        currChangingElems[currIndex++] = a1;
                        
                        
                        
                        if (!isWhite) {
                            setToBlack(buffer, lineOffset, bitOffset,
                            a1 - bitOffset);
                        }
                        bitOffset = a0 = a1;
                        isWhite = !isWhite;
                        
                        updatePointer(7 - bits);
                    } else {
                        throw new RuntimeException("Invalid code encountered while decoding 2D group 3 compressed data.");
                    }
                }
                
                
                
                currChangingElems[currIndex++] = bitOffset;
                changingElemSize = currIndex;
            } else {
                
                decodeNextScanline(buffer, lineOffset, startX);
            }
            
            lineOffset += scanlineStride;
        }
    }
    
    public void decodeT6(byte[] buffer,
    byte[] compData,
    int startX,
    int height,
    long tiffT6Options) {
        this.data = compData;
        compression = 4;
        
        bitPointer = 0;
        bytePointer = 0;
        
        int scanlineStride = (w + 7)/8;
        
        int a0, a1, b1, b2;
        int entry, code, bits;
        boolean isWhite;
        int currIndex;
        int temp[];
        
        
        int[] b = new int[2];
        
        
        
        
        uncompressedMode = (int)((tiffT6Options & 0x02) >> 1);
        
        
        int[] cce = currChangingElems;
        
        
        
        
        changingElemSize = 0;
        cce[changingElemSize++] = w;
        cce[changingElemSize++] = w;
        
        int lineOffset = 0;
        int bitOffset;
        
        for (int lines = 0; lines < height; lines++) {
            
            a0 = -1;
            isWhite = true;
            
            
            
            
            temp = prevChangingElems;
            prevChangingElems = currChangingElems;
            cce = currChangingElems = temp;
            currIndex = 0;
            
            
            bitOffset = startX;
            
            
            lastChangingElement = 0;
            
            
            while (bitOffset < w) {
                
                getNextChangingElement(a0, isWhite, b);
                b1 = b[0];
                b2 = b[1];
                
                
                entry = nextLesserThan8Bits(7);
                
                entry = twoDCodes[entry] & 0xff;
                
                
                code = (entry & 0x78) >>> 3;
                bits = entry & 0x07;
                
                if (code == 0) { 
                    
                    if (!isWhite) {
                        setToBlack(buffer, lineOffset, bitOffset,
                        b2 - bitOffset);
                    }
                    bitOffset = a0 = b2;
                    
                    
                    updatePointer(7 - bits);
                } else if (code == 1) { 
                    
                    updatePointer(7 - bits);
                    
                    
                    int number;
                    if (isWhite) {
                        
                        number = decodeWhiteCodeWord();
                        bitOffset += number;
                        cce[currIndex++] = bitOffset;
                        
                        number = decodeBlackCodeWord();
                        setToBlack(buffer, lineOffset, bitOffset, number);
                        bitOffset += number;
                        cce[currIndex++] = bitOffset;
                    } else {
                        
                        number = decodeBlackCodeWord();
                        setToBlack(buffer, lineOffset, bitOffset, number);
                        bitOffset += number;
                        cce[currIndex++] = bitOffset;
                        
                        number = decodeWhiteCodeWord();
                        bitOffset += number;
                        cce[currIndex++] = bitOffset;
                    }
                    
                    a0 = bitOffset;
                } else if (code <= 8) { 
                    a1 = b1 + (code - 5);
                    cce[currIndex++] = a1;
                    
                    
                    
                    if (!isWhite) {
                        setToBlack(buffer, lineOffset, bitOffset,
                        a1 - bitOffset);
                    }
                    bitOffset = a0 = a1;
                    isWhite = !isWhite;
                    
                    updatePointer(7 - bits);
                } else if (code == 11) {
                    if (nextLesserThan8Bits(3) != 7) {
                        throw new RuntimeException("Invalid code encountered while decoding 2D group 4 compressed data.");
                    }
                    
                    int zeros = 0;
                    boolean exit = false;
                    
                    while (!exit) {
                        while (nextLesserThan8Bits(1) != 1) {
                            zeros++;
                        }
                        
                        if (zeros > 5) {
                            
                            
                            
                            zeros = zeros - 6;
                            
                            if (!isWhite && (zeros > 0)) {
                                cce[currIndex++] = bitOffset;
                            }
                            
                            
                            bitOffset += zeros;
                            if (zeros > 0) {
                                
                                isWhite = true;
                            }
                            
                            
                            
                            if (nextLesserThan8Bits(1) == 0) {
                                if (!isWhite) {
                                    cce[currIndex++] = bitOffset;
                                }
                                isWhite = true;
                            } else {
                                if (isWhite) {
                                    cce[currIndex++] = bitOffset;
                                }
                                isWhite = false;
                            }
                            
                            exit = true;
                        }
                        
                        if (zeros == 5) {
                            if (!isWhite) {
                                cce[currIndex++] = bitOffset;
                            }
                            bitOffset += zeros;
                            
                            
                            isWhite = true;
                        } else {
                            bitOffset += zeros;
                            
                            cce[currIndex++] = bitOffset;
                            setToBlack(buffer, lineOffset, bitOffset, 1);
                            ++bitOffset;
                            
                            
                            isWhite = false;
                        }
                        
                    }
                } else {
                    throw new RuntimeException("Invalid code encountered while decoding 2D group 4 compressed data.");
                }
            }
            
            
            
            
            if(currIndex < cce.length) 
            cce[currIndex++] = bitOffset;
            
            
            changingElemSize = currIndex;
            
            lineOffset += scanlineStride;
        }
    }
    
    private void setToBlack(byte[] buffer,
    int lineOffset, int bitOffset,
    int numBits) {
        int bitNum = 8*lineOffset + bitOffset;
        int lastBit = bitNum + numBits;
        
        int byteNum = bitNum >> 3;
        
        
        int shift = bitNum & 0x7;
        if (shift > 0) {
            int maskVal = 1 << (7 - shift);
            byte val = buffer[byteNum];
            while (maskVal > 0 && bitNum < lastBit) {
                val |= maskVal;
                maskVal >>= 1;
                ++bitNum;
            }
            buffer[byteNum] = val;
        }
        
        
        byteNum = bitNum >> 3;
        while (bitNum < lastBit - 7) {
            buffer[byteNum++] = (byte)255;
            bitNum += 8;
        }
        
        
        while (bitNum < lastBit) {
            byteNum = bitNum >> 3;
            buffer[byteNum] |= 1 << (7 - (bitNum & 0x7));
            ++bitNum;
        }
    }
    
    
    private int decodeWhiteCodeWord() {
        int current, entry, bits, isT, twoBits, code = -1;
        int runLength = 0;
        boolean isWhite = true;
        
        while (isWhite) {
            current = nextNBits(10);
            entry = white[current];
            
            
            isT = entry & 0x0001;
            bits = (entry >>> 1) & 0x0f;
            
            if (bits == 12) {           
                
                twoBits = nextLesserThan8Bits(2);
                
                current = ((current << 2) & 0x000c) | twoBits;
                entry = additionalMakeup[current];
                bits = (entry >>> 1) & 0x07;     
                code = (entry >>> 4) & 0x0fff;   
                runLength += code;
                updatePointer(4 - bits);
            } else if (bits == 0) {     
                throw new RuntimeException("Invalid code encountered.");
            } else if (bits == 15) {    
                throw new RuntimeException("EOL code word encountered in White run.");
            } else {
                
                code = (entry >>> 5) & 0x07ff;
                runLength += code;
                updatePointer(10 - bits);
                if (isT == 0) {
                    isWhite = false;
                }
            }
        }
        
        return runLength;
    }
    
    
    private int decodeBlackCodeWord() {
        int current, entry, bits, isT, code = -1;
        int runLength = 0;
        boolean isWhite = false;
        
        while (!isWhite) {
            current = nextLesserThan8Bits(4);
            entry = initBlack[current];
            
            
            isT = entry & 0x0001;
            bits = (entry >>> 1) & 0x000f;
            code = (entry >>> 5) & 0x07ff;
            
            if (code == 100) {
                current = nextNBits(9);
                entry = black[current];
                
                
                isT = entry & 0x0001;
                bits = (entry >>> 1) & 0x000f;
                code = (entry >>> 5) & 0x07ff;
                
                if (bits == 12) {
                    
                    updatePointer(5);
                    current = nextLesserThan8Bits(4);
                    entry = additionalMakeup[current];
                    bits = (entry >>> 1) & 0x07;     
                    code  = (entry >>> 4) & 0x0fff;  
                    runLength += code;
                    
                    updatePointer(4 - bits);
                } else if (bits == 15) {
                    
                    throw new RuntimeException("EOL code word encountered in Black run.");
                } else {
                    runLength += code;
                    updatePointer(9 - bits);
                    if (isT == 0) {
                        isWhite = true;
                    }
                }
            } else if (code == 200) {
                
                current = nextLesserThan8Bits(2);
                entry = twoBitBlack[current];
                code = (entry >>> 5) & 0x07ff;
                runLength += code;
                bits = (entry >>> 1) & 0x0f;
                updatePointer(2 - bits);
                isWhite = true;
            } else {
                
                runLength += code;
                updatePointer(4 - bits);
                isWhite = true;
            }
        }
        
        return runLength;
    }
    
    private int readEOL(boolean isFirstEOL) {
        if (fillBits == 0) {
            int next12Bits = nextNBits(12);
            if (isFirstEOL && next12Bits == 0) {
                
                
                
                
                
                
                if(nextNBits(4) == 1) {
                    
                    
                    
                    fillBits = 1;
                    return 1;
                }
            }
            if(next12Bits != 1) {
                throw new RuntimeException("Scanline must begin with EOL code word.");
            }
        } else if (fillBits == 1) {
            
            
            
            
            
            int bitsLeft = 8 - bitPointer;
            
            if (nextNBits(bitsLeft) != 0) {
                throw new RuntimeException("All fill bits preceding EOL code must be 0.");
            }
            
            
            
            
            
            if (bitsLeft < 4) {
                if (nextNBits(8) != 0) {
                    throw new RuntimeException("All fill bits preceding EOL code must be 0.");
                }
            }
            
            
            
            
            int n;
            while ((n = nextNBits(8)) != 1) {
                
                
                if (n != 0) {
                    throw new RuntimeException("All fill bits preceding EOL code must be 0.");
                }
            }
        }
        
        
        if (oneD == 0) {
            return 1;
        } else {
            
            
            return nextLesserThan8Bits(1);
        }
    }
    
    private void getNextChangingElement(int a0, boolean isWhite, int[] ret) {
        
        int[] pce = this.prevChangingElems;
        int ces = this.changingElemSize;
        
        
        
        
        int start = lastChangingElement > 0 ? lastChangingElement - 1 : 0;
        if (isWhite) {
            start &= ~0x1; 
        } else {
            start |= 0x1; 
        }
        
        int i = start;
        for (; i < ces; i += 2) {
            int temp = pce[i];
            if (temp > a0) {
                lastChangingElement = i;
                ret[0] = temp;
                break;
            }
        }
        
        if (i + 1 < ces) {
            ret[1] = pce[i + 1];
        }
    }
    
    private int nextNBits(int bitsToGet) {
        byte b, next, next2next;
        int l = data.length - 1;
        int bp = this.bytePointer;
        
        if (fillOrder == 1) {
            b = data[bp];
            
            if (bp == l) {
                next = 0x00;
                next2next = 0x00;
            } else if ((bp + 1) == l) {
                next = data[bp + 1];
                next2next = 0x00;
            } else {
                next = data[bp + 1];
                next2next = data[bp + 2];
            }
        } else if (fillOrder == 2) {
            b = flipTable[data[bp] & 0xff];
            
            if (bp == l) {
                next = 0x00;
                next2next = 0x00;
            } else if ((bp + 1) == l) {
                next = flipTable[data[bp + 1] & 0xff];
                next2next = 0x00;
            } else {
                next = flipTable[data[bp + 1] & 0xff];
                next2next = flipTable[data[bp + 2] & 0xff];
            }
        } else {
            throw new RuntimeException("TIFF_FILL_ORDER tag must be either 1 or 2.");
        }
        
        int bitsLeft = 8 - bitPointer;
        int bitsFromNextByte = bitsToGet - bitsLeft;
        int bitsFromNext2NextByte = 0;
        if (bitsFromNextByte > 8) {
            bitsFromNext2NextByte = bitsFromNextByte - 8;
            bitsFromNextByte = 8;
        }
        
        bytePointer++;
        
        int i1 = (b & table1[bitsLeft]) << (bitsToGet - bitsLeft);
        int i2 = (next & table2[bitsFromNextByte]) >>> (8 - bitsFromNextByte);
        
        int i3 = 0;
        if (bitsFromNext2NextByte != 0) {
            i2 <<= bitsFromNext2NextByte;
            i3 = (next2next & table2[bitsFromNext2NextByte]) >>>
            (8 - bitsFromNext2NextByte);
            i2 |= i3;
            bytePointer++;
            bitPointer = bitsFromNext2NextByte;
        } else {
            if (bitsFromNextByte == 8) {
                bitPointer = 0;
                bytePointer++;
            } else {
                bitPointer = bitsFromNextByte;
            }
        }
        
        int i = i1 | i2;
        return i;
    }
    
    private int nextLesserThan8Bits(int bitsToGet) {
        byte b, next;
        int l = data.length - 1;
        int bp = this.bytePointer;
        
        if (fillOrder == 1) {
            b = data[bp];
            if (bp == l) {
                next = 0x00;
            } else {
                next = data[bp + 1];
            }
        } else if (fillOrder == 2) {
            b = flipTable[data[bp] & 0xff];
            if (bp == l) {
                next = 0x00;
            } else {
                next = flipTable[data[bp + 1] & 0xff];
            }
        } else {
            throw new RuntimeException("TIFF_FILL_ORDER tag must be either 1 or 2.");
        }
        
        int bitsLeft = 8 - bitPointer;
        int bitsFromNextByte = bitsToGet - bitsLeft;
        
        int shift = bitsLeft - bitsToGet;
        int i1, i2;
        if (shift >= 0) {
            i1 = (b & table1[bitsLeft]) >>> shift;
            bitPointer += bitsToGet;
            if (bitPointer == 8) {
                bitPointer = 0;
                bytePointer++;
            }
        } else {
            i1 = (b & table1[bitsLeft]) << (-shift);
            i2 = (next & table2[bitsFromNextByte]) >>> (8 - bitsFromNextByte);
            
            i1 |= i2;
            bytePointer++;
            bitPointer = bitsFromNextByte;
        }
        
        return i1;
    }
    
    
    private void updatePointer(int bitsToMoveBack) {
        int i = bitPointer - bitsToMoveBack;
        
        if (i < 0) {
            bytePointer--;
            bitPointer = 8 + i;
        } else {
            bitPointer = i;
        }
    }
    
    
    private boolean advancePointer() {
        if (bitPointer != 0) {
            bytePointer++;
            bitPointer = 0;
        }
        
        return true;
    }
}

