
package com.lowagie.text.pdf.codec;


public class TIFFConstants {
    

    public static final int TIFFTAG_SUBFILETYPE = 254;    
    public static final int     FILETYPE_REDUCEDIMAGE = 0x1;    
    public static final int     FILETYPE_PAGE = 0x2;    
    public static final int     FILETYPE_MASK = 0x4;    
    public static final int TIFFTAG_OSUBFILETYPE = 255; 
    public static final int     OFILETYPE_IMAGE = 1; 
    public static final int     OFILETYPE_REDUCEDIMAGE = 2; 
    public static final int     OFILETYPE_PAGE = 3; 
    public static final int TIFFTAG_IMAGEWIDTH = 256; 
    public static final int TIFFTAG_IMAGELENGTH = 257; 
    public static final int TIFFTAG_BITSPERSAMPLE = 258; 
    public static final int TIFFTAG_COMPRESSION = 259; 
    public static final int     COMPRESSION_NONE = 1; 
    public static final int     COMPRESSION_CCITTRLE = 2; 
    public static final int     COMPRESSION_CCITTFAX3 = 3;    
    public static final int     COMPRESSION_CCITTFAX4 = 4;    
    public static final int     COMPRESSION_LZW = 5;       
    public static final int     COMPRESSION_OJPEG = 6; 
    public static final int     COMPRESSION_JPEG = 7; 
    public static final int     COMPRESSION_NEXT = 32766; 
    public static final int     COMPRESSION_CCITTRLEW = 32771; 
    public static final int     COMPRESSION_PACKBITS = 32773; 
    public static final int     COMPRESSION_THUNDERSCAN = 32809; 
    
    public static final int     COMPRESSION_IT8CTPAD = 32895;   
    public static final int     COMPRESSION_IT8LW = 32896;   
    public static final int     COMPRESSION_IT8MP = 32897;   
    public static final int     COMPRESSION_IT8BL = 32898;   
    
    public static final int     COMPRESSION_PIXARFILM = 32908;   
    public static final int     COMPRESSION_PIXARLOG = 32909;   
    public static final int     COMPRESSION_DEFLATE = 32946; 
    public static final int     COMPRESSION_ADOBE_DEFLATE = 8;       
    
    public static final int     COMPRESSION_DCS = 32947;   
    public static final int     COMPRESSION_JBIG = 34661; 
    public static final int     COMPRESSION_SGILOG = 34676; 
    public static final int     COMPRESSION_SGILOG24 = 34677;    
    public static final int TIFFTAG_PHOTOMETRIC = 262; 
    public static final int     PHOTOMETRIC_MINISWHITE = 0; 
    public static final int     PHOTOMETRIC_MINISBLACK = 1; 
    public static final int     PHOTOMETRIC_RGB = 2; 
    public static final int     PHOTOMETRIC_PALETTE = 3; 
    public static final int     PHOTOMETRIC_MASK = 4; 
    public static final int     PHOTOMETRIC_SEPARATED = 5; 
    public static final int     PHOTOMETRIC_YCBCR = 6; 
    public static final int     PHOTOMETRIC_CIELAB = 8; 
    public static final int     PHOTOMETRIC_LOGL = 32844; 
    public static final int     PHOTOMETRIC_LOGLUV = 32845; 
    public static final int TIFFTAG_THRESHHOLDING = 263; 
    public static final int     THRESHHOLD_BILEVEL = 1; 
    public static final int     THRESHHOLD_HALFTONE = 2; 
    public static final int     THRESHHOLD_ERRORDIFFUSE = 3; 
    public static final int TIFFTAG_CELLWIDTH = 264; 
    public static final int TIFFTAG_CELLLENGTH = 265; 
    public static final int TIFFTAG_FILLORDER = 266; 
    public static final int     FILLORDER_MSB2LSB = 1; 
    public static final int     FILLORDER_LSB2MSB = 2; 
    public static final int TIFFTAG_DOCUMENTNAME = 269; 
    public static final int TIFFTAG_IMAGEDESCRIPTION = 270; 
    public static final int TIFFTAG_MAKE = 271; 
    public static final int TIFFTAG_MODEL = 272; 
    public static final int TIFFTAG_STRIPOFFSETS = 273; 
    public static final int TIFFTAG_ORIENTATION = 274; 
    public static final int     ORIENTATION_TOPLEFT = 1; 
    public static final int     ORIENTATION_TOPRIGHT = 2; 
    public static final int     ORIENTATION_BOTRIGHT = 3; 
    public static final int     ORIENTATION_BOTLEFT = 4; 
    public static final int     ORIENTATION_LEFTTOP = 5; 
    public static final int     ORIENTATION_RIGHTTOP = 6; 
    public static final int     ORIENTATION_RIGHTBOT = 7; 
    public static final int     ORIENTATION_LEFTBOT = 8; 
    public static final int TIFFTAG_SAMPLESPERPIXEL = 277; 
    public static final int TIFFTAG_ROWSPERSTRIP = 278; 
    public static final int TIFFTAG_STRIPBYTECOUNTS = 279; 
    public static final int TIFFTAG_MINSAMPLEVALUE = 280; 
    public static final int TIFFTAG_MAXSAMPLEVALUE = 281; 
    public static final int TIFFTAG_XRESOLUTION = 282; 
    public static final int TIFFTAG_YRESOLUTION = 283; 
    public static final int TIFFTAG_PLANARCONFIG = 284; 
    public static final int     PLANARCONFIG_CONTIG = 1; 
    public static final int     PLANARCONFIG_SEPARATE = 2; 
    public static final int TIFFTAG_PAGENAME = 285; 
    public static final int TIFFTAG_XPOSITION = 286; 
    public static final int TIFFTAG_YPOSITION = 287; 
    public static final int TIFFTAG_FREEOFFSETS = 288; 
    public static final int TIFFTAG_FREEBYTECOUNTS = 289; 
    public static final int TIFFTAG_GRAYRESPONSEUNIT = 290; 
    public static final int     GRAYRESPONSEUNIT_10S = 1; 
    public static final int     GRAYRESPONSEUNIT_100S = 2; 
    public static final int     GRAYRESPONSEUNIT_1000S = 3; 
    public static final int     GRAYRESPONSEUNIT_10000S = 4; 
    public static final int     GRAYRESPONSEUNIT_100000S = 5; 
    public static final int TIFFTAG_GRAYRESPONSECURVE = 291; 
    public static final int TIFFTAG_GROUP3OPTIONS = 292; 
    public static final int     GROUP3OPT_2DENCODING = 0x1;    
    public static final int     GROUP3OPT_UNCOMPRESSED = 0x2;    
    public static final int     GROUP3OPT_FILLBITS = 0x4;    
    public static final int TIFFTAG_GROUP4OPTIONS = 293; 
    public static final int     GROUP4OPT_UNCOMPRESSED = 0x2;    
    public static final int TIFFTAG_RESOLUTIONUNIT = 296; 
    public static final int     RESUNIT_NONE = 1; 
    public static final int     RESUNIT_INCH = 2; 
    public static final int     RESUNIT_CENTIMETER = 3;    
    public static final int TIFFTAG_PAGENUMBER = 297;    
    public static final int TIFFTAG_COLORRESPONSEUNIT = 300;    
    public static final int     COLORRESPONSEUNIT_10S = 1;    
    public static final int     COLORRESPONSEUNIT_100S = 2;    
    public static final int     COLORRESPONSEUNIT_1000S = 3;    
    public static final int     COLORRESPONSEUNIT_10000S = 4;    
    public static final int     COLORRESPONSEUNIT_100000S = 5;    
    public static final int TIFFTAG_TRANSFERFUNCTION = 301;    
    public static final int TIFFTAG_SOFTWARE = 305;    
    public static final int TIFFTAG_DATETIME = 306;    
    public static final int TIFFTAG_ARTIST = 315;    
    public static final int TIFFTAG_HOSTCOMPUTER = 316;    
    public static final int TIFFTAG_PREDICTOR = 317;    
    public static final int TIFFTAG_WHITEPOINT = 318;    
    public static final int TIFFTAG_PRIMARYCHROMATICITIES = 319;    
    public static final int TIFFTAG_COLORMAP = 320;    
    public static final int TIFFTAG_HALFTONEHINTS = 321;    
    public static final int TIFFTAG_TILEWIDTH = 322;    
    public static final int TIFFTAG_TILELENGTH = 323;    
    public static final int TIFFTAG_TILEOFFSETS = 324;    
    public static final int TIFFTAG_TILEBYTECOUNTS = 325;    
    public static final int TIFFTAG_BADFAXLINES = 326;    
    public static final int TIFFTAG_CLEANFAXDATA = 327;    
    public static final int     CLEANFAXDATA_CLEAN = 0;    
    public static final int     CLEANFAXDATA_REGENERATED = 1;    
    public static final int     CLEANFAXDATA_UNCLEAN = 2;    
    public static final int TIFFTAG_CONSECUTIVEBADFAXLINES = 328;    
    public static final int TIFFTAG_SUBIFD = 330;    
    public static final int TIFFTAG_INKSET = 332;    
    public static final int     INKSET_CMYK = 1;    
    public static final int TIFFTAG_INKNAMES = 333;    
    public static final int TIFFTAG_NUMBEROFINKS = 334;    
    public static final int TIFFTAG_DOTRANGE = 336;    
    public static final int TIFFTAG_TARGETPRINTER = 337;    
    public static final int TIFFTAG_EXTRASAMPLES = 338;    
    public static final int     EXTRASAMPLE_UNSPECIFIED = 0;    
    public static final int     EXTRASAMPLE_ASSOCALPHA = 1;    
    public static final int     EXTRASAMPLE_UNASSALPHA = 2;    
    public static final int TIFFTAG_SAMPLEFORMAT = 339;    
    public static final int     SAMPLEFORMAT_UINT = 1;    
    public static final int     SAMPLEFORMAT_INT = 2;    
    public static final int     SAMPLEFORMAT_IEEEFP = 3;    
    public static final int     SAMPLEFORMAT_VOID = 4;    
    public static final int     SAMPLEFORMAT_COMPLEXINT = 5;    
    public static final int     SAMPLEFORMAT_COMPLEXIEEEFP = 6;    
    public static final int TIFFTAG_SMINSAMPLEVALUE = 340;    
    public static final int TIFFTAG_SMAXSAMPLEVALUE = 341;    
    public static final int TIFFTAG_JPEGTABLES = 347;    

    public static final int TIFFTAG_JPEGPROC = 512;    
    public static final int     JPEGPROC_BASELINE = 1;    
    public static final int     JPEGPROC_LOSSLESS = 14;    
    public static final int TIFFTAG_JPEGIFOFFSET = 513;    
    public static final int TIFFTAG_JPEGIFBYTECOUNT = 514;    
    public static final int TIFFTAG_JPEGRESTARTINTERVAL = 515;    
    public static final int TIFFTAG_JPEGLOSSLESSPREDICTORS = 517;    
    public static final int TIFFTAG_JPEGPOINTTRANSFORM = 518;    
    public static final int TIFFTAG_JPEGQTABLES = 519;    
    public static final int TIFFTAG_JPEGDCTABLES = 520;    
    public static final int TIFFTAG_JPEGACTABLES = 521;    
    public static final int TIFFTAG_YCBCRCOEFFICIENTS = 529;    
    public static final int TIFFTAG_YCBCRSUBSAMPLING = 530;    
    public static final int TIFFTAG_YCBCRPOSITIONING = 531;    
    public static final int     YCBCRPOSITION_CENTERED = 1;    
    public static final int     YCBCRPOSITION_COSITED = 2;    
    public static final int TIFFTAG_REFERENCEBLACKWHITE = 532;    
    
    public static final int TIFFTAG_REFPTS = 32953;    
    public static final int TIFFTAG_REGIONTACKPOINT = 32954;    
    public static final int TIFFTAG_REGIONWARPCORNERS = 32955;    
    public static final int TIFFTAG_REGIONAFFINE = 32956;    
    
    public static final int TIFFTAG_MATTEING = 32995;    
    public static final int TIFFTAG_DATATYPE = 32996;    
    public static final int TIFFTAG_IMAGEDEPTH = 32997;    
    public static final int TIFFTAG_TILEDEPTH = 32998;    
    

    public static final int TIFFTAG_PIXAR_IMAGEFULLWIDTH = 33300;   
    public static final int TIFFTAG_PIXAR_IMAGEFULLLENGTH = 33301;   
 
    public static final int TIFFTAG_PIXAR_TEXTUREFORMAT = 33302;    
    public static final int TIFFTAG_PIXAR_WRAPMODES = 33303;    
    public static final int TIFFTAG_PIXAR_FOVCOT = 33304;    
    public static final int TIFFTAG_PIXAR_MATRIX_WORLDTOSCREEN = 33305;
    public static final int TIFFTAG_PIXAR_MATRIX_WORLDTOCAMERA = 33306;
    
    public static final int TIFFTAG_WRITERSERIALNUMBER = 33405;   
    
    public static final int TIFFTAG_COPYRIGHT = 33432;    
    
    public static final int TIFFTAG_RICHTIFFIPTC = 33723;
    
    public static final int TIFFTAG_IT8SITE = 34016;    
    public static final int TIFFTAG_IT8COLORSEQUENCE = 34017;    
    public static final int TIFFTAG_IT8HEADER = 34018;    
    public static final int TIFFTAG_IT8RASTERPADDING = 34019;    
    public static final int TIFFTAG_IT8BITSPERRUNLENGTH = 34020;    
    public static final int TIFFTAG_IT8BITSPEREXTENDEDRUNLENGTH = 34021;
    public static final int TIFFTAG_IT8COLORTABLE = 34022;    
    public static final int TIFFTAG_IT8IMAGECOLORINDICATOR = 34023;    
    public static final int TIFFTAG_IT8BKGCOLORINDICATOR = 34024;    
    public static final int TIFFTAG_IT8IMAGECOLORVALUE = 34025;    
    public static final int TIFFTAG_IT8BKGCOLORVALUE = 34026;    
    public static final int TIFFTAG_IT8PIXELINTENSITYRANGE = 34027;    
    public static final int TIFFTAG_IT8TRANSPARENCYINDICATOR = 34028;    
    public static final int TIFFTAG_IT8COLORCHARACTERIZATION = 34029;    
    
    public static final int TIFFTAG_FRAMECOUNT = 34232;   
    
    public static final int TIFFTAG_ICCPROFILE = 34675;    
    
    public static final int TIFFTAG_PHOTOSHOP = 34377;
    
    public static final int TIFFTAG_JBIGOPTIONS = 34750;    
    
    public static final int TIFFTAG_FAXRECVPARAMS = 34908;    
    public static final int TIFFTAG_FAXSUBADDRESS = 34909;    
    public static final int TIFFTAG_FAXRECVTIME = 34910;    
    
    public static final int TIFFTAG_STONITS = 37439;    
    
    public static final int TIFFTAG_FEDEX_EDR = 34929;    
    
    public static final int TIFFTAG_DCSHUESHIFTVALUES = 65535;   
    
}
