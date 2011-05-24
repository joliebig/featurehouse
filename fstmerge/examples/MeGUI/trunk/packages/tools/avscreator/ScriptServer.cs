// ****************************************************************************
// 
// Copyright (C) 2005-2009  Doom9 & al
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// 
// ****************************************************************************

using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using System.Windows.Forms;

namespace MeGUI
{
    public enum ResizeFilterType
    {

        [EnumTitle("Bilinear (Soft)", "BilinearResize({0},{1})")]
        Bilinear=0,
        [EnumTitle("Bicubic (Sharp)", "BicubicResize({0},{1},0,0.75)")]
        BicubicSharp,
        [EnumTitle("Bicubic (Neutral)", "BicubicResize({0},{1},0,0.5)")]
        BicubicNeutral,
        [EnumTitle("Bicubic (Soft)", "BicubicResize({0},{1},0.333,0.333)")]
        BicubicSoft,
        [EnumTitle("Lanczos (Sharp)", "LanczosResize({0},{1})")]
        Lanczos,
        [EnumTitle("Lanczos4 (Sharp)", "Lanczos4Resize({0},{1})")]
        Lanczos4,
        [EnumTitle("Gauss (Neutral)", "GaussResize({0},{1})")]
        Gauss,
        [EnumTitle("Point (Sharp)", "PointResize({0},{1})")]
        Point,
        [EnumTitle("Spline16 (Neutral)", "Spline16Resize({0},{1})")]
        Spline16,
        [EnumTitle("Spline36 (Neutral)", "Spline36Resize({0},{1})")]
        Spline32,
        [EnumTitle("Spline64 (Sharp)", "Spline64Resize({0},{1})")]
        Spline64

    }

    public enum DenoiseFilterType
    {

        [EnumTitle("Minimal Noise", "Undot()")]
        MinimalNoise = 0,
        [EnumTitle("Little Noise", "mergechroma(blur(1.3))")]
        LittleNoise,
        [EnumTitle("Medium Noise", "FluxSmoothST(7,7)")]
        MediumNoise,
        [EnumTitle("Heavy Noise", "Convolution3D(\"movielq\")")]
        HeavyNoise
    }

    public enum NvDeinterlacerType
    {
        [EnumTitle("No Deinterlacing", ", deinterlace=0")]
        nvDeInterlacerNone = 0,
        [EnumTitle("Single Rate Deinterlacing", ", deinterlace=1")]
        nvDeInterlacerSingle,
        [EnumTitle("Bobbing", ", deinterlace=2")]
        nvDeInterlacerDouble
    }

    public enum UserSourceType
    {
        [EnumTitle("Progressive", SourceType.PROGRESSIVE)]
        Progressive,
        [EnumTitle("Interlaced", SourceType.INTERLACED)]
        Interlaced,
        [EnumTitle("Film", SourceType.FILM)]
        Film,
        [EnumTitle("M-in-5 decimation required", SourceType.DECIMATING)]
        Decimating,
        [EnumTitle("Hybrid film/interlaced. Mostly film", SourceType.HYBRID_FILM_INTERLACED)]
        HybridFilmInterlaced,
        [EnumTitle("Hybrid film/interlaced. Mostly interlaced", SourceType.HYBRID_FILM_INTERLACED)]
        HybridInterlacedFilm,
        [EnumTitle("Partially interlaced", SourceType.HYBRID_PROGRESSIVE_INTERLACED)]
        HybridProgressiveInterlaced,
        [EnumTitle("Partially film", SourceType.HYBRID_PROGRESSIVE_FILM)]
        HybridProgressiveFilm
    }

    public enum UserFieldOrder
    {
        [EnumTitle("Top Field First", FieldOrder.TFF)]
        TFF,
        [EnumTitle("Bottom Field First", FieldOrder.BFF)]
        BFF,
        [EnumTitle("Varying field order", FieldOrder.VARIABLE)]
        Varying
    }

    public class ScriptServer
    {
        public static readonly IList ListOfResizeFilterType = EnumProxy.CreateArray(typeof(ResizeFilterType));

        public static readonly IList ListOfDenoiseFilterType = EnumProxy.CreateArray(typeof(DenoiseFilterType));

        public static readonly IList ListOfSourceTypes = EnumProxy.CreateArray(typeof(UserSourceType));

        public static readonly IList ListOfFieldOrders = EnumProxy.CreateArray(typeof(UserFieldOrder));

        public static readonly IList ListOfNvDeIntType = EnumProxy.CreateArray(typeof(NvDeinterlacerType));

        public static MainForm mainForm;
        

        public static string CreateScriptFromTemplate(string template, string inputLine, string cropLine, string resizeLine, string denoiseLines, string deinterlaceLines)
        {
            string newScript = template;
            newScript = newScript.Replace("<crop>", cropLine);
            newScript = newScript.Replace("<resize>", resizeLine);
            newScript = newScript.Replace("<denoise>", denoiseLines);
            newScript = newScript.Replace("<deinterlace>", deinterlaceLines);
            newScript = newScript.Replace("<input>", inputLine);
            return newScript;
        }

        public static string GetInputLine(string input, bool interlaced, PossibleSources sourceType,
            bool colormatrix, bool mpeg2deblock, bool flipVertical, double fps, bool dss2)
        {
            string inputLine = "#input";
            string strDLLPath = "";

            switch (sourceType)
            {
                case PossibleSources.avs:
                    inputLine = "Import(\"" + input + "\")";
                    break;
                case PossibleSources.d2v:
                    strDLLPath = Path.Combine(Path.GetDirectoryName(MainForm.Instance.Settings.DgIndexPath), "DGDecode.dll");
                    inputLine = "LoadPlugin(\"" + strDLLPath + "\")\r\nDGDecode_mpeg2source(\"" + input + "\"";
                    if (mpeg2deblock)
                        inputLine += ", cpu=4";
                    if (colormatrix)
                        inputLine += ", info=3";
                    inputLine += ")";
                    if (colormatrix)
                        inputLine += string.Format("\r\nLoadPlugin(\"" + Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "ColorMatrix.dll") + "\")\r\nColorMatrix(hints=true{0}, threads=0)", interlaced ? ", interlaced=true" : "");
                    break;
                case PossibleSources.dga:
                    strDLLPath = Path.Combine(Path.GetDirectoryName(MainForm.Instance.Settings.DgavcIndexPath), "DGAVCDecode.dll");
                    inputLine = "LoadPlugin(\"" + strDLLPath + "\")\r\nAVCSource(\"" + input + "\")"; 
                    break;
                case PossibleSources.ffindex:
                    strDLLPath = Path.Combine(Path.GetDirectoryName(MainForm.Instance.Settings.FFMSIndexPath), "ffms2.dll");

                    if (input.ToLower().EndsWith(".ffindex"))
                        inputLine = "LoadPlugin(\"" + strDLLPath + "\")\r\nFFVideoSource(\"" + input.Substring(0, input.Length - 8) + "\",colorspace=\"YV12\")";
                    else
                        inputLine = "LoadPlugin(\"" + strDLLPath + "\")\r\nFFVideoSource(\"" + input + "\",colorspace=\"YV12\")";


                    if (input.ToLower().EndsWith(".ffindex"))
                        inputLine = "LoadCPlugin(\"" + strDLLPath + "\")\r\nFFVideoSource(\"" + input.Substring(0, input.Length - 8) + "\",colorspace=\"YV12\")";
                    else
                        inputLine = "LoadCPlugin(\"" + strDLLPath + "\")\r\nFFVideoSource(\"" + input + "\",colorspace=\"YV12\")";

                    break;
                case PossibleSources.dgi:
                    if (MainForm.Instance.Settings.UseCUVIDserver == true)
                    {
                        strDLLPath = Path.Combine(Path.GetDirectoryName(MainForm.Instance.Settings.DgnvIndexPath), "DGDecodeNV.dll");
                        inputLine = "LoadPlugin(\"" + strDLLPath + "\")\r\nDGSource(\"" + input + "\"";
                    }
                    else
                    {
                        strDLLPath = Path.Combine(Path.GetDirectoryName(MainForm.Instance.Settings.DgnvIndexPath), "DGMultiDecodeNV.dll");
                        inputLine = "LoadPlugin(\"" + strDLLPath + "\")\r\nDGMultiSource(\"" + input + "\"";
                    }
                    if (MainForm.Instance.Settings.AutoForceFilm &&
                        MainForm.Instance.Settings.ForceFilmThreshold <= (decimal)dgiFile.GetFilmPercent(input))
                        inputLine += ",fieldop=1";
                    else
                        inputLine += ",fieldop=0";
                    break;
                case PossibleSources.vdr:
                    inputLine = "AVISource(\"" + input + "\", audio=false)";
                    break;
                case PossibleSources.directShow:
                    if (input.ToLower().EndsWith(".avi"))
                    {
                        inputLine = "AVISource(\"" + input + "\", audio=false)";
                    }
                    else
                    {
                        if (dss2)
                            inputLine = "LoadPlugin(\"" + MeGUISettings.HaaliMSPath + "\\avss.dll" + "\")\r\ndss2(\"" + input + "\"" + ((fps > 0) ? ", fps=" + fps.ToString("F3", new CultureInfo("en-us")) : string.Empty) + ")";
                        else
                            inputLine = "DirectShowSource(\"" + input + "\"" + ((fps > 0) ? ", fps=" + fps.ToString("F3", new CultureInfo("en-us")) : string.Empty) + ", audio=false, convertfps=true)";
                        if (flipVertical)
                            inputLine = inputLine + "\r\nFlipVertical()";
                    }
                    break;
            }
            return inputLine;
        }

        public static string GetCropLine(bool crop, CropValues cropValues)
        {
            return GetCropLine(crop, cropValues.left, cropValues.top, cropValues.right, cropValues.bottom);
        }

        public static string GetCropLine(bool crop, int cropLeft, int cropTop, int cropRight, int cropBottom)
        {
            string cropLine = "#crop";
            if (crop)
            {
                cropLine = string.Format("crop( {0}, {1}, {2}, {3}){4}",cropLeft, cropTop, -cropRight, -cropBottom, Environment.NewLine );
            }
            return cropLine;
        }

        public static string GetResizeLine(bool resize, int hres, int vres, ResizeFilterType type)
        {
            if (!resize)
                return "#resize";
            EnumProxy p = EnumProxy.Create(type);
            if (p.Tag != null)
                return string.Format(p.Tag + " # {2}", hres, vres, p);
            else
                return "#resize - " + p;
        }

        public static string GetDenoiseLines(bool denoise, DenoiseFilterType type)
        {
            string denoiseLines = "#denoise";
            string strPath = "";
            if (denoise)
            {
                EnumProxy p = EnumProxy.Create(type);
                if (p.Tag != null)
                {
                    if (p.Tag.ToString().ToLower().Contains("undot"))
                        strPath = "LoadPlugin(\"" + Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "UnDot.dll") + "\")\r\n";
                    else if (p.Tag.ToString().ToLower().Contains("fluxsmoothst"))
                        strPath = "LoadPlugin(\"" + Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "FluxSmooth.dll") + "\")\r\n";
                    else if (p.Tag.ToString().ToLower().Contains("convolution3d"))
                        strPath = "LoadPlugin(\"" + Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "Convolution3DYV12.dll") + "\")\r\n";
                    denoiseLines = string.Format(strPath + p.Tag + " # " + p);
                }
                else
                    denoiseLines = "#denoise - " + p;
            }
            return denoiseLines;
        }

        public static string GetNvDeInterlacerLine(bool deint, NvDeinterlacerType type)
        {
            string nvDeInterlacerLine = "";
            if (deint)
            {
                EnumProxy p = EnumProxy.Create(type);
                if (p.Tag != null)
                    nvDeInterlacerLine = p.Tag.ToString();
            }
            return nvDeInterlacerLine;
        }


        public static List<DeinterlaceFilter> GetDeinterlacers(SourceInfo info)
        {
            List<DeinterlaceFilter> filters = new List<DeinterlaceFilter>();
            if (info.sourceType == SourceType.PROGRESSIVE)
            {
                filters.Add(new DeinterlaceFilter(
                    "Do nothing",
                    "#Not doing anything because the source is progressive"));
            }
            else if (info.sourceType == SourceType.DECIMATING)
            {
                ScriptServer.AddTDecimate(info.decimateM, filters);
            }
            else if (info.sourceType == SourceType.INTERLACED)
            {
                ScriptServer.AddYadif(info.fieldOrder, filters, false);
                ScriptServer.AddYadif(info.fieldOrder, filters, true);
                ScriptServer.AddTDeint(info.fieldOrder, filters, true, false, false);
                ScriptServer.AddTDeint(info.fieldOrder, filters, true, false, true);
                ScriptServer.AddTDeint(info.fieldOrder, filters, true, true, false);
                ScriptServer.AddTDeint(info.fieldOrder, filters, true, true, true);
                if (info.fieldOrder != FieldOrder.VARIABLE)
                    ScriptServer.AddLeakDeint(info.fieldOrder, filters);
                ScriptServer.AddTMC(info.fieldOrder, filters);
                ScriptServer.AddFieldDeint(info.fieldOrder, filters, true, true);
                ScriptServer.AddFieldDeint(info.fieldOrder, filters, true, false);
            }
            else if (info.sourceType == SourceType.FILM)
            {
                ScriptServer.AddTIVTC("", info.isAnime, false, true, false, info.fieldOrder, filters);
                ScriptServer.AddIVTC(info.fieldOrder, false, true, filters);
            }
            else if (info.sourceType == SourceType.HYBRID_FILM_INTERLACED ||
                info.sourceType == SourceType.HYBRID_PROGRESSIVE_FILM)
            {
                ScriptServer.AddTIVTC("", info.isAnime, true, info.majorityFilm, false,
                    info.fieldOrder, filters);
                ScriptServer.AddTIVTC("", info.isAnime, true, info.majorityFilm, true,
                    info.fieldOrder, filters);
                ScriptServer.AddIVTC(info.fieldOrder, true, info.majorityFilm, filters);
            }
            else if (info.sourceType == SourceType.HYBRID_PROGRESSIVE_INTERLACED)
            {
                ScriptServer.AddYadif(info.fieldOrder, filters, false);
                ScriptServer.AddYadif(info.fieldOrder, filters, true);
                ScriptServer.AddTDeint(info.fieldOrder, filters, false, false, false);
                ScriptServer.AddTDeint(info.fieldOrder, filters, true, false, true);
                ScriptServer.AddTDeint(info.fieldOrder, filters, true, true, false);
                ScriptServer.AddTDeint(info.fieldOrder, filters, false, true, true);
                ScriptServer.AddFieldDeint(info.fieldOrder, filters, false, true);
                ScriptServer.AddFieldDeint(info.fieldOrder, filters, false, false);
                if (info.fieldOrder != FieldOrder.VARIABLE)
                    ScriptServer.AddLeakDeint(info.fieldOrder, filters);
                ScriptServer.AddTMC(info.fieldOrder, filters);
            }
            return filters;
        }

        
        public static int Order(FieldOrder order)
        {
            int i_order = -1;
            if (order == FieldOrder.BFF)
                i_order = 0;
            if (order == FieldOrder.TFF)
                i_order = 1;
            return i_order;
        }

        public static void AddYadif(FieldOrder order, List<DeinterlaceFilter> filters, bool bobber)
        {
            filters.Add(new DeinterlaceFilter(
                bobber ? "Yadif (with Bob)" : "Yadif",
                string.Format("Load_Stdcall_Plugin(\"{0}\"){1}Yadif({2}order={3})", 
                    MainForm.Instance.Settings.YadifPath, Environment.NewLine,
                    bobber ? "mode=1, " : "", Order(order))));
        }

        public static void AddLeakDeint(FieldOrder order, List<DeinterlaceFilter> filters)
        {
            filters.Add(new DeinterlaceFilter(
                "LeakKernelDeint",
                string.Format("LoadPlugin(\"{0}\"){1}LeakKernelDeint(order={2},sharp=true)", Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "LeakKernelDeint.dll"), Environment.NewLine, Order(order))));
        }

        public static void AddTDeint(FieldOrder order, List<DeinterlaceFilter> filters, bool processAll, bool eedi2, bool bob)
        {
            StringBuilder script = new StringBuilder();
            script.Append("LoadPlugin(\"" + Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "TDeint.dll") + "\")\r\n");
            if (eedi2)
            {
                script.Append("LoadPlugin(\"" + Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "EEDI2.dll") + "\")\r\n");
                script.Append("edeintted = last.");
                if (order == FieldOrder.TFF)
                    script.Append("AssumeTFF().");
                else if (order == FieldOrder.BFF)
                    script.Append("AssumeBFF().");
                script.Append("SeparateFields().SelectEven().EEDI2(field=-1)\r\n");
            }
            script.Append("TDeint(");

            if (bob)
                script.Append("mode=1,");
            if (order != FieldOrder.VARIABLE)
                script.Append("order=" + Order(order) + ",");
            if (!processAll) // For hybrid clips
                script.Append("full=false,");
            if (eedi2)
                script.Append("edeint=edeintted,");

            script = new StringBuilder(script.ToString().TrimEnd(new char[] { ',' }));
            script.Append(")");
            filters.Add(new DeinterlaceFilter(
                bob ? (eedi2 ? "TDeint (with EDI + Bob)" : "TDeint (with Bob)") : (eedi2 ? "TDeint (with EDI)" : "TDeint"),
                script.ToString()));
        }

        public static void AddFieldDeint(FieldOrder order, List<DeinterlaceFilter> filters, bool processAll, bool blend)
        {
            string name = "FieldDeinterlace";
            if (!blend)
                name = "FieldDeinterlace (no blend)";

            StringBuilder script = new StringBuilder();
            script.Append("LoadPlugin(\"" + Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "Decomb.dll") + "\")\r\n");
            if (order == FieldOrder.TFF)
                script.Append("AssumeTFF().");
            else if (order == FieldOrder.BFF)
                script.Append("AssumeBFF().");

            script.Append("FieldDeinterlace(");
            
            if (!blend)
                script.Append("blend=false");

            if (!processAll)
            {
                if (!blend)
                    script.Append(",");
                script.Append("full=false");
            }
            script.Append(")");
            filters.Add(new DeinterlaceFilter(
                name,
                script.ToString()));
        }

        public static void AddTMC(FieldOrder order, List<DeinterlaceFilter> filters)
        {
            filters.Add(new DeinterlaceFilter(
                "TomsMoComp",
                string.Format("LoadPlugin(\"{0}\"){1}TomsMoComp({2},5,1)", Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "TomsMoComp.dll"), Environment.NewLine, Order(order))));
        }

        public static void Portionize(List<DeinterlaceFilter> filters, string trimLine)
        {
            for (int i = 0; i < filters.Count; i++)
            {
                string script = filters[i].Script;
                StringBuilder newScript = new StringBuilder();
                newScript.AppendLine("original = last");
                newScript.Append("deintted = original.");
                newScript.AppendLine(script);
                newScript.Append(trimLine);
                filters[i].Script = newScript.ToString();
            }
        }

        
        
        public static void AddTIVTC(string d2vFile, bool anime, bool hybrid, bool mostlyFilm, bool advancedDeinterlacing,
            FieldOrder fieldOrder, List<DeinterlaceFilter> filters)
        {
            StringBuilder script = new StringBuilder();
            script.Append("LoadPlugin(\"" + Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "TIVTC.dll") + "\")\r\n");
            if (advancedDeinterlacing)
            {
                script.Append("LoadPlugin(\"" + Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "EEDI2.dll") + "\")\r\n");
                script.Append("LoadPlugin(\"" + Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "TDeint.dll") + "\")\r\n");
                script.Append("edeintted = ");
                if (fieldOrder == FieldOrder.TFF)
                    script.Append("AssumeTFF().");
                else if (fieldOrder == FieldOrder.BFF)
                    script.Append("AssumeBFF().");
                script.AppendFormat("SeparateFields().SelectEven().EEDI2(field=-1)\r\n");
                script.Append("tdeintted = TDeint(edeint=edeintted");
                if (fieldOrder != FieldOrder.VARIABLE)
                    script.Append(",order=" + Order(fieldOrder));
                script.Append(")\r\n");
            }

            script.Append("tfm(");
            if (d2vFile.Length <= 0)
                script.AppendFormat("order={0}", Order(fieldOrder));
            if (advancedDeinterlacing)
            {
                if (d2vFile.Length <= 0)
                    script.Append(",");
                script.Append("clip2=tdeintted");
            }
            script.Append(")");

            script.Append(".tdecimate(");
            if (anime)
                script.Append("mode=1");
            if (hybrid)
            {
                if (anime)
                    script.Append(",");
                if (mostlyFilm)
                    script.Append("hybrid=1");
                else
                    script.Append("hybrid=3");
                
            }
            script.Append(")");
            filters.Add(new DeinterlaceFilter(
                advancedDeinterlacing ? "TIVTC + TDeint(EDI) -- slow" : "TIVTC",
                script.ToString()));
        }

        public static void AddIVTC(FieldOrder order, bool hybrid, bool mostlyFilm,
            List<DeinterlaceFilter> filters)
        {
            StringBuilder script = new StringBuilder();
            script.Append("LoadPlugin(\"" + Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "Decomb.dll") + "\")\r\n");
            if (order == FieldOrder.TFF)
                script.Append("AssumeTFF().");
            else if (order == FieldOrder.BFF)
                script.Append("AssumeBFF().");

            script.Append("Telecide(guide=1).Decimate(");

            if (hybrid)
            {
                if (mostlyFilm)
                    script.Append("mode=3,");
                else
                    script.Append("mode=1,");

                script.Append("threshold=2.0");
            }

            script.Append(")");
            filters.Add(new DeinterlaceFilter(
                "Decomb IVTC",
                script.ToString()));
        }


        
        
        public static void AddTDecimate(int decimateM, List<DeinterlaceFilter> filters)
        {
            filters.Add(new DeinterlaceFilter(
                "Tritical Decimate",
                string.Format("LoadPlugin(\"{0}\"){1}TDecimate(cycleR={2})", Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "TIVTC.dll"), Environment.NewLine, decimateM)));
        }
        
        
        private const string DetectionScript =
@"{0} #original script
{1} #trimming
{5} #LoadPlugin
global unused_ = blankclip(pixel_type=""yv12"", length=10).TFM()
file=""{2}""
global sep=""-""
function IsMoving() {{
  global b = (diff < 1.0) ? false : true}}
ConvertToYV12()
c = last
global clip = last
c = WriteFile(c, file, ""a"", ""sep"", ""b"")
c = FrameEvaluate(c, ""global a = IsCombedTIVTC(clip, cthresh=9)"")
c = FrameEvaluate(c, ""IsMoving"")
c = FrameEvaluate(c,""global diff = 0.50*YDifferenceFromPrevious(clip) + 0.25*UDifferenceFromPrevious(clip) + 0.25*VDifferenceFromPrevious(clip)"")
crop(c,0,0,16,16)
SelectRangeEvery({3},{4},0)";

        private const string FieldOrderScript =
@"{0} # original script
{1} #trimming
file=""{2}""
global sep=""-""
ConvertToYV12()
d = last
global abff = d.assumebff().separatefields()
global atff = d.assumetff().separatefields()
c = d.loop(2)
c = WriteFile(c, file, ""diffa"", ""sep"", ""diffb"")
c = FrameEvaluate(c,""global diffa = 0.50*YDifferenceFromPrevious(abff) + 0.25*UDifferenceFromPrevious(abff) + 0.25*VDifferenceFromPrevious(abff)"")
c = FrameEvaluate(c,""global diffb = 0.50*YDifferenceFromPrevious(atff) + 0.25*UDifferenceFromPrevious(atff) + 0.25*VDifferenceFromPrevious(atff)"")
crop(c,0,0,16,16)
SelectRangeEvery({3},{4},0)
";

        public static string getScript(int scriptType, string originalScript, string trimLine, string logFileName, int selectEvery, int selectLength)
        {
            if (scriptType == 0) // detection
                return string.Format(DetectionScript, originalScript, trimLine, logFileName, selectEvery, selectLength, "LoadPlugin(\"" + Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "TIVTC.dll") + "\")");
            else if (scriptType == 1) // field order
                return string.Format(FieldOrderScript, originalScript, trimLine, logFileName, selectEvery, selectLength);
            else
                return null;
        }
        

        public static void undercrop(ref CropValues crop)
        {
            if (crop.left % 2 != 0 && crop.top % 2 != 0 && crop.bottom % 2 != 0 && crop.right % 2 != 0)
                throw new Exception("Cropping by odd numbers not supported in undercropping to mod16");

            while ((crop.left + crop.right) % 16 > 0)
            {
                if (crop.left > crop.right)
                {
                    if (crop.left > 1)
                        crop.left -= 2;
                    else
                        crop.left = 0;
                }
                else
                {
                    if (crop.right > 1)
                        crop.right -= 2;
                    else
                        crop.right = 0;
                }
            }
            while ((crop.top + crop.bottom) % 16 > 0)
            {
                if (crop.top > crop.bottom)
                {
                    if (crop.top > 1)
                        crop.top -= 2;
                    else
                        crop.top = 0;
                }
                else
                {
                    if (crop.bottom > 1)
                        crop.bottom -= 2;
                    else
                        crop.bottom = 0;
                }
            }
        }

        public static void overcrop(ref CropValues crop)
        {
            if (crop.left % 2 != 0 && crop.top % 2 != 0 && crop.bottom % 2 != 0 && crop.right % 2 != 0)
                throw new Exception("Cropping by odd numbers not supported in overcropping to mod16");

            bool doLeftNext = true;
            while ((crop.left + crop.right) % 16 != 0)
            {
                if (doLeftNext)
                    crop.left += 2;
                else
                    crop.right += 2;
                doLeftNext = !doLeftNext;
            }

            bool doTopNext = true;
            while ((crop.top + crop.bottom) % 16 != 0)
            {
                if (doTopNext)
                    crop.top += 2;
                else
                    crop.bottom += 2;
                doTopNext = !doTopNext;
            }
        }

        public static void cropMod4Horizontal(ref CropValues crop)
        {
            if (crop.left % 2 != 0 && crop.top % 2 != 0 && crop.bottom % 2 != 0 && crop.right % 2 != 0)
                throw new Exception("Cropping by odd numbers not supported in mod4 horizontal cropping");
            while ((crop.left + crop.right) % 4 > 0)
            {
                if (crop.left > crop.right)
                {
                    if (crop.left > 1)
                    {
                        crop.left -= 2;
                    }
                    else
                    {
                        crop.left = 0;
                    }
                }
                else
                {
                    if (crop.right > 1)
                    {
                        crop.right -= 2;
                    }
                    else
                    {
                        crop.right = 0;
                    }
                }
            }
        }

    }
}
