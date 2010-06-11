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
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Globalization;
using System.Text;
using System.Threading;
using System.Windows.Forms;

namespace MeGUI
{
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public delegate void UpdateSourceDetectionStatus(int numDone, int total); // catches the UpdateGUI events fired from the encoder
    public delegate void FinishedAnalysis(SourceInfo info, bool error, string errorMessage);

    public class SourceInfo
    {
        public FieldOrder fieldOrder;
        public SourceType sourceType;
        public int decimateM;
        public bool majorityFilm;
        public bool isAnime;
    }

    public enum SourceType
    {
        UNKNOWN, NOT_ENOUGH_SECTIONS,
        PROGRESSIVE, INTERLACED, FILM, DECIMATING,
        HYBRID_FILM_INTERLACED, HYBRID_PROGRESSIVE_INTERLACED, HYBRID_PROGRESSIVE_FILM
    };

    public enum FieldOrder
    {
        UNKNOWN, TFF, BFF, VARIABLE, 
    };

    public class SourceDetector
    {
        public SourceDetector(string avsScript, string d2vFile, 
            bool isAnime, SourceDetectorSettings settings,
            UpdateSourceDetectionStatus updateMethod,
            FinishedAnalysis finishedMethod)
        {

            this.script = avsScript;
            this.d2vFileName = d2vFile;
            this.settings = settings;
            this.isAnime = isAnime;
            analyseUpdate += updateMethod;
            finishedAnalysis += finishedMethod;
            
            //
            trimmedFilteredLine = "";
            type = SourceType.UNKNOWN;
            majorityFilm = false;
            usingPortions = false;
            error = false;
            continueWorking = true;
        }

        #region local info
        bool isAnime;
        bool error = false, continueWorking = true;
        string errorMessage = "";
        SourceDetectorSettings settings;
        private event UpdateSourceDetectionStatus analyseUpdate;
        private event FinishedAnalysis finishedAnalysis;
        private string script, d2vFileName, trimmedFilteredLine;
        private SourceType type;
        private int decimateM;
        private int tffCount = 0, bffCount = 0;
        private FieldOrder fieldOrder = FieldOrder.UNKNOWN;
        public bool majorityFilm, usingPortions;

        private string analysis;
        private List<DeinterlaceFilter> filters = new List<DeinterlaceFilter>();
        #endregion
        #region Processing
        #region helper methods
        private string findPortions(List<int[]> portions, int selectEvery, int selectLength, int numPortions,
            int sectionCount, int inputFrames, string type, out string trimLine, out int frameCount)
        {
            frameCount = 0;
            trimLine = "";
            trimmedFilteredLine = "";
            string outputText = string.Format("There are {0} {1} portions.\r\n", numPortions, type);

            int lastEndFrame = -1;
            for (int i = 0; i < numPortions; i++)
            {
                int portionStart = portions[i][0];
                int portionEnd = portions[i][1];
                int startFrame = Math.Max(0, (portionStart) * selectEvery);
                if (portionEnd == 0)
                    portionEnd = sectionCount;
                int endFrame = Math.Min(inputFrames-1, (portionEnd + 1) * selectEvery);
                frameCount += endFrame - startFrame;
                trimLine += string.Format("trim({0},{1}) ++ ", startFrame, endFrame);
                outputText += string.Format("Portion number {0} goes from frame {1} to frame {2}.\r\n", i + 1, startFrame, endFrame);
                trimmedFilteredLine += string.Format("original.trim({0},{1}) ++ deintted.trim({2},{3}) ++ ",
                    lastEndFrame + 1, startFrame - 1, startFrame, endFrame);
                lastEndFrame = endFrame;
            }
            if (lastEndFrame < inputFrames - 1)
                trimmedFilteredLine += string.Format("original.trim({0},{1})", lastEndFrame + 1, inputFrames);
            trimLine = trimLine.TrimEnd(new char[] { ' ', '+' });
            return outputText;
        }

        private string getLogFileName(string logFileName)
        {
            return Path.Combine(Path.GetTempPath(), logFileName);
        }

        // stax
        private void Process(string scriptBlock)
        {
            try
            {
                using (AvsFile af = AvsFile.ParseScript(scriptBlock))
                {
                    int i = 0;
                    int frameCount = (int)af.Info.FrameCount;
                    bool running = true;
                    new Thread(new ThreadStart(delegate
                    {
                        if (analyseUpdate != null)
                        {
                            while (running)
                            {
                                analyseUpdate(i, frameCount);
                                Thread.Sleep(500);
                            }
                        }
                    })).Start();


                    IntPtr zero = new IntPtr(0);
                    for (i = 0; i < frameCount && continueWorking; i++)
                    {
                        af.Clip.ReadFrame(zero, 0, i);
                    }
                    running = false;
                }
            }
            catch (Exception ex)
            {
                error = true;
                errorMessage = "Error opening analysis script " + ex.Message + "\r\n" +
                    "Check to make sure you have TIVTC.dll in your AviSynth plugins directory.\r\n" + ex.Message;
                finishProcessing();
            }
        }
        #endregion
        #region script generation and running
        private void runScript(int scriptType, int frameCount, string trimLine)
        {
            int numFrames = 0;
            try
            {
                using (AvsFile af = AvsFile.ParseScript(script))
                {
                    numFrames = (int)af.Info.FrameCount;
                }
            }
            catch (Exception e)
            {
                error = true;
                errorMessage = "The input clip for source detection could not be opened.\r\n"
                    + e.Message;
                finishProcessing();
                return;
            }

            if (frameCount > 0)
                numFrames = frameCount;

            const int selectLength = 5; // This used to be variable, but I found no need to. It's useful to keep this name, though
            int selectEvery = (int) ((100.0 * (double)selectLength) / ((double)settings.AnalysePercent));

            int minAnalyseSections = settings.MinimumAnalyseSections;
            if (scriptType == 1) // Field order script. For this, we separatefields, so we have twice as many frames anyway
                // It saves time, and costs nothing to halve the minimum sections to analyse for this example
                minAnalyseSections = minAnalyseSections / 2 + 1; // We add one to prevent getting 0;

            // Check if we need to modify the SelectRangeEvery parameters:
            if (((double)selectLength * (double)numFrames / (double)selectEvery) < (int)minAnalyseSections * 5) 
            {
                if (numFrames >= minAnalyseSections * 5) // If there are actually enough frames
                {
                    selectEvery = (int)((((double)numFrames) / ((double)minAnalyseSections * 5.0)) * (double)selectLength);
                }
                else
                    // if there aren't enough frames, analyse everything -- that's got to be good enough
                    selectEvery = selectLength;
            }

            string logFileName = getLogFileName((scriptType == 1) ? "ff_interlace-" + Guid.NewGuid().ToString("N") + ".log" : "interlace-" + Guid.NewGuid().ToString("N") + ".log");



            if (File.Exists(logFileName))
                File.Delete(logFileName);

            string resultScript = ScriptServer.getScript(scriptType, script, trimLine, logFileName,
                selectEvery, selectLength);

            // stax
            MethodInvoker mi = delegate {
                try
                {
                    Process(resultScript); // stax
                    if (error)
                        return;
                    if (!continueWorking)
                        return;
                    if (scriptType == 0) // detection
                        analyse(logFileName, selectEvery, selectLength, numFrames);
                    else if (scriptType == 1) // field order
                        analyseFF(logFileName);
                }
                finally
                {
                    try
                    {
                        File.Delete(logFileName);
                    }
                    catch(Exception)
                    {
                    }
                }
            };

            Thread t = new Thread(new ThreadStart(mi));
            t.Priority = settings.Priority;
            t.Start();
        }

        #endregion
        #region analysis
        private bool checkDecimate(int[] data)
        {
            int[] dataCopy = new int[6];
            Array.Copy(data, dataCopy, 6);
            Array.Sort(dataCopy);

                        int numMovingFrames = -1;

            for (int i = 0; i < data.Length; i++)
            {
                if (dataCopy[5] == data[i])
                    numMovingFrames = i;
            }

            if (dataCopy[5] > (double)dataCopy[4] * settings.DecimationThreshold &&
                numMovingFrames != 5 && numMovingFrames != 0)
                // If there are 5 moving frames, then it needs no decimation
                // If there are 0 moving frames, then we have a problem.
            {
                type = SourceType.DECIMATING;
                decimateM = 5 - numMovingFrames;
                return true;
            }
            return false;
        }
        private void analyseFF(string filename)
        {
            CultureInfo ci = new CultureInfo("en-us");
            StreamReader instream;
            try
            {
                instream = new StreamReader(filename);
            }
            catch (Exception)
            {
                instream = null;
                error = true;
                errorMessage = "Opening the field order analysis file failed.";
                finishProcessing();
                return;
            }
            int countA = 0, countB = 0, countEqual = 0;
            int localCountA = 0, localCountB = 0, sectionCountA = 0, sectionCountB = 0;
            double sumA = 0, sumB = 0;
            double valueA, valueB;
            int count = 0;

            string line = instream.ReadLine();
            while (line != null)
            {
                if (count != 0 && line.IndexOf("-1.#IND00") == -1) //Scene change or unexptected value -> ignore
                {
                    string[] contents = line.Split(new char[] { '-' });
                    try
                    {
                        valueA = Double.Parse(contents[0], ci);
                        valueB = Double.Parse(contents[1], ci);
                    }
                    catch (Exception)
                    {
                        error = true;
                        errorMessage = "Unexpected value in file " + filename + "\r\n" +
                            "This error should not have occurred. Please report it on \r\n" +
                            "post it on http://sourceforge.net/projects/megui with the file named above.";
                        errorMessage += "\r\nMore debugging info:\r\n" +
                            "Line contents: " + line + "\r\n";
                        finishProcessing();
                        return;
                    }
                    if (valueA > valueB)
                    {
                        countA++;
                        localCountA++;
                    }
                    else if (valueB > valueA)
                    {
                        countB++;
                        localCountB++;
                    }
                    else
                        countEqual++;
                    sumA += valueA;
                    sumB += valueB;
                }
                count++;
                if (count == 10)
                {
                    count = 0;
                    // Truly interlaced sections should always make one of the counts be 5 and the other 0.
                    // Progressive sections will be randomly distributed between localCountA and localCountB,
                    // so this algorithm successfully ignores those sections.
                    // Film sections will always have two frames which show the actual field order, and the other
                    // frames will show an arbitrary field order. This algorithm (luckily) seems to work very well
                    // with film sections as well. Using this thresholding as opposed to just comparing countB to countA
                    // produces _much_ more heavily-sided results.
                    if (localCountA > localCountB && localCountB == 0)
                        sectionCountA++;
                    if (localCountB > localCountA && localCountA == 0)
                        sectionCountB++;
                    localCountA = 0;
                    localCountB = 0;
                }
                line = instream.ReadLine();
            }
            instream.Close();
            if ((((double)sectionCountA +(double)sectionCountB) / 100.0 * (double)settings.HybridFOPercent) > sectionCountB)
            {
                analysis += "Source is declared tff by a margin of " + sectionCountA + " to " + sectionCountB + ".";
                fieldOrder = FieldOrder.TFF;
            }
            else if ((((double)sectionCountA +(double)sectionCountB) / 100.0 * (double)settings.HybridFOPercent) > sectionCountA)
            {
                analysis += "Source is declared bff by a margin of " + sectionCountB + " to " + sectionCountA + ".";
                fieldOrder = FieldOrder.BFF;
            }
            else
            {
                analysis += "Source is hybrid bff and tff at " + sectionCountB + " bff and " + sectionCountA + " tff.";
                fieldOrder = FieldOrder.VARIABLE;
            }

            tffCount = countA;
            bffCount = countB;
            finishProcessing();
        }
        private void analyse(string logFileName, int selectEvery, int selectLength, int inputFrames)
        {
            #region variable declaration
            bool stillWorking = false;
            StreamReader instream;
            try
            {
                instream = new StreamReader(logFileName);
            }
            catch (IOException)
            {
                error = true;
                errorMessage = "Can't open analysis log file, \"" + logFileName + "\"\r\n" +
                    "Make sure that TIVTC.dll is in your AviSynth plugins dir.";
                finishProcessing();
                return;
            }
            bool[,] data = new bool[5, 2];
            int count = 0;
            int numTC = 0, numProg = 0, numInt = 0, numUseless = 0;
            int sectionCount = 0;

            // Decimation data
            int totalCombed = 0;
            int[] sectionsWithMovingFrames = new int[6];
            
            int maxPortions = settings.MaxPortions;
            // interlaced portions
            List<int[]>[] portions = new List<int[]>[2];
            portions[0] = new List<int[]>();
            portions[1] = new List<int[]>();

            int[] portionLength = new int[2];
            int[] nextPortionIndex = new int[2];
            bool[] inPortion = new bool[2];
            int[] numPortions = new int[2];
            int[] portionStatus = new int[2];


            #endregion
            #region loop
            string line = instream.ReadLine();
            while (line != null)
            {
                if (line.Length > 11)
                {
                    error = true;
                    errorMessage = "Unexpected value in file " + logFileName + "\r\n" +
                        "Make sure you have TIVTC.dll in your AviSynth plugins directory.";
                }
                string[] contents = line.Split(new char[] { '-' });
                data[count, 0] = (contents[0].Equals("true"));
                data[count, 1] = (contents[1].Equals("true"));
                count++;

                #region 5-ly analysis
                if (count == 5)
                {
                    sectionCount++;
                    int numComb = 0;
                    int numMoving = 0;
                    int combA = -1, combB = -1;
                    for (int i = 0; i < 5; i++)
                    {
                        if (data[i, 0])
                        {
                            numComb++;
                            if (combA == -1)
                                combA = i;
                            else
                                combB = i;
                        }
                        if (data[i, 1])
                            numMoving++;
                    }
                    totalCombed += numComb;
                    sectionsWithMovingFrames[numMoving]++;
                    if (numMoving < 5)
                    {
                        numUseless++;
                        portionStatus[0] = 1;
                        portionStatus[1] = 1;
                    }
                    else if (numComb == 2 && ((combB - combA == 1) || (combB - combA == 4)))
                    {
                        numTC++;
                        portionStatus[0] = 0;
                        portionStatus[1] = 2;
                    }
                    else if (numComb > 0)
                    {
                        numInt++;
                        portionStatus[0] = 2;
                        portionStatus[1] = 0;
                    }
                    else
                    {
                        numProg++;
                        portionStatus[0] = 0;
                        portionStatus[1] = 0;
                    }
                    #region portions
                    // Manage film and interlaced portions
                    for (int i = 0; i < 2; i++)
                    {
                        if (portionStatus[i] == 0) // Stop any portions we are in.
                        {
                            if (inPortion[i])
                            {
                                ((int[])portions[i][nextPortionIndex[i]])[1] = sectionCount;
                                #region useless comments
                                /*                                if (portionLength[i] == 1) // This should help reduce random fluctuations, by removing length 1 portions
 * I've now changed my mind about random fluctuations. I believe they are good, because they occur when TIVTC is on the verge of making
 * a wrong decision. Instead of continuing with this decision, which would then regard this section of the film as progressive, leaving combing
 * this now has the effect of dramatically increasing the number of portions, forcing the whole thing to be deinterlaced, which is better,
 * as it leaves no residual combing.
 * 
 * Edit again: i've left this section commented out, but the other section which removes length 1 progressive sections, I've left in, as it is
 * safer to deinterlace progressive stuff than vice versa.
                                {
                                    portions[i].RemoveAt(nextPortionIndex[i]);
                                    nextPortionIndex[i]--;
                                    numPortions[i]--;
                                }
*/
                                #endregion
                                nextPortionIndex[i]++;
                                inPortion[i] = false;
                            }
                            portionLength[i] = 0;
                        }
                        else if (portionStatus[i] == 1) // Continue all portions, but don't start a new one.
                        {
                            portionLength[i]++;
                        }
                        else if (portionStatus[i] == 2) // Start a new portion, or continue an old one.
                        {
                            if (inPortion[i])
                                portionLength[i]++;
                            else
                            {
                                int startIndex = sectionCount - portionLength[i];
                                int lastEndIndex = -2;
                                if (nextPortionIndex[i] > 0)
                                    lastEndIndex = ((int[])portions[i][nextPortionIndex[i]-1])[1];
                                if (startIndex - lastEndIndex > 1) // If the last portion ended more than 1 section ago. This culls trivial portions
                                {
                                    portions[i].Add(new int[2]);
                                    ((int[])portions[i][nextPortionIndex[i]])[0] = startIndex;
                                    portionLength[i]++;
                                    numPortions[i]++;
                                }
                                else
                                {
                                    nextPortionIndex[i]--;
                                }
                                inPortion[i] = true;
                            }
                        }
                    }
                    #endregion
                    count = 0;
                }
                #endregion
                line = instream.ReadLine();
            }
            #endregion
            #region final counting
            instream.Close();

            int[] array = new int[] { numInt, numProg, numTC };
            Array.Sort(array);

            analysis = string.Format("For your reference:\r\nProgressive sections: {0}\r\nInterlaced sections: {1}\r\nPartially Static Sections: {2}\r\nFilm Sections: {3}\r\n", numProg, numInt, numUseless, numTC);

            if (numInt + numProg + numTC < settings.MinimumUsefulSections)
            {
                if (checkDecimate(sectionsWithMovingFrames))
                {
                    analysis += "Source is declared as repetition-upconverted. Decimation is required\r\n";
                    finishProcessing();
                    return;
                }
                else
                {
                    analysis += "Source does not have enough data. This either comes from an internal error or an unexpected source type.\r\n";
                    type = SourceType.NOT_ENOUGH_SECTIONS;
                    finishProcessing();
                    return;
                }
            }

            #region plain
            if (array[1] < (double)(array[0]+array[1]+array[2]) /100.0 * settings.HybridPercent)
            {
                if (array[2] == numProg)
                {
                    analysis += "Source is declared progressive.\r\n";
                    type = SourceType.PROGRESSIVE;
                    checkDecimate(sectionsWithMovingFrames);
                }
                else if (array[2] == numInt)
                {
                    analysis += "Source is declared interlaced.\r\n";
                    type = SourceType.INTERLACED;
                    stillWorking = true;
                    runScript(1, -1, "#no trimming"); //field order script
                }
                else
                {
                    analysis += "Source is declared telecined.\r\n";
                    type = SourceType.FILM;
                    stillWorking = true;
                    runScript(1, -1, "#no trimming"); //field order script
                }
            }
            #endregion
            #region hybrid
            else
            {
                if (array[0] == numProg) // We have a hybrid film/ntsc. This is the most common
                {
                    analysis += "Source is declared hybrid film/ntsc. Majority is ";
                    if (array[2] == numTC)
                    {
                        analysis += "film.\r\n";
                        majorityFilm = true;
                    }
                    else
                    {
                        analysis += "ntsc (interlaced).\r\n";
                        majorityFilm = false;
                    }
                    type = SourceType.HYBRID_FILM_INTERLACED;
                    stillWorking = true;
                    runScript(1, -1, "#no trimming");

                }
                else if (array[0] == numInt)
                {
                    if (array[0] > (double)(array[0]+array[1]+array[2]) / 100.0 * settings.HybridPercent) // There is also a section of interlaced
                    {
                        analysis += "Source is declared hybrid film/ntsc. Majority is film.\r\n";
                        type = SourceType.HYBRID_FILM_INTERLACED;
                        majorityFilm = true;
                        stillWorking = true;
                        runScript(1, -1, "#no trimming");
                    }
                    else
                    {
                        analysis += "Source is declared hybrid film/progressive.\r\n";
                        majorityFilm = (array[2] == numTC);
                        type = SourceType.HYBRID_PROGRESSIVE_FILM;

                        // Although we don't actually end up using portions for this situation, 
                        // it is good to only analyse the sections which are actually film.
                        int frameCount = -1;
                        string trimLine = "#no trimming";
                        string textLines = "The number of portions is " + numPortions[1] + ".\r\n";
                        if (numPortions[1] <= maxPortions)
                        {
                            textLines = findPortions(portions[1], selectEvery, selectLength, 
                                numPortions[1], sectionCount, inputFrames, "telecined", out trimLine, out frameCount);
                        }
                        stillWorking = true;
                        runScript(1, frameCount, trimLine);
                    }
                }
                else if (array[0] == numTC)
                {
                    if (array[0] > (double)(array[0] + array[1] + array[2]) / 100.0 * settings.HybridPercent) // There is also a section of film
                    {
                        analysis += "Source is declared hybrid film/interlaced. Majority is interlaced.\r\n";
                        type = SourceType.HYBRID_FILM_INTERLACED;
                        majorityFilm = false;

                        stillWorking = true;
                        runScript(1, -1, "#no trimming");
                    }
                    else
                    {
                        analysis += "Source is declared hybrid progressive/interlaced. ";
                        
                        type = SourceType.HYBRID_PROGRESSIVE_INTERLACED;

                        usingPortions = false;
                        int frameCount = -1;
                        string trimLine = "#no trimming";
                        string textLines = "The number of portions is " + numPortions[0] + ".\r\n";

                        if (settings.PortionsAllowed &&
                            numPortions[0] <= maxPortions &&
                            array[2] < ((double)array[1] * settings.PortionThreshold))
                        {
                            textLines = findPortions(portions[0], selectEvery, selectLength,
                                numPortions[0], sectionCount, inputFrames, "interlaced", out trimLine, out frameCount);
                            usingPortions = true;
                            analysis += textLines;
                        }
                        else
                        {
                            analysis += "This should be deinterlaced by a deinterlacer that tries to weave it before deinterlacing.\r\n";
                        }
                        stillWorking = true;
                        runScript(1, frameCount, trimLine); //field order script
                    }
                }
            }
            #endregion
            #endregion
            if (!stillWorking)
                finishProcessing();
        }
        #endregion
        #region finalizing
        private void finishProcessing()
        {
            if (error)
            {
                finishedAnalysis(null, true, errorMessage);
                return;
            }

            if (!continueWorking)
            {
                return;
            }

            if (fieldOrder == FieldOrder.VARIABLE &&
                d2vFileName.Length == 0) // We are stuck for field order information, lets just go for what we have most of
                fieldOrder = (bffCount > tffCount) ? FieldOrder.BFF : FieldOrder.TFF;

            SourceInfo info = new SourceInfo();
            info.sourceType = type;
            info.decimateM = decimateM;
            info.fieldOrder = fieldOrder;
            info.majorityFilm = majorityFilm;
           

            finishedAnalysis(info, false, null);

        }
        #endregion
        #endregion
        #region Program interface
        public void analyse()
        {
            runScript(0, -1, "#no trimming");
        }
        public void stop()
        {
            continueWorking = false;
        }
        #endregion
    }
}
