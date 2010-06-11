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
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using System.Xml.Serialization;

using MeGUI.core.details;
using MeGUI.core.util;

namespace MeGUI
{
    /// <summary>
    /// JobUtil is used to perform various job related tasks like loading/saving jobs, 
	/// generating all types of jobs, update bitrates in jobs, and get the properties
	/// of a video input file
	/// </summary>
	public class JobUtil
    {
       
        #region start/stop
		MainForm mainForm;
        AVCLevels al;
        string mbtreeFile = ".stats.mbtree";
		public JobUtil(MainForm mainForm)
		{
			this.mainForm = mainForm;
            al = new AVCLevels();
        }
        #endregion
		#region job generation
		#region single job generation
        
        public VideoJob generateVideoJob(string input, string output, VideoCodecSettings settings, Dar? dar, Zone[] zones)
        {
            return generateVideoJob(input, output, settings, false, dar, zones);
        }
        
        /// <summary>
		/// generates a videojob from the given settings
		/// returns the job and whether or not this is an automated job (in which case another job
		/// will have to be created)
		/// </summary>
		/// <param name="input">the video input (avisynth script)</param>
		/// <param name="output">the video output</param>
		/// <param name="settings">the codec settings for this job</param>
		/// <returns>the generated job or null if there was an error with the video source</returns>
		public VideoJob generateVideoJob(string input, string output, VideoCodecSettings settings, bool skipVideoCheck, Dar? dar, Zone[] zones)
		{
			VideoJob job = new VideoJob(input, output, settings, dar, zones);
			
			if (Path.GetDirectoryName(settings.Logfile).Equals("")) // no path set
				settings.Logfile = Path.ChangeExtension(output, ".stats");
            if (job.Settings.SettingsID.Equals("x264"))
                mbtreeFile = Path.ChangeExtension(output, ".stats.mbtree");
			if (job.Settings.EncodingMode == 4) // automated 2 pass, change type to 2 pass 2nd pass
			{
				job.Settings.EncodingMode = 3;
			}
			else if (job.Settings.EncodingMode == 8) // automated 3 pass, change type to 3 pass first pass
			{
				if (mainForm.Settings.OverwriteStats)
					job.Settings.EncodingMode = 7;
				else
					job.Settings.EncodingMode = 3; // 2 pass 2nd pass.. doesn't overwrite the stats file
			}

            if (!skipVideoCheck)
                checkVideo(job.Input);

            return job;
		}

        private void checkVideo(string p)
        {
            ulong a;
            double b;
            getInputProperties(out a, out b, p);
        }
        /// <summary>
        /// sets the number of encoder threads in function of the number of processors found on the system
        /// </summary>
        /// <param name="settings"></param>
        private void adjustNbThreads(VideoCodecSettings settings)
        {
            string nbProc = System.Environment.GetEnvironmentVariable("NUMBER_OF_PROCESSORS");
            if (!String.IsNullOrEmpty(nbProc))
            {
                try
                {
                    int nbCPUs = int.Parse(nbProc);
                    settings.setAdjustedNbThreads(nbCPUs);
                }
                catch (Exception) { }
            }
        }

        public JobChain GenerateMuxJobs(VideoStream video, decimal? framerate, MuxStream[] audioStreamsArray, MuxableType[] audioTypes,
            MuxStream[] subtitleStreamsArray, MuxableType[] subTypes,
            string chapterFile, MuxableType chapterInputType, ContainerType container, string output, FileSize? splitSize, List<string> inputsToDelete, string deviceType, MuxableType deviceOutputType)
        {
            Debug.Assert(splitSize == null || splitSize.Value != FileSize.Empty);

            MuxProvider prov = mainForm.MuxProvider;
            List<MuxableType> allTypes = new List<MuxableType>();
            allTypes.Add(video.VideoType);
            allTypes.AddRange(audioTypes);
            allTypes.AddRange(subTypes);
            if (chapterInputType != null)
                allTypes.Add(chapterInputType);
            if (deviceOutputType != null)
                allTypes.Add(deviceOutputType);
            MuxPath muxPath = prov.GetMuxPath(container, splitSize.HasValue, allTypes.ToArray());
            List<MuxJob> jobs = new List<MuxJob>();
            List<MuxStream> subtitleStreams = new List<MuxStream>(subtitleStreamsArray);
            List<MuxStream> audioStreams = new List<MuxStream>(audioStreamsArray);
            int index = 0;
            int tempNumber = 1;
            string previousOutput = null;
            foreach (MuxPathLeg mpl in muxPath)
            {
                List<string> filesToDeleteThisJob = new List<string>();

                MuxJob mjob = new MuxJob();


                if (previousOutput != null)
                {
                    mjob.Settings.MuxedInput = previousOutput;
                    filesToDeleteThisJob.Add(previousOutput);
                }

                mjob.NbOfFrames = video.NumberOfFrames;
                mjob.NbOfBFrames = video.Settings.NbBframes;
                mjob.Codec = video.Settings.Codec.ToString();
                string fpsFormated = String.Format("{0:##.###}", framerate); // this formating is required for mkvmerge at least to avoid fps rounding error
                mjob.Settings.Framerate = Convert.ToDecimal(fpsFormated); 
                mjob.Settings.VideoName = video.Settings.VideoName;

                string tempOutputName = Path.Combine(Path.GetDirectoryName(output),
                    Path.GetFileNameWithoutExtension(output) + tempNumber + ".");
                tempNumber++;
                foreach (MuxableType o in mpl.handledInputTypes)
                {
                    if (o.outputType is VideoType)
                    {
                        mjob.Settings.VideoInput = video.Output;
                        if (inputsToDelete.Contains(video.Output))
                            filesToDeleteThisJob.Add(video.Output);
                        mjob.Settings.DAR = video.DAR;
                    }
                    else if (o.outputType is AudioType)
                    {
                        MuxStream stream = audioStreams.Find(delegate(MuxStream m)
                        {
                            return (VideoUtil.guessAudioType(m.path) == o.outputType);
                        });

                        if (stream != null)
                        {
                            mjob.Settings.AudioStreams.Add(stream);
                            audioStreams.Remove(stream);

                            if (inputsToDelete.Contains(stream.path))
                                filesToDeleteThisJob.Add(stream.path);
                        }
                    }
                    else if (o.outputType is SubtitleType)
                    {
                        MuxStream stream = subtitleStreams.Find(delegate(MuxStream m)
                        {
                            return (VideoUtil.guessSubtitleType(m.path) == o.outputType);
                        });

                        if (stream != null)
                        {
                            mjob.Settings.SubtitleStreams.Add(stream);
                            subtitleStreams.Remove(stream);

                            if (inputsToDelete.Contains(stream.path))
                                filesToDeleteThisJob.Add(stream.path);
                        }
                    }
                    else if (o.outputType is ChapterType)
                    {
                        if ((VideoUtil.guessChapterType(chapterFile) == o.outputType))
                            mjob.Settings.ChapterFile = chapterFile;
                        if (inputsToDelete.Contains(chapterFile))
                            filesToDeleteThisJob.Add(chapterFile);
                    }
                    else if (o.outputType is DeviceType)
                    {
                        if ((VideoUtil.guessDeviceType(deviceType) == o.outputType))
                            mjob.Settings.DeviceType = deviceType;
                    }
                }
                foreach (MuxStream s in mjob.Settings.AudioStreams)
                {
                    audioStreams.Remove(s);
                }
                foreach (MuxStream s in mjob.Settings.SubtitleStreams)
                {
                    subtitleStreams.Remove(s);
                }
                mjob.FilesToDelete.AddRange(filesToDeleteThisJob);
                if (index == muxPath.Length - 1)
                {
                    mjob.Settings.MuxedOutput = output;
                    mjob.Settings.SplitSize = splitSize;
                    mjob.Settings.DAR = video.DAR;
                    mjob.ContainerType = container;
                }
                else
                {
                    ContainerType cot = mpl.muxerInterface.GetContainersInCommon(muxPath[index + 1].muxerInterface)[0];
                    mjob.Settings.MuxedOutput = tempOutputName + cot.Extension;
                    mjob.ContainerType = cot;
                }
                previousOutput = mjob.Settings.MuxedOutput;
                index++;
                jobs.Add(mjob);
                if (string.IsNullOrEmpty(mjob.Settings.VideoInput))
                    mjob.Input = mjob.Settings.MuxedInput;
                else
                    mjob.Input = mjob.Settings.VideoInput;
                mjob.Output = mjob.Settings.MuxedOutput;
                mjob.MuxType = mpl.muxerInterface.MuxerType;
            }

            return new SequentialChain(jobs.ToArray());
        }
		#endregion
		#region job preparation (aka multiple job generation)

        public bool AddVideoJobs(string movieInput, string movieOutput, VideoCodecSettings settings,
            int introEndFrame, int creditsStartFrame, Dar? dar, bool prerender, bool checkVideo, Zone[] zones)
        {
            bool cont = getFinalZoneConfiguration(settings, introEndFrame, creditsStartFrame, ref zones);
            if (!cont) // abort
                return false;
            JobChain jobs = prepareVideoJob(movieInput, movieOutput, settings, dar, prerender, checkVideo, zones);
            if (jobs == null)
                return false;
            mainForm.Jobs.addJobsWithDependencies(jobs);

            return false;
        }
		/// <summary>
		/// at first, the job from the currently configured settings is generated. In addition, we find out if this job is 
		/// a part of an automated series of jobs. If so, it means the first generated job was the second pass, and we have
		/// to create the first pass using the same settings
		/// then, all the generated jobs are returned
		/// </summary>
		/// <returns>an Array of VideoJobs in the order they are to be encoded</returns>
		public JobChain prepareVideoJob(string movieInput, string movieOutput, VideoCodecSettings settings, Dar? dar, bool prerender, bool checkVideo, Zone[] zones)
		{
			bool twoPasses = false, threePasses = false;
			if (settings.EncodingMode == 4) // automated twopass
				twoPasses = true;
			else if (settings.EncodingMode == 8) // automated threepass
				threePasses = true;

            VideoJob prerenderJob = null;
            string hfyuFile = null;
            string inputAVS = movieInput;
            if (prerender)
            {
                hfyuFile = Path.Combine(Path.GetDirectoryName(movieInput), "hfyu_" + 
                    Path.GetFileNameWithoutExtension(movieInput) + ".avi");
                inputAVS = Path.ChangeExtension(hfyuFile, ".avs");
                if (File.Exists(hfyuFile))
                {
                    if (MessageBox.Show("The intended temporary file, " + hfyuFile + " already exists.\r\n" +
                        "Do you wish to over-write it?", "File already exists", MessageBoxButtons.YesNo, MessageBoxIcon.Exclamation)
                        == DialogResult.No)
                        return null;
                }
                if (File.Exists(inputAVS))
                {
                    if (MessageBox.Show("The intended temporary file, " + inputAVS + " already exists.\r\n" +
                        "Do you wish to over-write it?", "File already exists", MessageBoxButtons.YesNo, MessageBoxIcon.Exclamation)
                        == DialogResult.No)
                        return null;
                }
                try
                {
                    StreamWriter hfyuWrapper = new StreamWriter(inputAVS, false, Encoding.Default);
                    hfyuWrapper.WriteLine("AviSource(\"" + hfyuFile + "\")");
                    hfyuWrapper.Close();
                }
                catch (IOException)
                {
                    return null;
                }
                prerenderJob = this.generateVideoJob(movieInput, hfyuFile, new hfyuSettings(), dar, zones);
                if (prerenderJob == null)
                    return null;
            }
            if (checkVideo)
            {
                VideoUtil vUtil = new VideoUtil(mainForm);
                string error = vUtil.checkVideo(movieInput);
                if (error != null)
                {
                    bool bContinue = mainForm.DialogManager.createJobs(error);
                    if (!bContinue)
                    {
                        MessageBox.Show("Job creation aborted due to invalid AviSynth script");
                        return null;
                    }
                }
            }
            VideoJob job = this.generateVideoJob(inputAVS, movieOutput, settings, prerender, dar, zones);
			VideoJob firstpass = null;
			VideoJob middlepass = null;
			if (job != null)
			{
				if (twoPasses || threePasses) // we just created the last pass, now create previous one(s)
				{
					job.FilesToDelete.Add(job.Settings.Logfile);
                    if (job.Settings.SettingsID.Equals("x264"))
                        job.FilesToDelete.Add(mbtreeFile);
                    firstpass = cloneJob(job);
					firstpass.Output = ""; // the first pass has no output
					firstpass.Settings.EncodingMode = 2;
                    firstpass.DAR = dar;
					if (threePasses)
					{
						firstpass.Settings.EncodingMode = 5; // change to 3 pass 3rd pass just for show
						middlepass = cloneJob(job);
						middlepass.Settings.EncodingMode = 6; // 3 pass 2nd pass
                        if (mainForm.Settings.Keep2of3passOutput) // give the 2nd pass a new name
                        {
                            middlepass.Output = Path.Combine(Path.GetDirectoryName(job.Output), Path.GetFileNameWithoutExtension(job.Output)
                                + "-2ndpass" + Path.GetExtension(job.Output));
                            job.FilesToDelete.Add(middlepass.Output);
                        }
                        middlepass.DAR = dar;
					}
				}
                if (prerender)
                {
                    job.FilesToDelete.Add(hfyuFile);
                    job.FilesToDelete.Add(inputAVS);
                }
                List<VideoJob> jobList = new List<VideoJob>();
                if (prerenderJob != null)
                    jobList.Add(prerenderJob);
                if (firstpass != null)
                    jobList.Add(firstpass);
                if (middlepass != null) // we have a middle pass
                    jobList.Add(middlepass);
                jobList.Add(job);

                return new SequentialChain(jobList.ToArray());
			}
			return null;
		}
		/// <summary>
		/// creates a copy of the most important parameters of a job
		/// </summary>
		/// <param name="oldJob">the job to be cloned</param>
		/// <returns>the cloned job</returns>
		private VideoJob cloneJob(VideoJob oldJob)
		{
			VideoJob job = new VideoJob();
			job.Input = oldJob.Input;
			job.Output = oldJob.Output;
            job.Settings = oldJob.Settings.Clone();
            return job;
		}
		#endregion
		#endregion
		#region bitrate updates
		/// <summary>
		/// updates the video bitrate of a video job with the given bitrate
		/// in addition, the commandline is regenerated to reflect the bitrate change
		/// </summary>
		/// <param name="job">the job whose video bitrate is to be updated</param>
		/// <param name="bitrate">the new desired video bitrate</param>
		public void updateVideoBitrate(VideoJob job, int bitrate)
		{
			job.Settings.BitrateQuantizer = bitrate;
		}
		#endregion
		#region input properties
		/// <summary>
		/// gets the number of frames and framerate from an avisynth script
		/// </summary>
		/// <param name="nbOfFrames">number of frames of the source</param>
		/// <param name="framerate">framerate of the source</param>
		/// <param name="video">path of the source</param>
		/// <returns>true if the input file could be opened, false if not</returns>
		public static bool getInputProperties(out ulong nbOfFrames, out double framerate, string video)
		{
            int d1, d2;
            Dar d;
            return getAllInputProperties(out nbOfFrames, out framerate, out d1, out d2, out d, video);
		}

        public static void GetAllInputProperties(out ulong nbOfFrames, out double framerate, out int hRes, 
			out int vRes, out Dar dar, string video)
		{
            nbOfFrames = 0;
            hRes = vRes = 0;
            framerate = 0.0;
            try
			{
                using (AvsFile avi = AvsFile.OpenScriptFile(video))
                {
                    checked { nbOfFrames = (ulong)avi.Info.FrameCount; }
                    framerate = avi.Info.FPS;
                    hRes = (int)avi.Info.Width;
                    vRes = (int)avi.Info.Height;
                    dar = avi.Info.DAR;
                }
			}
			catch (Exception e)
			{
                throw new JobRunException("The file " + video + " cannot be opened.\r\n"
                     + "Error message for your reference: " + e.Message, e);
            }
			
		}


		/// <summary>
		/// gets the number of frames, framerate, horizontal and vertical resolution from a video source
		/// </summary>
		/// <param name="nbOfFrames">the number of frames</param>
		/// <param name="framerate">the framerate</param>
		/// <param name="hRes">the horizontal resolution</param>
		/// <param name="vRes">the vertical resolution</param>
		/// <param name="video">the video whose properties are to be read</param>
		/// <returns>whether the source could be opened or not</returns>
		public static bool getAllInputProperties(out ulong nbOfFrames, out double framerate, out int hRes, 
			out int vRes, out Dar dar, string video)
		{
            try
            {
                GetAllInputProperties(out nbOfFrames, out framerate, out hRes, out vRes, out dar, video);
                return true;
            }
            catch (Exception e)
            {
                MessageBox.Show(e.Message,
                        "Cannot open video input", MessageBoxButtons.OK, MessageBoxIcon.Stop);
                nbOfFrames = 0;
                hRes = vRes = 0;
                framerate = 0;
                dar = Dar.ITU16x9PAL;
                return false;
            }
		}

        /// <summary>
		/// validates a source against a given AVC level taking into account the rest of the configuration
		/// </summary>
		/// <param name="source">the source to be validated</param>
		/// <param name="level">the level that this source should correspond to</param>
		/// <param name="bframeType">type of b-frames used. 0 = none, 1 = b-frames without pyramid, 
		/// 2 = b-frames with pyramid order</param>
		/// <param name="nbReferences">the number of reference frames used</param>
		/// <param name="compliantLevel">the first avc level that can be used to encode this source</param>
		/// <returns>whether or not the current level is okay, if false and compliantLevel is -1, 
		/// the source could not be read</returns>
		public bool validateAVCLevel(string source, x264Settings settings, out int compliantLevel)
		{
			int hRes, vRes;
            Dar d;
            ulong nbFrames;
			double framerate;
			compliantLevel = -1;
			if (getAllInputProperties(out nbFrames, out framerate, out hRes, out vRes, out d, source))
			{
				return this.al.validateAVCLevel(hRes, vRes, framerate, settings, out compliantLevel);
			}
			else
				return false;
		}
		/// <summary>
		/// gets the number of frames of a videostream
		/// </summary>
		/// <param name="path"></param>
		/// <returns></returns>
		public ulong getNumberOfFrames(string path)
		{
			ulong retval = 0;
			double framerate = 0.0;
			bool succ = getInputProperties(out retval, out framerate, path);
			return retval;
		}
		/// <summary>
		/// gets the framerate of a video stream
		/// </summary>
		/// <param name="path"></param>
		/// <returns></returns>
		public double getFramerate(string path)
		{
			ulong retval = 0;
			double framerate = 0.0;
			bool succ = getInputProperties(out retval, out framerate, path);
			return framerate;
		}
		#endregion
		#region zones
		/// <summary>
		/// takes a series of non overlapping zones and adds zones with weight 1.0 in between
		/// this is used for xvid which doesn't know zone end frames
		/// </summary>
		/// <param name="zones">a set of zones to be analyzed</param>
		/// <param name="nbOfFrames">number of frames the video source has</param>
		/// <returns>an array of all the zones</returns>
		public Zone[] createHelperZones(Zone[] zones, int nbOfFrames)
		{
			ArrayList newZones = new ArrayList();
			Zone z = zones[0];
			Zone newZone = new Zone();
			newZone.mode = ZONEMODE.Weight;
			newZone.modifier = (decimal)100;
			if (z.startFrame > 0) // zone doesn't start at the beginning, add zone before the first configured zone
			{
				newZone.startFrame = 0;
				newZone.endFrame = z.startFrame - 1;
				newZones.Add(newZone);
			}
			if (zones.Length == 1) // special case
			{
				newZones.Add(z);
				if (z.endFrame < nbOfFrames -1) // we hav to add an end zone
				{
					newZone.startFrame = z.endFrame + 1;
					newZone.endFrame = nbOfFrames - 1;
					newZones.Add(newZone);
				}
			}
			else if (zones.Length == 2)
			{
				newZones.Add(z);
				Zone second = zones[1];
				if (z.endFrame + 1 < second.startFrame) // new zone needs to go in between
				{
					newZone.startFrame = z.endFrame + 1;
					newZone.endFrame = second.startFrame - 1;
					newZones.Add(newZone);
				}
				newZones.Add(second);
				if (second.endFrame < nbOfFrames - 1) // add end zone
				{
					newZone.startFrame = second.endFrame + 1;
					newZone.endFrame = nbOfFrames - 1;
					newZones.Add(newZone);
				}
			}
			else
			{
				for (int i = 0; i <= zones.Length - 2; i++)
				{
					Zone first = zones[i];
					Zone second = zones[i+1];
					if (first.endFrame + 1 == second.startFrame) // zones are adjacent
					{
						newZones.Add(first);
						continue;
					}
					else // zones are not adjacent, create filler zone
					{
						newZone.startFrame = first.endFrame + 1;
						newZone.endFrame = second.startFrame - 1;
						newZones.Add(first);
						newZones.Add(newZone);
					}
				}
				z = zones[zones.Length - 1];
				newZones.Add(z);
				if (z.endFrame != nbOfFrames - 1) // we have to add another zone extending till the end of the video
				{
					newZone.startFrame = z.endFrame + 1;
					newZone.endFrame = nbOfFrames - 1;
					newZones.Add(newZone);
				}
			}
			Zone[] retval = new Zone[newZones.Count];
			int index = 0;
			foreach (object o in newZones)
			{
				z = (Zone)o;
				if (index < 64)
				{
					retval[index] = z;
					index++;
				}
				else
				{
					DialogResult dr = MessageBox.Show("XviD only supports 64 zones. Including filler zones your current\r\nconfiguration yields " + retval.Length + " zones. Do you want to discard the "
						+ "remaining zones?\r\nPress Cancel to reconfigure your zones. Keep in mind that if you have no adjacent zones, a filler zone will have to be added\r\nso 32 non adjacent zones is the "
						+ "maximum number of zones you can have. Both intro and credits region also require a zone.", "Too many zones", MessageBoxButtons.OKCancel, MessageBoxIcon.Warning);
					if (dr == DialogResult.OK)
						break;
					else // user wants to abort
						return null;
				}
			}
			return retval;
		}
		/// <summary>
		/// compiles the final zone configuration based on intro end frame, credits start frame and the configured zones
		/// </summary>
		/// <param name="vSettings">the video settings containing the list of configured zones</param>
		/// <param name="introEndFrame">the frame where the intro ends</param>
		/// <param name="creditsStartFrame">the frame where the credits begin</param>
		/// <param name="newZones">the zones that are returned</param>
		/// <returns>an array of zones objects in the proper order</returns>
		public bool getFinalZoneConfiguration(VideoCodecSettings vSettings, int introEndFrame, int creditsStartFrame, ref Zone[] zones)
		{
			Zone introZone = new Zone();
			Zone creditsZone = new Zone();
			ulong nbOfFrames = getNumberOfFrames(mainForm.Video.VideoInput);
			bool doIntroZone = false, doCreditsZone = false;
			int flushZonesStart = 0, flushZonesEnd = 0;
			if (introEndFrame > 0) // add the intro zone
			{
				introZone.startFrame = 0;
				introZone.endFrame = introEndFrame;
				introZone.mode = ZONEMODE.Quantizer;
				introZone.modifier = vSettings.CreditsQuantizer;
				if (zones.Length > 0)
				{
					Zone z = zones[0];
					if (z.startFrame > introZone.endFrame) // the first configured zone starts after the intro zone
						doIntroZone = true;
					else
					{
						flushZonesStart = 1;
						int numberOfConfiguredZones = zones.Length;
						while (flushZonesStart <= numberOfConfiguredZones)// iterate through all zones backwards until we find the first that goes with the intro
						{
							Zone conflict = zones[flushZonesStart];
							if (conflict.startFrame <= introZone.endFrame) // zone starts before the end of the intro -> conflict
								flushZonesStart++;
							else
								break;
						}
						DialogResult dr = MessageBox.Show("Your intro zone overlaps " + flushZonesStart + " zone(s) configured\nin the codec settings.\n"
							+ "Do you want to remove those zones and add the intro zone instead?", "Zone overlap detected", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Warning);
						if (dr == DialogResult.Yes)
							doIntroZone = true;
						else if (dr == DialogResult.Cancel) // abort
							return false;
						else // discard the intro zone
							flushZonesStart = 0;
					}
				}
				else
					doIntroZone = true;
			}
			if (creditsStartFrame > 0) // add the credits zone
			{
				creditsZone.startFrame = creditsStartFrame;
				creditsZone.endFrame = (int)nbOfFrames-1;
				creditsZone.mode = ZONEMODE.Quantizer;
				creditsZone.modifier = vSettings.CreditsQuantizer;
				if (zones.Length > 0)
				{
					Zone z = zones[zones.Length - 1]; // get the last zone
					if (z.endFrame < creditsZone.startFrame) // the last configured zone ends before the credits start zone
						doCreditsZone = true;
					else
					{
						flushZonesEnd = 1;
						int numberOfConfiguredZones = zones.Length;
						while (numberOfConfiguredZones - flushZonesEnd -1 >= 0)// iterate through all zones backwards until we find the first that goes with the credits
						{
							Zone conflict = zones[numberOfConfiguredZones - flushZonesEnd -1];
							if (conflict.endFrame >= creditsZone.startFrame) // zone ends after the end of the credits -> conflict
								flushZonesEnd++;
							else
								break;
						}
						DialogResult dr = MessageBox.Show("Your credits zone overlaps " + flushZonesEnd + " zone(s) configured\nin the codec settings.\n"
							+ "Do you want to remove those zones and add the credits zone instead?", "Zone overlap detected", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Warning);
						if (dr == DialogResult.Yes)
							doCreditsZone = true;
						else if (dr == DialogResult.Cancel) // abort
							return false;
						else // discard the credits zone
							flushZonesEnd = 0;
					}
				}
				else // no additional zones configured
					doCreditsZone = true;
			}
			int newZoneSize = zones.Length - flushZonesStart - flushZonesEnd;
			if (doIntroZone)
				newZoneSize++;
			if (doCreditsZone)
				newZoneSize++;
			Zone[] newZones = new Zone[newZoneSize];
			int index = 0;
			if (doIntroZone)
			{
				newZones[index] = introZone;
				index++;
			}
			for (int i = flushZonesStart; i < zones.Length - flushZonesEnd; i++)
			{
				newZones[index] = zones[i];
				index++;
			}
			if (doCreditsZone)
			{
				newZones[index] = creditsZone;
				index++;
			}
			if (vSettings is xvidSettings && newZones.Length > 0)
			{
				Zone[] xvidZones = createHelperZones(newZones, (int)nbOfFrames);
				if (xvidZones == null)
					return false;
				else
				{
					zones = xvidZones;
					return true;
				}
			}
			zones = newZones;
			return true;
		}
		#endregion
	}
}
