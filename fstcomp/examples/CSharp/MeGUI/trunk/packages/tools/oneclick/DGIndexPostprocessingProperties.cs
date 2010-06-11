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
using System.Xml.Serialization;

using MeGUI.core.details;
using MeGUI.core.util;

namespace MeGUI
{
	/// <summary>
	/// Summary description for DGIndexPostprocessingProperties.
	/// </summary>
	public class DGIndexPostprocessingProperties
	{
		private bool autoDeriveAR, signalAR, autoDeint, autoCrop, keepInputResolution, prerenderJob, useChapterMarks;
		private int horizontalOutputResolution;
        private FileSize? splitSize;
        private ContainerType container;
		private FileSize? outputSize;
		private Dar? ar;
		private VideoCodecSettings videoSettings;
        private AviSynthSettings avsSettings;
		private double customAR;
		private string chapterFile, finalOutput, aviSynthScript, deviceType;

		public DGIndexPostprocessingProperties()
		{
			autoDeriveAR = false;
			signalAR = false;
            autoCrop = true;
            keepInputResolution = false;
            ar = null;
            avsSettings = new AviSynthSettings();
			horizontalOutputResolution = 640;
			customAR = 1.0;
			container = MeGUI.ContainerType.MKV;
            outputSize = null;
			splitSize = null;
            prerenderJob = false;
            deviceType = null;
            useChapterMarks = false;
		}

        public AudioJob[] AudioJobs;
        public MuxStream[] DirectMuxAudio;

        public bool AutoDeinterlace
        {
            get { return autoDeint; }
            set { autoDeint = value; }
        }
        /// <summary>
		/// gets / sets whether the aspect ratio should be derived from the dgindex project
		/// </summary>
		public bool AutoDeriveAR
		{
			get { return autoDeriveAR; }
			set { autoDeriveAR = value; }
		}
		/// <summary>
		/// gets / sets whether the aspect ratio should be signalled in the output and thus
		/// resizing should not unstretch anamorphically stretched content
		/// </summary>
		public bool SignalAR
		{
			get { return signalAR; }
			set { signalAR = value; }
		}
        /// <summary>
        /// gets / sets the AutoCrop function
        /// </summary>
        public bool AutoCrop
        {
            get { return autoCrop; }
            set { autoCrop = value; }
        }
        /// <summary>
        /// gets / sets Keep Input Resolution
        /// </summary>
        public bool KeepInputResolution
        {
            get { return keepInputResolution; }
            set { keepInputResolution = value; }
        }
        /// <summary>
        /// gets / sets Prerender Job
        /// </summary>
        public bool PrerenderJob
        {
            get { return prerenderJob; }
            set { prerenderJob = value; }
        }
		/// <summary>
		/// gets / sets the horizontal output resolution the output should have
		/// </summary>
		public int HorizontalOutputResolution
		{
			get { return horizontalOutputResolution; }
			set { horizontalOutputResolution = value; }
		}
		/// <summary>
		/// gets / sets the container of the output
		/// </summary>
        [XmlIgnore()]
        public ContainerType Container
		{
			get { return container; }
			set { container = value; }
		}

        public string ContainerTypeString
        {
            get { return Container.ID; }
            set
            {
                foreach (ContainerType t in MainForm.Instance.MuxProvider.GetSupportedContainers())
                {
                    if (t.ID == value) { Container = t; return; }
                }
                Container = null;
            }
        }
        /// <summary>
		/// gets / sets the output size
		/// </summary>
		public FileSize? OutputSize
		{
			get { return outputSize; }
			set { outputSize = value; }
		}
		/// <summary>
		/// gets / sets the split size for the output
		/// </summary>
		public FileSize? Splitting
		{
			get { return splitSize; }
			set { splitSize = value; }
		}
		/// <summary>
		/// gets / sets the aspect ratio of the video input (if known)
		/// </summary>
		public Dar? DAR
		{
			get { return ar; }
			set { ar = value; }
		}
        public AviSynthSettings AvsSettings
        {
            get { return avsSettings; }
            set { avsSettings = value; }
        }
		/// <summary>
		/// gets / sets the video codec settings used for video encoding
		/// </summary>
		public VideoCodecSettings VideoSettings
		{
			get { return videoSettings; }
			set { videoSettings = value; }
		}
		/// <summary>
		/// gets / sets a custom aspect ratio for the input
		/// (requires AR set to AspectRatio.Custom to be taken into account)
		/// </summary>
		public double CustomAR
		{
			get { return customAR; }
			set { customAR = value; }
		}
		/// <summary>
		/// gets / sets the chapter file for the output
		/// </summary>
		public string ChapterFile
		{
			get { return chapterFile; }
			set { chapterFile = value; }
		}
		/// <summary>
		/// gets / sets the path and name of the final output file
		/// </summary>
		public string FinalOutput
		{
			get { return finalOutput; }
			set { finalOutput = value; }
		}
		/// <summary>
		/// gets / sets the path and name of the AviSynth script created from the dgindex project
		/// this is filled in during postprocessing
		/// </summary>
		public string AviSynthScript
		{
			get { return aviSynthScript; }
			set { aviSynthScript = value; }
		}
        /// <summary>
        /// gets / sets the device output type
        /// </summary>
        public string DeviceOutputType
        {
            get { return deviceType; }
            set { deviceType = value; }
        }

        public bool UseChaptersMarks
        {
            get { return useChapterMarks; }
            set { useChapterMarks = value; }
        }
	}
}