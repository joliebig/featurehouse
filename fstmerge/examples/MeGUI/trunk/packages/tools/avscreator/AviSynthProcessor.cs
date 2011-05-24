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
using System.Diagnostics;
using System.Text;
using System.Threading;

using MeGUI.core.util;

namespace MeGUI
{
 	public delegate void AviSynthStatusUpdateCallback(StatusUpdate su);

    public class AviSynthProcessor : IJobProcessor
    {
        DateTime startTime;
        public static readonly JobProcessorFactory Factory =
       new JobProcessorFactory(new ProcessorFactory(init), "AviSynthProcessor");

        private static IJobProcessor init(MainForm mf, Job j)
        {
            if (j is AviSynthJob) return new AviSynthProcessor();
            return null;
        }

        
        protected System.Threading.ManualResetEvent mre = new System.Threading.ManualResetEvent(true); // lock used to pause encoding

        private AvsFile file;
        private IVideoReader reader;
        private bool aborted;
        private ulong position;
        private Thread processorThread, statusThread;
        public StatusUpdate stup = null;
        private AviSynthJob job;
        
        
        public AviSynthProcessor()
        {
        }
        
        

        private void update()
        {
            while (!aborted && position < stup.NbFramesTotal)
            {
                stup.NbFramesDone = position;
                stup.TimeElapsed = DateTime.Now - startTime;
                stup.FillValues();
                StatusUpdate(stup);
                Thread.Sleep(1000);
            }
            stup.IsComplete = true;
            StatusUpdate(stup);
        }

        private void process()
        {
            IntPtr zero = new IntPtr(0);
            for (position = 0; position < stup.NbFramesTotal && !aborted; position++)
            {
                file.Clip.ReadFrame(zero, 0, (int)position);
                mre.WaitOne();
            }
            file.Dispose();
        }
        /// <summary>
        /// sets up encoding
        /// </summary
        /// <param name="job">the job to be processed</param>
        /// <param name="error">output for any errors that might ocurr during this method</param>
        /// <returns>true if the setup has succeeded, false if it has not</returns>
        public void setup(Job job, StatusUpdate su, LogItem _)
        {
            Debug.Assert(job is AviSynthJob, "Job isn't an AviSynthJob");
            stup = su;
            this.job = (AviSynthJob)job;

            try 
            {
                file = AvsFile.OpenScriptFile(job.Input);
                reader = file.GetVideoReader();
            }
            catch (Exception ex)
            {
                throw new JobRunException(ex);
            }
            stup.NbFramesTotal = (ulong)reader.FrameCount;
            stup.ClipLength = TimeSpan.FromSeconds((double)stup.NbFramesTotal / file.Info.FPS);
            stup.Status = "Playing through file...";
            position = 0;
            try
            {
                processorThread = new Thread(new ThreadStart(process));
            }
            catch (Exception e)
            {
                throw new JobRunException(e);
            }
            try
            {
                statusThread = new Thread(new ThreadStart(update));
            }
            catch (Exception e)
            {
                throw new JobRunException(e);
            }
        }
        /// <summary>
        /// starts the encoding process
        /// </summary>
        /// <param name="error">output for any errors that might ocurr during this method</param>
        /// <returns>true if encoding has been successfully started, false if not</returns>
        public void start()
        {
            try
            {
                statusThread.Start();
                processorThread.Start();
                startTime = DateTime.Now;
            }
            catch (Exception e)
            {
                throw new JobRunException(e);
            }
        }
        /// <summary>
        /// stops the encoding process
        /// </summary>
        /// <param name="error">output for any errors that might ocurr during this method</param>
        /// <returns>true if encoding has been successfully stopped, false if not</returns>
        public void stop()
        {
            aborted = true;
        }
        /// <summary>
        /// pauses the encoding process
        /// </summary>
        /// <param name="error">output for any errors that might ocurr during this method</param>
        /// <returns>true if encoding has been successfully paused, false if not</returns>
        public void pause()
        {
            if (!mre.Reset())
                throw new JobRunException("Could not reset mutex");
        }
        /// <summary>
        /// resumes the encoding process
        /// </summary>
        /// <param name="error">output for any errors that might ocurr during this method</param>
        /// <returns>true if encoding has been successfully started, false if not</returns>
        public void resume()
        {
            if (!mre.Set())
                throw new JobRunException("Could not set mutex");
        }
        /// <summary>
        /// changes the priority of the encoding process/thread
        /// </summary>
        /// <param name="priority">the priority to change to</param>
        /// <param name="error">output for any errors that might ocurr during this method</param>
        /// <returns>true if the priority has been changed, false if not</returns>
        public void changePriority(ProcessPriority priority)
        {
            if (processorThread != null && processorThread.IsAlive)
            {
                try
                {
                    if (priority == ProcessPriority.IDLE)
                        processorThread.Priority = ThreadPriority.Lowest;
                    else if (priority == ProcessPriority.NORMAL)
                        processorThread.Priority = ThreadPriority.Normal;
                    else if (priority == ProcessPriority.HIGH)
                        processorThread.Priority = ThreadPriority.Highest;
                    return;
                }
                catch (Exception e) // process could not be running anymore
                {
                    throw new JobRunException(e);
                }
            }
            else
            {
                if (processorThread == null)
                    throw new JobRunException("Process has not been started yet");
                else
                    throw new JobRunException("Process has exited");
            }
        }
        public event JobProcessingStatusUpdateCallback StatusUpdate;
        
    }
}
