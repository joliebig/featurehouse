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
using System.IO;
using System.Text;
using System.Threading;

using MeGUI.core.plugins.implemented;
using MeGUI.core.util;

namespace MeGUI
{
    public enum StreamType : ushort { None = 0, Stderr = 1, Stdout = 2 }

    public abstract class CommandlineJobProcessor<TJob> : IJobProcessor
        where TJob : Job
    {
        
        protected TJob job;
        protected DateTime startTime;
        protected bool isProcessing = false;
        protected Process proc = new Process(); // the encoder process
        protected string executable; // path and filename of the commandline encoder to be used
        protected ManualResetEvent mre = new ManualResetEvent(true); // lock used to pause encoding
        protected ManualResetEvent stdoutDone = new ManualResetEvent(false);
        protected ManualResetEvent stderrDone = new ManualResetEvent(false);
        protected StatusUpdate su;
        protected LogItem log;
        protected StringBuilder stdoutBuilder = new StringBuilder();
        protected StringBuilder stderrBuilder = new StringBuilder();
        protected Thread readFromStdErrThread;
        protected Thread readFromStdOutThread;
        protected List<string> tempFiles = new List<string>();

        

        
        protected void writeTempTextFile(string filePath, string text)
        {
            using (Stream temp = new FileStream(filePath, System.IO.FileMode.Create))
            {
                using (TextWriter avswr = new StreamWriter(temp, System.Text.Encoding.Default))
                {
                    avswr.WriteLine(text);
                }
            }
            tempFiles.Add(filePath);
        }
        private void deleteTempFiles()
        {
            foreach (string filePath in tempFiles)
                safeDelete(filePath);

        }

        private static void safeDelete(string filePath)
        {
            try
            {
                File.Delete(filePath);
            }
            catch
            {
                // Do Nothing
            }
        }
        

        protected virtual void checkJobIO()
        {
            Util.ensureExists(job.Input);
        }

        protected virtual void doExitConfig()
        { }

        // returns true if the exit code yields a meaningful answer
        protected virtual bool checkExitCode
        {
            get { return true; }
        }

        protected abstract string Commandline
        {
            get;
        }


        /// <summary>
        /// handles the encoder process existing
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        protected void proc_Exited(object sender, EventArgs e)
        {
            mre.Set();  // Make sure nothing is waiting for pause to stop
            stdoutDone.WaitOne(); // wait for stdout to finish processing
            stderrDone.WaitOne(); // wait for stderr to finish processing
            if (checkExitCode && proc.ExitCode != 0) // check the exitcode because x264.exe sometimes exits with error but without
                su.HasError = true; // any commandline indication as to why

            log.LogValue("Standard output stream", stdoutBuilder);
            log.LogValue("Standard error stream", stderrBuilder);
            su.IsComplete = true;
            doExitConfig();
            StatusUpdate(su);
        }

        

        public void setup(Job job2, StatusUpdate su, LogItem log)
        {
            Debug.Assert(job2 is TJob, "Job is the wrong type");

            this.log = log;
            TJob job = (TJob)job2;
            this.job = job;

            // This enables relative paths, etc
            executable = Path.Combine(System.Windows.Forms.Application.StartupPath, executable);

            Util.ensureExists(executable);

            this.su = su;
            checkJobIO();

        }

        public void start()
        {
            proc = new Process();
            ProcessStartInfo pstart = new ProcessStartInfo();
            pstart.FileName = executable;
            pstart.Arguments = Commandline;
            log.LogValue("Job commandline", '"' + pstart.FileName + "\" " + pstart.Arguments);
            pstart.RedirectStandardOutput = true;
            pstart.RedirectStandardError = true;
            pstart.WindowStyle = ProcessWindowStyle.Minimized;
            pstart.CreateNoWindow = true;
            pstart.UseShellExecute = false;
            proc.StartInfo = pstart;
            proc.EnableRaisingEvents = true;
            proc.Exited += new EventHandler(proc_Exited);

            try
            {
                bool started = proc.Start();
                startTime = DateTime.Now;
                isProcessing = true;
                readFromStdErrThread = new Thread(new ThreadStart(readStdErr));
                readFromStdOutThread = new Thread(new ThreadStart(readStdOut));
                readFromStdOutThread.Start();
                readFromStdErrThread.Start();
                new System.Windows.Forms.MethodInvoker(this.RunStatusCycle).BeginInvoke(null, null);
                this.changePriority(MainForm.Instance.Settings.DefaultPriority);
            }
            catch (Exception e)
            {
                throw new JobRunException(e);
            }
        }

        public void stop()
        {
            if (proc != null && !proc.HasExited)
            {
                try
                {
                    mre.Set(); // if it's paused, then unpause
                    su.WasAborted = true;
                    proc.Kill();
                    proc.WaitForExit(10000);
                    return;
                }
                catch (Exception e)
                {
                    throw new JobRunException(e);
                }
            }
            else
            {
                if (proc == null)
                    throw new JobRunException("Encoder process does not exist");
                else
                    throw new JobRunException("Encoder process has already existed");
            }
        }

        public void pause()
        {
            if (!canPause)
                throw new JobRunException("Can't pause this kind of job.");
            if (!mre.Reset())
                throw new JobRunException("Could not reset mutex. pause failed");
        }

        public void resume()
        {
            if (!canPause)
                throw new JobRunException("Can't resume this kind of job.");
            if (!mre.Set())
                throw new JobRunException("Could not set mutex. pause failed");
        }

        public bool isRunning()
        {
            return (proc != null && !proc.HasExited);
        }

        public void changePriority(ProcessPriority priority)
        {
            if (isRunning())
            {
                try
                {
    				switch (priority)
					{
						case ProcessPriority.IDLE:
	    						proc.PriorityClass = ProcessPriorityClass.Idle;
								break;
						case ProcessPriority.BELOW_NORMAL:
								proc.PriorityClass = ProcessPriorityClass.BelowNormal;
								break;
		    			case ProcessPriority.NORMAL:
			    				proc.PriorityClass = ProcessPriorityClass.Normal;
				    			break;
						case ProcessPriority.ABOVE_NORMAL:
					    		proc.PriorityClass = ProcessPriorityClass.AboveNormal;
								break;
						case ProcessPriority.HIGH:
						    	proc.PriorityClass = ProcessPriorityClass.RealTime;
								break;
					}
				    return;
                }
                catch (Exception e) // process could not be running anymore
                {
                    throw new JobRunException(e);
                }
            }
            else 
            {
                if (proc == null)
                    throw new JobRunException("Process has not been started yet");
                else
                {
                    Debug.Assert(proc.HasExited);
                    throw new JobRunException("Process has exited");
                }
            }
        }

        public virtual bool canPause
        {
            get { return true; }
        }


        
        
        protected virtual void readStream(StreamReader sr, ManualResetEvent rEvent, StreamType str)
        {
            string line;
            if (proc != null)
            {
                try
                {
                    while ((line = sr.ReadLine()) != null)
                    {
                        mre.WaitOne();
                        ProcessLine(line, str);
                    }
                }
                catch (Exception e)
                {
                    ProcessLine("Exception in readStdErr: " + e.Message, str);
                }
                rEvent.Set();
            }
        }
        protected void readStdOut()
        {
            StreamReader sr = null;
            try
            {
                sr = proc.StandardOutput;
            }
            catch (Exception e)
            {
                log.LogValue("Exception getting IO reader for stdout", e, ImageType.Error);
                stdoutDone.Set();
                return;
            }
            readStream(sr, stdoutDone, StreamType.Stdout);
        }
        protected void readStdErr()
        {
            StreamReader sr = null;
            try
            {
                sr = proc.StandardError;
            }
            catch (Exception e)
            {
                log.LogValue("Exception getting IO reador for stderr", e, ImageType.Error);
                stderrDone.Set();
                return;
            }
            readStream(sr, stderrDone, StreamType.Stderr);
        }

        public virtual void ProcessLine(string line, StreamType stream)
        {
            if (stream == StreamType.Stdout)
                stdoutBuilder.AppendLine(line);
            if (stream == StreamType.Stderr)
                stderrBuilder.AppendLine(line);
        }

        
        
        public event JobProcessingStatusUpdateCallback StatusUpdate;
        protected void RunStatusCycle()
        {
            while (isRunning())
            {
                su.TimeElapsed = DateTime.Now - startTime;
                su.CurrentFileSize = FileSize.Of2(job.Output);

                doStatusCycleOverrides();
                su.FillValues();
                if (StatusUpdate != null && proc != null && !proc.HasExited)
                    StatusUpdate(su);
                Thread.Sleep(1000);

            }
        }

        protected virtual void doStatusCycleOverrides()
        { }
        
    }
}
