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
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using System.Windows.Forms;

using MeGUI.core.util;

using MediaInfoWrapper;

namespace MeGUI
{
    public sealed class AviSynthAudioEncoder : IJobProcessor // : AudioEncoder
    {
        public static readonly JobProcessorFactory Factory =
new JobProcessorFactory(new ProcessorFactory(init), "AviSynthAudioEncoder");

        private static IJobProcessor init(MainForm mf, Job j)
        {
            if (j is AudioJob &&
                (((j as AudioJob).Settings is MP3Settings) ||
                ((j as AudioJob).Settings is MP2Settings) ||
                ((j as AudioJob).Settings is AC3Settings) ||
                ((j as AudioJob).Settings is WinAmpAACSettings) ||
                ((j as AudioJob).Settings is AudXSettings) ||
                ((j as AudioJob).Settings is OggVorbisSettings) ||
                ((j as AudioJob).Settings is FaacSettings) ||
                ((j as AudioJob).Settings is NeroAACSettings) ||
                ((j as AudioJob).Settings is AftenSettings)))
                return new AviSynthAudioEncoder(mf.Settings);
            return null;
        }

        #region fields
        private Process _encoderProcess;
        private string _avisynthAudioScript;
        private string _encoderExecutablePath;
        private string _encoderCommandLine;
        private bool _mustSendWavHeaderToEncoderStdIn;

        private int _sampleRate;
        private int _downMixModeNb;

        private System.Threading.ManualResetEvent _mre = new System.Threading.ManualResetEvent(true); // lock used to pause encoding
        private Thread _encoderThread = null;
        private Thread _readFromStdOutThread = null;
        private Thread _readFromStdErrThread = null;
        private string _encoderStdErr = null;
        private string _encoderStdOut = null;
        private LogItem _log;
        private static readonly System.Text.RegularExpressions.Regex _cleanUpStringRegex = new System.Text.RegularExpressions.Regex(@"\n[^\n]+\r", System.Text.RegularExpressions.RegexOptions.Compiled | System.Text.RegularExpressions.RegexOptions.CultureInvariant);

        private MeGUISettings _settings = null;
        private int SAMPLES_PER_UPDATE;
        private AudioJob audioJob;
        private StatusUpdate su;
        private DateTime _start;

        private List<string> _tempFiles = new List<string>();
        private readonly string _uniqueId = Guid.NewGuid().ToString("N");
        #endregion

        #region methods

        private void writeTempTextFile(string filePath, string text)
        {
            using (Stream temp = new FileStream(filePath, System.IO.FileMode.Create))
            {
                using (TextWriter avswr = new StreamWriter(temp, System.Text.Encoding.Default))
                {
                    avswr.WriteLine(text);
                }
            }
            _tempFiles.Add(filePath);
        }


        private void deleteTempFiles()
        {
            foreach (string filePath in _tempFiles)
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

        private void createTemporallyEqFiles(string tempPath)
        {
            // http://forum.doom9.org/showthread.php?p=778156#post778156
            writeTempTextFile(tempPath + "front.feq", @"-96
-96
-96
-4
-4
-4
-4
-4
-4
-4
-4
-4
-4
-4
-4
-96
-96
-96
");
            writeTempTextFile(tempPath + "center.feq", @"-96
-96
-96
-96
-96
-96
3
3
3
3
3
3
3
3
3
3
3
3
");
            writeTempTextFile(tempPath + "lfe.feq", @"0
0
0
-96
-96
-96
-96
-96
-96
-96
-96
-96
-96
-96
-96
-96
-96
-96
");
            writeTempTextFile(tempPath + "back.feq", @"-96
-96
-96
-6
-6
-6
-6
-6
-6
-6
-6
-6
-6
-6
-6
-96
-96
-96
");
        }

        private void raiseEvent()
        {
            if (su.IsComplete = (su.IsComplete || su.WasAborted || su.HasError))
                createLog();
            if (StatusUpdate != null)
                StatusUpdate(su);
        }

        private void setProgress(decimal n)
        {
            su.PercentageDoneExact = n * 100M;
            su.CurrentFileSize = FileSize.Of2(audioJob.Output);
            su.TimeElapsed = DateTime.Now - _start;
            su.FillValues();
            raiseEvent();
        }

        private void raiseEvent(string s)
        {
            su.Status = s;
            raiseEvent();
        }


        internal AviSynthAudioEncoder(MeGUISettings settings)
        {
            SAMPLES_PER_UPDATE = (int)settings.AudioSamplesPerUpdate;
            _settings = settings;
        }

        private void readStdOut()
        {
            readStdStream(true);
        }

        private void readStdErr()
        {
            readStdStream(false);
        }

        private string cleanUpString(string s)
        {
            return _cleanUpStringRegex.Replace(s.Replace(Environment.NewLine, "\n"), Environment.NewLine);
        }

        private void readStdStream(bool bStdOut)
        {
            using (StreamReader r = bStdOut ? _encoderProcess.StandardOutput : _encoderProcess.StandardError)
            {
                while (!_encoderProcess.HasExited)
                {
                    Thread.Sleep(0);
                    string text1 = r.ReadToEnd();
                    if (text1 != null)
                    {
                        if (text1.Length > 0)
                        {
                            if (bStdOut)
                                _encoderStdOut = cleanUpString(text1);
                            else
                                _encoderStdErr = cleanUpString(text1);
                        }
                    }
                    Thread.Sleep(0);
                }
            }
        }

        private void encode()
        {
            try
            {
                _log.LogEvent("Encode thread started");
                raiseEvent("Preprocessing...   ***PLEASE WAIT***");
                using (AviSynthScriptEnvironment env = new AviSynthScriptEnvironment())
                {
                    _log.LogEvent("Avisynth script environment opened");
                    using (AviSynthClip a = env.ParseScript(_avisynthAudioScript))
                    {
                        _log.LogEvent("Script loaded");
                        if (0 == a.ChannelsCount)
                            throw new ApplicationException("Can't find audio stream");

                        LogItem inputLog = _log.Info("Output Decoder");
                        inputLog.LogValue("Channels", a.ChannelsCount);
                        inputLog.LogValue("Bits per sample", a.BitsPerSample);
                        inputLog.LogValue("Sample rate", a.AudioSampleRate);

                        _start = DateTime.Now;

                        const int MAX_SAMPLES_PER_ONCE = 4096;
                        int frameSample = 0;
                        int lastUpdateSample = 0;
                        int frameBufferTotalSize = MAX_SAMPLES_PER_ONCE * a.ChannelsCount * a.BytesPerSample;
                        byte[] frameBuffer = new byte[frameBufferTotalSize];
                        createEncoderProcess(a);
                        _log.LogEvent("Encoder process started");
                        try
                        {
                            using (Stream target = _encoderProcess.StandardInput.BaseStream)
                            {
                                // let's write WAV Header
                                if (_mustSendWavHeaderToEncoderStdIn)
                                    writeHeader(target, a);

                                _sampleRate = a.AudioSampleRate;

                                bool hasStartedEncoding = false;

                                GCHandle h = GCHandle.Alloc(frameBuffer, GCHandleType.Pinned);
                                IntPtr address = h.AddrOfPinnedObject();
                                try
                                {
                                    su.ClipLength = TimeSpan.FromSeconds((double)a.SamplesCount / (double)_sampleRate);
                                    while (frameSample < a.SamplesCount)
                                    {
                                        _mre.WaitOne();
                                        
                                        if (_encoderProcess != null)
                                            if (_encoderProcess.HasExited)
                                                throw new ApplicationException("Abnormal encoder termination " + _encoderProcess.ExitCode.ToString());
                                        int nHowMany = Math.Min((int)(a.SamplesCount - frameSample), MAX_SAMPLES_PER_ONCE);
                                        a.ReadAudio(address, frameSample, nHowMany);
                                        
                                        _mre.WaitOne();
                                        if (!hasStartedEncoding)
                                        {
                                            raiseEvent("Encoding audio...");
                                            hasStartedEncoding = true;
                                        }


                                        target.Write(frameBuffer, 0, nHowMany * a.ChannelsCount * a.BytesPerSample);
                                        target.Flush();
                                        frameSample += nHowMany;
                                        if (frameSample - lastUpdateSample > SAMPLES_PER_UPDATE)
                                        {
                                            setProgress((decimal)frameSample / (decimal)a.SamplesCount);
                                            lastUpdateSample = frameSample;
                                        }
                                        Thread.Sleep(0);
                                    }
                                }
                                finally
                                {
                                    h.Free();
                                }
                                setProgress(1M);

                                if (_mustSendWavHeaderToEncoderStdIn && a.BytesPerSample % 2 == 1)
                                    target.WriteByte(0);
                            }
                            raiseEvent("Finalizing encoder");
                            _encoderProcess.WaitForExit();
                            _readFromStdErrThread.Join();
                            _readFromStdOutThread.Join();
                            if (0 != _encoderProcess.ExitCode)
                                throw new ApplicationException("Abnormal encoder termination " + _encoderProcess.ExitCode.ToString());

                        }
                        finally
                        {
                            if (!_encoderProcess.HasExited)
                            {
                                _encoderProcess.Kill();
                                _encoderProcess.WaitForExit();
                                _readFromStdErrThread.Join();
                                _readFromStdOutThread.Join();
                            }
                            _readFromStdErrThread = null;
                            _readFromStdOutThread = null;
                        }
                    }
                }
            }
            catch (Exception e)
            {
                deleteOutputFile();
                if (e is ThreadAbortException)
                {
                    _log.LogEvent("Aborting...");
                    su.WasAborted = true;
                    raiseEvent();
                }
                else
                {
                    _log.LogValue("An error occurred", e, ImageType.Error);
                    su.HasError = true;
                    raiseEvent();
                }
                return;
            }
            finally
            {
                deleteTempFiles();
            }
            su.IsComplete = true;
            raiseEvent();
        }

        private void createLog()
        {
            if (_encoderStdErr != null)
                _log.LogValue("Output from encoder via stderr", _encoderStdErr + Environment.NewLine);

            if (_encoderStdOut != null)
                _log.LogValue("Output from encoder via stdout", _encoderStdOut + Environment.NewLine);
        }

        private void deleteOutputFile()
        {
            safeDelete(audioJob.Output);
        }

        private void createEncoderProcess(AviSynthClip a)
        {
            try
            {
                _encoderProcess = new Process();
                ProcessStartInfo info = new ProcessStartInfo();
                // Command line arguments, to be passed to encoder
                // {0} means output file name
                // {1} means samplerate in Hz
                // {2} means bits per sample
                // {3} means channel count
                // {4} means samplecount
                // {5} means size in bytes
                info.Arguments = string.Format(_encoderCommandLine,
                    audioJob.Output, a.AudioSampleRate, a.BitsPerSample, a.ChannelsCount, a.SamplesCount, a.AudioSizeInBytes);
                info.FileName = _encoderExecutablePath;
                _log.LogValue("Commandline", _encoderExecutablePath + " " + info.Arguments);
                info.UseShellExecute = false;
                info.RedirectStandardInput = true;
                info.RedirectStandardOutput = true;
                info.RedirectStandardError = true;
                info.CreateNoWindow = true;
                _encoderProcess.StartInfo = info;
                _encoderProcess.Start();

                // Take priority from Avisynth thread rather than default in settings
                // just in case user has managed to change job setting before getting here.
                if (_encoderThread.Priority == ThreadPriority.Lowest)
                    _encoderProcess.PriorityClass = ProcessPriorityClass.Idle;
                else if (_encoderThread.Priority == ThreadPriority.Normal)
                    _encoderProcess.PriorityClass = ProcessPriorityClass.Normal;
                else if (_encoderThread.Priority == ThreadPriority.AboveNormal)
                    _encoderProcess.PriorityClass = ProcessPriorityClass.High;

                _readFromStdOutThread = new Thread(new ThreadStart(readStdOut));
                _readFromStdErrThread = new Thread(new ThreadStart(readStdErr));
                _readFromStdOutThread.Start();
                _readFromStdOutThread.Priority = ThreadPriority.Normal;
                _readFromStdErrThread.Start();
                _readFromStdErrThread.Priority = ThreadPriority.Normal;
            }
            catch (Exception e)
            {
                throw new ApplicationException("Can't start encoder: " + e.Message, e);
            }
        }

        private void writeHeader(Stream target, AviSynthClip a)
        {
            const uint FAAD_MAGIC_VALUE = 0xFFFFFF00;
            const uint WAV_HEADER_SIZE = 36;
            bool useFaadTrick = a.AudioSizeInBytes >= ((long)uint.MaxValue - WAV_HEADER_SIZE);
            target.Write(System.Text.Encoding.ASCII.GetBytes("RIFF"), 0, 4);
            target.Write(BitConverter.GetBytes(useFaadTrick ? FAAD_MAGIC_VALUE : (uint)(a.AudioSizeInBytes + WAV_HEADER_SIZE)), 0, 4);
            target.Write(System.Text.Encoding.ASCII.GetBytes("WAVEfmt "), 0, 8);
            target.Write(BitConverter.GetBytes((uint)0x10), 0, 4);
            target.Write(BitConverter.GetBytes((a.SampleType==AudioSampleType.FLOAT) ? (short)0x03 : (short)0x01), 0, 2);
            target.Write(BitConverter.GetBytes(a.ChannelsCount), 0, 2);
            target.Write(BitConverter.GetBytes(a.AudioSampleRate), 0, 4);
            target.Write(BitConverter.GetBytes(a.AvgBytesPerSec), 0, 4);
            target.Write(BitConverter.GetBytes(a.BytesPerSample*a.ChannelsCount), 0, 2);
            target.Write(BitConverter.GetBytes(a.BitsPerSample), 0, 2);
            target.Write(System.Text.Encoding.ASCII.GetBytes("data"), 0, 4);
            target.Write(BitConverter.GetBytes(useFaadTrick ? (FAAD_MAGIC_VALUE - WAV_HEADER_SIZE) : (uint)a.AudioSizeInBytes), 0, 4);
        }


        internal void Start()
        {
            _encoderThread = new Thread(new ThreadStart(this.encode));
            _encoderThread.Priority = ThreadPriority.BelowNormal;
            _encoderThread.Start();
        }

        internal void Abort()
        {
            _encoderThread.Abort();
            _encoderThread = null;
        }

        #endregion

        #region IJobProcessor Members


        public void setup(Job job, StatusUpdate su, LogItem log)
        {
            this._log = log;
            this.audioJob = (AudioJob)job;

            this.su = su;


            //let's create avisynth script
            StringBuilder script = new StringBuilder();

            string id = _uniqueId;
            string tmp = Path.Combine(Path.GetTempPath(), id);
            FileInfo fi = new FileInfo(audioJob.Input);
            long size = fi.Length;

            bool directShow = audioJob.Settings.ForceDecodingViaDirectShow;
            if (!directShow)
            {
                switch (Path.GetExtension(audioJob.Input).ToLower())
                {
                    case ".ac3":
                    case ".ddp":
                    case ".eac3":
                        script.AppendFormat("LoadPlugin(\"{0}\"){1}", Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "NicAudio.dll"), Environment.NewLine);
                        script.AppendFormat("NicAc3Source(\"{0}\"", audioJob.Input);
                        if (audioJob.Settings.ApplyDRC)
                            script.AppendFormat(", DRC=1){0}", Environment.NewLine);
                        else
                            script.AppendFormat("){0}", Environment.NewLine);
                        break;
                    case ".avi":
                        script.AppendFormat("AVISource(\"{0}\", audio=true){1}", audioJob.Input, Environment.NewLine);
                        script.AppendFormat("EnsureVBRMP3Sync(){0}", Environment.NewLine);
                        script.AppendFormat("Trim(0,0){0}", Environment.NewLine); // to match audio length
                        break;
                    case ".avs":
                        script.AppendFormat("Import(\"{0}\"){1}", audioJob.Input, Environment.NewLine);
                        break;
                    case ".dtshd":
                    case ".dtsma":
                    case ".dts":
                        script.AppendFormat("LoadPlugin(\"{0}\"){1}", Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "NicAudio.dll"), Environment.NewLine);
                        script.AppendFormat("NicDtsSource(\"{0}\"", audioJob.Input);
                        if (audioJob.Settings.ApplyDRC)
                            script.AppendFormat(", DRC=1){0}", Environment.NewLine);
                        else
                            script.AppendFormat("){0}", Environment.NewLine);
                        break;
                    case ".mpa":
                    case ".mpg":
                    case ".mp2":
                    case ".mp3":
                        script.AppendFormat("LoadPlugin(\"{0}\"){1}", Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "NicAudio.dll"), Environment.NewLine);
                        script.AppendFormat("NicMPG123Source(\"{0}\"){1}", audioJob.Input, Environment.NewLine);
                        audioJob.FilesToDelete.Add(audioJob.Input + ".d2a");
                        break;
                    case ".wav":
                        BinaryReader r = new BinaryReader(File.Open(audioJob.Input, FileMode.Open));

                        try {
                            r.ReadBytes(20);
                            UInt16 AudioFormat = r.ReadUInt16();  // read a LE int_16, offset 20 + 2 = 22

                            switch (AudioFormat) {
                                case 0x0001:         // PCM Format Int
                                    r.ReadBytes(22);   // 22 + 22 = 44
                                    UInt32 DtsHeader = r.ReadUInt32(); // read a LE int_32
                                    script.AppendFormat("LoadPlugin(\"{0}\"){1}", Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "NicAudio.dll"), Environment.NewLine);
                                    if (DtsHeader == 0xE8001FFF)
                                    {
                                        script.AppendFormat("NicDtsSource(\"{0}\"", audioJob.Input);
                                        if (audioJob.Settings.ApplyDRC)
                                            script.AppendFormat(", DRC=1){0}", Environment.NewLine);
                                        else
                                            script.AppendFormat("){0}", Environment.NewLine);
                                    }
                                    else
                                        script.AppendFormat("RaWavSource(\"{0}\"){1}", audioJob.Input, Environment.NewLine);
                                    break;
                                case 0x0003:         // IEEE Float
                                case 0xFFFE:         // WAVE_FORMAT_EXTENSIBLE header
                                    script.AppendFormat("LoadPlugin(\"{0}\"){1}", Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "NicAudio.dll"), Environment.NewLine);
                                    script.AppendFormat("RaWavSource(\"{0}\"){1}", audioJob.Input, Environment.NewLine);
                                    break;
                                case 0x0055:         // MPEG Layer 3
                                    script.AppendFormat("LoadPlugin(\"{0}\"){1}", Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "NicAudio.dll"), Environment.NewLine);
                                    script.AppendFormat("NicMPG123Source(\"{0}\"){1}", audioJob.Input, Environment.NewLine);
                                    break;
                                case 0x2000:         // AC3
                                    script.AppendFormat("LoadPlugin(\"{0}\"){1}", Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "NicAudio.dll"), Environment.NewLine);
                                    script.AppendFormat("NicAc3Source(\"{0}\"", audioJob.Input);
                                    if (audioJob.Settings.ApplyDRC)
                                        script.AppendFormat(", DRC=1){0}", Environment.NewLine);
                                    else
                                        script.AppendFormat("){0}", Environment.NewLine); 
                                    break;
                                default:
                                    script.AppendFormat("WavSource(\"{0}\"){1}", audioJob.Input, Environment.NewLine);
                                    break;
                            }
                        }
                        catch(EndOfStreamException e) {
                            Console.WriteLine("{0}, wavfile can't be read.", e.GetType().Name);
                        }
                        finally {
                            r.Close();
                        }
                        break;
                    case ".w64":
                    case ".aif":
                    case ".au":
                    case ".caf":
                    case ".bwf":
                        script.AppendFormat("LoadPlugin(\"{0}\"){1}", Path.Combine(MainForm.Instance.Settings.AvisynthPluginsPath, "NicAudio.dll"), Environment.NewLine);
                        script.AppendFormat("RaWavSource(\"{0}\", 2){1}", audioJob.Input, Environment.NewLine);
                        break;
                    default:
                        directShow = true;
                        break;
                }
            }
            if (directShow)
            {
                try
                {
                    MediaInfo info = new MediaInfo(audioJob.Input);
                    if (info.Audio.Count > 0)
                    {
                        if (info.Video.Count > 0)
                             script.AppendFormat("DirectShowSource(\"{0}\", video=false){1}", audioJob.Input, Environment.NewLine);
                        else script.AppendFormat("DirectShowSource(\"{0}\"){1}", audioJob.Input, Environment.NewLine);
                        script.AppendFormat("EnsureVBRMP3Sync(){0}", Environment.NewLine);
                    }
                }
                catch (Exception)
                {
                    deleteTempFiles();
                    throw new JobRunException("Broken input file, " + audioJob.Input + ", can't continue.");
                } 
            } 
            
            if (audioJob.Delay != 0)
                script.AppendFormat("DelayAudio({0}.0/1000.0){1}", audioJob.Delay, Environment.NewLine);

            if (!string.IsNullOrEmpty(audioJob.CutFile))
            {
                try
                {
                    Cuts cuts = FilmCutter.ReadCutsFromFile(audioJob.CutFile);
                    script.AppendLine(FilmCutter.GetCutsScript(cuts, true));
                }
                catch (FileNotFoundException)
                {
                    deleteTempFiles();
                    throw new MissingFileException(audioJob.CutFile);
                }
                catch (Exception)
                {
                    deleteTempFiles();
                    throw new JobRunException("Broken cuts file, " + audioJob.CutFile + ", can't continue.");
                }
            }

            switch (audioJob.Settings.DownmixMode)
            {
                case ChannelMode.KeepOriginal:
                    _downMixModeNb = 0;
                    break;
                case ChannelMode.ConvertToMono:
                    script.AppendFormat("ConvertToMono(){0}", Environment.NewLine); _downMixModeNb = 1;
                    break;
                case ChannelMode.DPLDownmix:
                    script.Append("6<=Audiochannels(last)?x_dpl" + id + @"(ConvertAudioToFloat(last)):last" + Environment.NewLine); _downMixModeNb = 2;
                    break;
                case ChannelMode.DPLIIDownmix:
                    script.Append("6<=Audiochannels(last)?x_dpl2" + id + @"(ConvertAudioToFloat(last)):last" + Environment.NewLine); _downMixModeNb = 3;
                    break;
                case ChannelMode.StereoDownmix:
                    script.Append("6<=Audiochannels(last)?x_stereo" + id + @"(ConvertAudioToFloat(last)):last" + Environment.NewLine); _downMixModeNb = 4;
                    break;
                case ChannelMode.Upmix:
                    createTemporallyEqFiles(tmp);
                    script.Append("2==Audiochannels(last)?x_upmix" + id + @"(last):last" + Environment.NewLine); _downMixModeNb = 5;
                    break;
                case ChannelMode.UpmixUsingSoxEq:
                    script.Append("2==Audiochannels(last)?x_upmixR" + id + @"(last):last" + Environment.NewLine); _downMixModeNb = 6;
                    break;
                case ChannelMode.UpmixWithCenterChannelDialog:
                    script.Append("2==Audiochannels(last)?x_upmixC" + id + @"(last):last" + Environment.NewLine); _downMixModeNb = 7;
                    break;
            }

            // SampleRate
            switch (audioJob.Settings.SampleRateType)
            {
                case 0:
                    break;
                case 1:
                    script.Append("SSRC(8000)" + Environment.NewLine);
                    break;
                case 2:
                    script.Append("SSRC(11025)" + Environment.NewLine);
                    break;
                case 3:
                    script.Append("SSRC(22050)" + Environment.NewLine);
                    break;
                case 4:
                    script.Append("SSRC(44100)" + Environment.NewLine);
                    break;
                case 5:
                    script.Append("SSRC(48000)" + Environment.NewLine);
                    break;
                case 6:
                    script.Append("AssumeSampleRate((AudioRate()*1001+480)/960).SSRC(AudioRate())" + Environment.NewLine);
                    break;
                case 7:
                    script.Append("SSRC((AudioRate()*1001+480)/960).AssumeSampleRate(AudioRate())" + Environment.NewLine);
                    break;
            }

            // put Normalize() after downmix cases >> http://forum.doom9.org/showthread.php?p=1166117#post1166117
            if (audioJob.Settings.AutoGain)
            {
                if (audioJob.Settings.Normalize != 100)
                     script.AppendFormat("Normalize(" + (audioJob.Settings.Normalize / 100.0).ToString(new CultureInfo("en-us")) + "){0}", Environment.NewLine);
                else script.AppendFormat("Normalize(){0}", Environment.NewLine);
            }

            //let's obtain command line & other staff
            if (audioJob.Settings is AftenSettings)
            {
                _mustSendWavHeaderToEncoderStdIn = true;
                AftenSettings n = audioJob.Settings as AftenSettings;
                _encoderExecutablePath = this._settings.AftenPath;
                _encoderCommandLine = "-readtoeof 1 -b " + n.Bitrate + " - \"{0}\"";
            }
            if (audioJob.Settings is AC3Settings)
            {
                script.Append("6<=Audiochannels(last)?GetChannel(last,1,3,2,5,6,4):last" + Environment.NewLine);
                script.Append("32==Audiobits(last)?ConvertAudioTo16bit(last):last" + Environment.NewLine); // ffac3 encoder doesn't support 32bits streams
                _mustSendWavHeaderToEncoderStdIn = true;
                AC3Settings n = audioJob.Settings as AC3Settings;
                _encoderExecutablePath = this._settings.FFMpegPath;
                _encoderCommandLine = "-i - -y -acodec ac3 -ab " + n.Bitrate + "k \"{0}\"";
            }
            if (audioJob.Settings is MP2Settings)
            {
                script.Append("32==Audiobits(last)?ConvertAudioTo16bit(last):last" + Environment.NewLine); // ffmp2 encoder doesn't support 32 bits streams
                _mustSendWavHeaderToEncoderStdIn = true;
                MP2Settings n = audioJob.Settings as MP2Settings;
                _encoderExecutablePath = this._settings.FFMpegPath;
                _encoderCommandLine = "-i - -y -acodec mp2 -ab " + n.Bitrate + "k \"{0}\"";
            } 
            if (audioJob.Settings is WinAmpAACSettings)
            {
                _mustSendWavHeaderToEncoderStdIn = false;
                WinAmpAACSettings n = audioJob.Settings as WinAmpAACSettings;
                _encoderExecutablePath = this._settings.EncAacPlusPath;

                // Better Errors Exception for Audio Encoders
                string encoder_path = Path.GetDirectoryName(_encoderExecutablePath);

                if (!File.Exists(Path.Combine(encoder_path, "enc_aacplus.dll")))
                    FileUtil.CopyFile(MeGUISettings.WinampPath+"\\Plugins", encoder_path, "enc_aacplus.dll", true);
                if (!File.Exists(Path.Combine(encoder_path, "nscrt.dll")))
                    FileUtil.CopyFile(MeGUISettings.WinampPath, encoder_path, "nscrt.dll", true);
                if (!File.Exists(Path.Combine(encoder_path, "libmp4v2.dll")))
                    FileUtil.CopyFile(MeGUISettings.WinampPath, encoder_path, "libmp4v2.dll", true);

                script.Append("32==Audiobits(last)?ConvertAudioTo16bit(last):last" + Environment.NewLine);  // winamp aac encoder doesn't support 32bits streams
                StringBuilder sb = new StringBuilder("- \"{0}\" --rawpcm {1} {3} {2}");

                sb.Append(" --br " + n.Bitrate * 1000);

                if (n.Mpeg2AAC)
                    sb.Append(" --mpeg2aac");
                else
                    sb.Append(" --mpeg4aac");

                switch (n.Profile)
                {
                    case AacProfile.PS:
                        sb.Append(" --ps");
                        break;
                    case AacProfile.HE:
                        sb.Append(" --he");
                        break;
                    case AacProfile.LC:
                        sb.Append(" --lc");
                        break;
                    case AacProfile.HIGH:
                        sb.Append(" --high");
                        break;
                }
                switch (n.StereoMode)
                {
                    case WinAmpAACSettings.AacStereoMode.Dual:
                        sb.Append(" --dc");
                        break;
                    case WinAmpAACSettings.AacStereoMode.Joint:
                        break;
                    case WinAmpAACSettings.AacStereoMode.Independent:
                        sb.Append(" --is");
                        break;

                }
                _encoderCommandLine = sb.ToString();
            }

            if (audioJob.Settings is AudXSettings)
            {
                script.Append("ResampleAudio(last,48000)" + Environment.NewLine);
                script.Append("32==Audiobits(last)?ConvertAudioTo16bit(last):last" + Environment.NewLine);  // audX encoder doesn't support 32bits streams
                script.Append("6==Audiochannels(last)?last:GetChannel(last,1,1,1,1,1,1)" + Environment.NewLine);
                _mustSendWavHeaderToEncoderStdIn = false;
                AudXSettings n = audioJob.Settings as AudXSettings;
                _encoderExecutablePath = this._settings.EncAudXPath;
                _encoderCommandLine = "- \"{0}\" --q " + ((int)n.Quality) + " --raw {1}";
            }
            if (audioJob.Settings is OggVorbisSettings)
            {
                // http://forum.doom9.org/showthread.php?p=831098#post831098
                script.Append("6==Audiochannels(last)?GetChannel(last,1,3,2,5,6,4):last" + Environment.NewLine);
                _mustSendWavHeaderToEncoderStdIn = true;
                OggVorbisSettings n = audioJob.Settings as OggVorbisSettings;
                _encoderExecutablePath = this._settings.OggEnc2Path;
                _encoderCommandLine = "-Q --ignorelength --quality " + n.Quality.ToString(System.Globalization.CultureInfo.InvariantCulture) + " -o \"{0}\" -";
            }
            if (audioJob.Settings is NeroAACSettings)
            {
                _mustSendWavHeaderToEncoderStdIn = true;
                NeroAACSettings n = audioJob.Settings as NeroAACSettings;
                NeroAACSettings nas = n;
                _encoderExecutablePath = this._settings.NeroAacEncPath;
                StringBuilder sb = new StringBuilder("-ignorelength ");
                switch (n.Profile)
                {
                    case AacProfile.HE:
                        sb.Append("-he ");
                        break;
                    case AacProfile.PS:
                        sb.Append("-hev2 ");
                        break;
                    case AacProfile.LC:
                        sb.Append("-lc ");
                        break;
                }

                switch (n.BitrateMode)
                {
                    case BitrateManagementMode.ABR:
                        sb.AppendFormat(System.Globalization.CultureInfo.InvariantCulture, "-br {0} ", n.Bitrate*1000);
                        break;
                    case BitrateManagementMode.CBR:
                        sb.AppendFormat(System.Globalization.CultureInfo.InvariantCulture, "-cbr {0} ", n.Bitrate*1000);
                        break;
                    case BitrateManagementMode.VBR:
                        sb.AppendFormat(System.Globalization.CultureInfo.InvariantCulture, "-q {0} ", n.Quality);
                        break;
                }

                sb.Append("-if - -of \"{0}\"");

                _encoderCommandLine = sb.ToString();
            }
            if (audioJob.Settings is FaacSettings)
            {
                FaacSettings f = audioJob.Settings as FaacSettings;
                _encoderExecutablePath = this._settings.FaacPath;
                _mustSendWavHeaderToEncoderStdIn = true;
                switch (f.BitrateMode)
                {
                    case BitrateManagementMode.VBR:
                        _encoderCommandLine = "-q " + f.Quality + " --mpeg-vers 4 -o \"{0}\" -"; 
                        break;
                    default:
                        _encoderCommandLine = "-b " + f.Bitrate + " --mpeg-vers 4 -o \"{0}\" -";
                        break;
                }
            }
            if (audioJob.Settings is MP3Settings)
            {
                MP3Settings m = audioJob.Settings as MP3Settings;
                _mustSendWavHeaderToEncoderStdIn = true;
                _encoderExecutablePath = this._settings.LamePath;
                script.Append("32==Audiobits(last)?ConvertAudioTo16bit(last):last" + Environment.NewLine); // lame encoder doesn't support 32bits streams

                switch (m.BitrateMode)
                {
                    case BitrateManagementMode.VBR:
                        _encoderCommandLine = "-V" + (100 - m.Quality) / 10 + " - \"{0}\"";
                        break;
                    case BitrateManagementMode.CBR:
                        _encoderCommandLine = "-b " + m.Bitrate + " --cbr -h - \"{0}\"";
                        break;
                    case BitrateManagementMode.ABR:
                        _encoderCommandLine = "--abr " + m.Bitrate + " -h - \"{0}\"";
                        break;
                }
            }

            //Just check encoder existance
            _encoderExecutablePath = Path.Combine(AppDomain.CurrentDomain.SetupInformation.ApplicationBase, _encoderExecutablePath);
            if (!File.Exists(_encoderExecutablePath))
            {
                deleteTempFiles();             
                throw new EncoderMissingException(_encoderExecutablePath);
            }

            script.AppendLine(Environment.NewLine);
            script.AppendLine(@"return last");
            script.AppendLine(Environment.NewLine);

            // copy the appropriate function at the end of the script
            switch (audioJob.Settings.DownmixMode)
            {
                case ChannelMode.KeepOriginal:
                    break;
                case ChannelMode.ConvertToMono:
                    break;
                case ChannelMode.DPLDownmix:
                    script.AppendLine(@"
function x_dpl" + id + @"(clip a) 
  {
     fl = GetChannel(a, 1)
     fr = GetChannel(a, 2)
     c = GetChannel(a, 3)
     sl = GetChannel(a, 5)
     sr = GetChannel(a, 6)
     ssr = MixAudio(sl, sr, 0.2222, 0.2222)
     ssl = Amplify(ssr, -1.0)
     fl_c = MixAudio(fl, c, 0.3254, 0.2301)
     fr_c = MixAudio(fr, c, 0.3254, 0.2301)
     l = MixAudio(ssl, fl_c, 1.0, 1.0)
     r = MixAudio(ssr, fr_c, 1.0, 1.0)
     return MergeChannels(l, r)
  }");
                    break;
                case ChannelMode.DPLIIDownmix:
                    script.AppendLine(@"
function x_dpl2" + id + @"(clip a) 
  {
     fl = GetChannel(a, 1)
     fr = GetChannel(a, 2)
     c = GetChannel(a, 3)
     sl = GetChannel(a, 5)
     sr = GetChannel(a, 6)
     ssl = MixAudio(sl, sr, 0.2818, 0.1627).Amplify(-1.0)
     fl_c = MixAudio(fl, c, 0.3254, 0.2301)
     ssr = MixAudio(sl, sr, 0.1627, 0.2818)
     fr_c = MixAudio(fr, c, 0.3254, 0.2301)
     l = MixAudio(ssl, fl_c, 1.0, 1.0)
     r = MixAudio(ssr, fr_c, 1.0, 1.0)
     return MergeChannels(l, r)
  }");
                    break;
                case ChannelMode.StereoDownmix:
                    script.AppendLine(@"
function x_stereo" + id + @"(clip a) 
  {
     fl = GetChannel(a, 1)
     fr = GetChannel(a, 2)
     c = GetChannel(a, 3)
     lfe = GetChannel(a, 4)
     sl = GetChannel(a, 5)
     sr = GetChannel(a, 6)
     l_sl = MixAudio(fl, sl, 0.2929, 0.2929)
     c_lfe = MixAudio(lfe, c, 0.2071, 0.2071)
     r_sr = MixAudio(fr, sr, 0.2929, 0.2929)
     l = MixAudio(l_sl, c_lfe, 1.0, 1.0)
     r = MixAudio(r_sr, c_lfe, 1.0, 1.0)
     return MergeChannels(l, r)
  }");
                    break;
                case ChannelMode.Upmix:
                    script.AppendLine(@"
function x_upmix" + id + @"(clip a) 
 {
    m = ConvertToMono(a)
    a1 = GetLeftChannel(a)
    a2 = GetRightChannel(a)
    fl = SuperEQ(a1,""" + tmp + @"front.feq"")
    fr = SuperEQ(a2,""" + tmp + @"front.feq"")
    c = SuperEQ(m,""" + tmp + @"center.feq"") 
    lfe = SuperEQ(m,""" + tmp + @"lfe.feq"") 
    sl = SuperEQ(a1,""" + tmp + @"back.feq"")
    sr = SuperEQ(a2,""" + tmp + @"back.feq"")
    return MergeChannels(fl,fr,c,lfe,sl,sr)
 }");
                    break;
                case ChannelMode.UpmixUsingSoxEq:
                    script.AppendLine(@"
function x_upmixR" + id + @"(clip Stereo) 
 {
    Front = mixaudio(Stereo.soxfilter(""filter 0-600""),mixaudio(Stereo.soxfilter(""filter 600-1200""),Stereo.soxfilter(""filter 1200-7000""),0.45,0.25),0.50,1)
    Back = mixaudio(Stereo.soxfilter(""filter 0-600""),mixaudio(Stereo.soxfilter(""filter 600-1200""),Stereo.soxfilter(""filter 1200-7000""),0.35,0.15),0.40,1)
    fl = GetLeftChannel(Front)
    fr = GetRightChannel(Front)
    cc = ConvertToMono(stereo).SoxFilter(""filter 625-24000"")
    lfe = ConvertToMono(stereo).SoxFilter(""lowpass 100"",""vol -0.5"")
    sl = GetLeftChannel(Back)
    sr = GetRightChannel(Back)
    sl = DelayAudio(sl,0.02)
    sr = DelayAudio(sr,0.02)
    return MergeChannels(fl,fr,cc,lfe,sl,sr)
 }");
                    break;
                case ChannelMode.UpmixWithCenterChannelDialog:
                    script.AppendLine(@"
function x_upmixC" + id + @"(clip stereo) 
 {
    left = stereo.GetLeftChannel()
    right = stereo.GetRightChannel()
    fl = mixaudio(left.soxfilter(""filter 0-24000""),right.soxfilter(""filter 0-24000""),0.6,-0.5)
    fr = mixaudio(right.soxfilter(""filter 0-24000""),left.soxfilter(""filter 0-24000""),0.6,-0.5)
    cc = ConvertToMono(stereo).SoxFilter(""filter 625-24000"")
    lfe = ConvertToMono(stereo).SoxFilter(""lowpass 100"",""vol -0.5"")
    sl = mixaudio(left.soxfilter(""filter 0-24000""),right.soxfilter(""filter 0-24000""),0.5,-0.4)
    sr = mixaudio(right.soxfilter(""filter 0-24000""),left.soxfilter(""filter 0-24000""),0.5,-0.4)
    sl = DelayAudio(sl,0.02)
    sr = DelayAudio(sr,0.02)
     return MergeChannels(fl,fr,cc,lfe,sl,sr)                                                                                                                                              
 }");
                    break;
            }

            _avisynthAudioScript = script.ToString();

            _log.LogValue("Avisynth script", _avisynthAudioScript);
            _log.LogValue("Commandline used", _encoderCommandLine);
        }

        public void start()
        {
            try
            {
                this.Start();
            }
            catch (Exception e)
            {
                throw new JobRunException(e);
            }
        }

        public void stop()
        {
            try
            {
                this.Abort();
            }
            catch (Exception e)
            {
                throw new JobRunException(e);
            }
        }

        public void pause()
        {
            if (!_mre.Reset())
                throw new JobRunException("Could not reset mutex. pause failed");
        }

        public void resume()
        {
            if (!_mre.Set())
                throw new JobRunException("Could not set mutex. pause failed");
        }

        public void changePriority(ProcessPriority priority)
        {
            if (this._encoderThread != null && _encoderThread.IsAlive)
            {
                try
                {
                    switch (priority)
					{
					    case ProcessPriority.IDLE:
							_encoderThread.Priority = ThreadPriority.Lowest;
							break;
						case ProcessPriority.BELOW_NORMAL:
							_encoderThread.Priority = ThreadPriority.BelowNormal;
							break;
						case ProcessPriority.NORMAL:
							_encoderThread.Priority = ThreadPriority.Normal;
							break;
						case ProcessPriority.ABOVE_NORMAL:
							_encoderThread.Priority = ThreadPriority.AboveNormal;
							break;
						case ProcessPriority.HIGH:
							_encoderThread.Priority = ThreadPriority.Highest;
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
                if (_encoderThread == null)
                    throw new JobRunException("Thread has not been started yet");
                else
                    throw new JobRunException("Thread has exited");
            }
        }

        public event JobProcessingStatusUpdateCallback StatusUpdate;
        #endregion
    }
}
