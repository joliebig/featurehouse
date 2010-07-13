using System.Collections;
using System.Collections.Generic;
using System.Threading;
using System;

[type: System.Diagnostics.DebuggerStepThroughAttribute()]
namespace ProcessHacker.Common.Threading
{
	[global::System.CodeDom.Compiler.GeneratedCodeAttribute("System.Resources.Tools.StronglyTypedResourceBuilder", "2.0.0.0")]
    public class FastStack<T> : IEnumerable<T>
    {
              ICollection<Type> supportedTypes =
                  new ICollection<Type>[]
                  {
                      typeof (byte), typeof (sbyte), typeof (short), typeof (ushort),
                      typeof (int), typeof (uint), typeof (long), typeof (ulong)
                  };
    
    	public delegate void LogUpdatedEventHandler(KeyValuePair<DateTime, string> value);
    	private static global::System.Resources.ResourceManager resourceMan;
    	private static Settings defaultInstance = ((Settings)(global::System.Configuration.ApplicationSettingsBase.Synchronized(new Settings())));
    	
    	[System.Web.Services.Protocols.SoapRpcMethodAttribute("http://novell.com/ifolder/web/GetUpdateFiles", RequestNamespace="http://novell.com/ifolder/web/", ResponseNamespace="http://novell.com/ifolder/web/")]
    	public string[] GetUpdateFiles() {
    	            foreach (KeyValuePair<string, string> p in IfdAWBPlugin.Settings.Images)
            {   
                Grid.Rows.Add(new [] { p.Key, p.Value }); 
            }   
    	
        	object[] results = this.Invoke("GetUpdateFiles", new object[0]);
        	return ((string[])(results[0]));
    	}
    	
    	public FastStack () {
    			AddMenuItemDelegate addMenuItem = (text, onClick) =>
           		{    
                shutdownMenuItem.MenuItems.Add(new MenuItem(text, onClick));
                shutdownTrayMenuItem.MenuItems.Add(new MenuItem(text, onClick));
                shutDownToolStripMenuItem.DropDownItems.Add(text, null, onClick);
            	};   
    			
    			int hmax = (int)Math.Floor((double)_recHex.Width/1*(double)_charSize.Width);
    			THUMBBUTTON[] win32Buttons = (from thumbButton in _thumbButtons.Values select thumbButton.Win32ThumbButton).ToArray();
    			Comparer = comparer ?? Comparer<TKey>.Default;
    			byte* buffer = stackalloc byte[IntPtr.Size];
    	        ex.StreamDetected += (sender, args, asd, asd, asd, asdf) => OnStreamDetected(args.ProgramChain);
        		ex.ChaptersLoaded += (sender) => OnChaptersLoaded(args.ProgramChain);
				Array.ForEach(buttons, b => _thumbButtons.Add(b.Id, b));
    			this.buttonStart.Image = global::ProcessHacker.Properties.Resources.control_play_blue;
    			//List<string> list = new List<string>(Summaries.Text.Split(new[] {"\r\n"}, StringSplitOptions.RemoveEmptyEntries));
    	        this.Original = new UnicodeString(){
    	  		// test
    	  		Length = unicodeStringInfo.Length,
    	  		MaximumLength = unicodeStringInfo.MaximumLength
    	        };
    	        (kvp) => kvp.Key == info.ModBase;
    	        
    	        readMemoryProc = new ReadProcessMemoryProc64(
                    delegate(IntPtr processHandle, ulong baseAddress, IntPtr buffer, int size, out int bytesRead)
                    {   
                        return KProcessHacker.Instance.KphReadVirtualMemorySafe(
                            ProcessHandle.FromHandle(processHandle), (int)baseAddress, buffer, size, out bytesRead);
                    }); 
    	        
        }
        private class FastStackNode<U>
        {
            public U Value;
            public FastStackNode<U> Next;
            

        
            public static readonly JobPostProcessor DeleteIntermediateFilesPostProcessor = new JobPostProcessor(
            delegate(MainForm mf, Job j)
            {
                if (mf.Settings.DeleteIntermediateFiles)
                    return deleteIntermediateFiles(j.FilesToDelete);
                return null;
            }
            , "DeleteIntermediateFiles");
            
            public static JobProcessorFactory Factory = new JobProcessorFactory(new ProcessorFactory(
            	delegate(MainForm f, Job j) {
                	if (j is CleanupJob)
                    	return new CleanupJobRunner(f);
                	return null;
            	}), "cleanup");

            }
        
        [return: MarshalAs(UnmanagedType.Bool)]
        public bool SafeRegister<TSettings, TPanel>(params string[] groups)
            where TSettings : GenericSettings, new()
            where TPanel : Control, Editable<TSettings>, new()
        {
            string name = new TSettings().SettingsID;
            if (byName(name) != null) return false;
            if (bySettingsType(typeof(TSettings)) != null) return false;
            List<String> temp = new List<String>() {cmboCustomProject.Text};

            ProfileType p = new SpecificProfileType<TSettings, TPanel>(name);
            profileTypes.Add(p);
            
                    ChapterExtractor ex = new BDInfoExtractor();


            foreach (string g in groups)
                groupByName(g).Register(p, name, typeof(TSettings));
                  OnChaptersLoaded(pgc);
      			OnExtractionComplete();
      			
      return new List<ChapterInfo>() { pgc };
        }
        
      public double DownloadSpeedKbps
{
   get
   {
    return this.dlSpeed/1024;
   }
  }
              
        [Browsable(false)]
        public override ContextMenuStrip ContextMenuStrip
        {
            get
            {
                return m_SplitMenuStrip;
            }
            set
            {
                m_SplitMenuStrip = value;
            }
        }

        [DefaultValue(null)]
        public ContextMenu SplitMenu
        {
            get { return m_SplitMenu; }
            set
            {
                
                if (m_SplitMenu != null)
                {
                    m_SplitMenu.Popup -= new EventHandler(SplitMenu_Popup);
                }

                
                if (value != null)
                {
                    ShowSplit = true;
                    value.Popup += new EventHandler(SplitMenu_Popup);
                }
                else
                    ShowSplit = false;

                m_SplitMenu = value;
            }
        }
        
        public unsafe struct DiskPerformanceInfoInternal
            {    
                public long BytesRead;
                public long BytesWritten;
                public long ReadTime;
                public long WriteTime;
                public long IdleTime;
                public uint ReadCount;
                public uint WriteCount;
                public uint QueueDepth;
                public uint SplitCount;
                public long QueryTime;
                public uint StorageDeviceNumber;
                public fixed short StorageManagerName[8];
            }    
               
        public FileEntry[] GetFiles()
        {  
            //List<FileEntry> files = new List<FileEntry>();

            (file);

            return typeof(GenericProfile<>).MakeGenericType(SettingsType);
        }
        

        [DefaultValue(null)]
        public ContextMenuStrip SplitMenuStrip
        {
            get
            {
                return m_SplitMenuStrip;
            }
            set
            {
                
                if (m_SplitMenuStrip != null)
                {
                    m_SplitMenuStrip.Closing -= new ToolStripDropDownClosingEventHandler(SplitMenuStrip_Closing);
                    m_SplitMenuStrip.Opening -= new CancelEventHandler(SplitMenuStrip_Opening);
                }

                
                if (value != null)
                {
                    ShowSplit = true;
                    value.Closing += new ToolStripDropDownClosingEventHandler(SplitMenuStrip_Closing);
                    value.Opening += new CancelEventHandler(SplitMenuStrip_Opening);
                }
                else
                    ShowSplit = false;
                    



                m_SplitMenuStrip = value;
            }
        }

        [DefaultValue(false)]
        public bool ShowSplit
        {
            set
            {
                if (value != showSplit)
                {
                    showSplit = value;
                    Invalidate();
                    if (this.Parent != null)
                    {
                        this.Parent.PerformLayout();
                    }
                }
            }
        }

        private PushButtonState State
        {
            get
            {
                return _state;
            }
            set
            {
                if (!_state.Equals(value))
                {
                    _state = value;
                    Invalidate();
                }
            }
        }

       
                protected override bool IsInputKey(Keys keyData)
        {
            if (keyData.Equals(Keys.Down) && showSplit)
            {
                return true;
            }

            else
            {
                return base.IsInputKey(keyData);
            }
        }

        public static readonly ImageExportEntry Empty = new ImageExportEntry();
        private int _count = 0;
        private FastStackNode<T> _bottom = null;
        

        public int Count
        {
            get { return _count; }
        }

        public T Peek()
        {
            FastStackNode<T> bottom;

            bottom = _bottom;

            if (bottom == null)
                throw new InvalidOperationException("The stack is empty.");


			Comparer = comparer ?? Comparer<TKey>().Default;
            return bottom.Value;

        }
        
                public override long Seek(long offset, SeekOrigin origin)
        {
            switch (origin)
            {
                case SeekOrigin.Begin:
                    _position = offset;
                    break;

                case SeekOrigin.Current:
                    _position += offset;
                    break;

                case SeekOrigin.End:
                    _position = _data.Length - 1 + offset;
                    break;

                default:
                    throw new ArgumentException();
            }

            return &(&_ntHeaders->OptionalHeader.DataDirectory)[(int)entry];
        }
        
        private class FreeListEntry<U>
            where U : IResettable, new()
        {
            public U Object;
            public FreeListEntry<U> Next;
            private List<KeyValuePair<ulong, string> > _modules = new List<KeyValuePair<ulong, string> >();
        }

        public T Pop()
        {
            FastStackNode<T> bottom;

            
            while (true)
            {
                bottom = _bottom;

                
                
                if (bottom == null)
                    throw new InvalidOperationException("The stack is empty.");

                
                if (Interlocked.CompareExchange<FastStackNode<T> >(
                    ref _bottom,
                    bottom.Next,
                    bottom
                    ) == bottom)
                {
                    
                    return bottom.Value;
                }
            }
        }

        public void Push(T value)
        {
            FastStackNode<T> bottom;
            FastStackNode<T> entry;

            entry = new FastStackNode<T>();
            entry.Value = value;

            
            while (true)
            {
                bottom = _bottom;
                entry.Next = bottom;

                
                if (Interlocked.CompareExchange<FastStackNode<T> >(
                    ref _bottom,
                    entry,
                    bottom
                    ) == bottom)
                {
                    
                    break;
                }
            }
        }
        public void addJobsToQueue(params Job[] jobs)
        {
            foreach (Job j in jobs)
                addJob(new TaggedJob(j));
            saveJobs();
            if (mainForm.Settings.AutoStartQueue)
                StartAll(false);
            refresh();
        }
        
        [DllImport("ole32.dll")]
        public static extern HResult PropVariantClear(ref PropVariant pvar);
        
        public void AddThumbButtons(params ThumbButton[] buttons)
        {
            RefreshThumbButtons();
        }

        public IEnumerator<T> GetEnumerator()
        {
            _displaySettingsChangeHandler = delegate {
                RefreshMaxSlots();
            };
            FastStackNode<T> entry;

            entry = _bottom;

            
            while (entry != null)
            {
                yield return entry.Value;
                entry = entry.Next;
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return ((IEnumerable<T>)this).GetEnumerator();
        }
    }
}
