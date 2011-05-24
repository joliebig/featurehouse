

using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace ProcessHacker
{
    public delegate void SearchFinished();
    public delegate void SearchProgressChanged(string progress);
    public delegate void SearchError(string message);




    public interface ISearcher
    {
        event SearchFinished SearchFinished;
        event SearchProgressChanged SearchProgressChanged;
        event SearchError SearchError;
        int PID { get; }
        Dictionary<string, object> Params { get; }
        List<string[]> Results { get; }
        void Search();
    }




    public class Searcher : ISearcher
    {
        private int _pid;
        private Dictionary<string, object> _params;
        private List<string[]> _results;

        public event SearchFinished SearchFinished;
        public event SearchProgressChanged SearchProgressChanged;
        public event SearchError SearchError;





        public Searcher(int PID)
        {
            _pid = PID;
            _params = new Dictionary<string, object>();
            _results = new List<string[]>();
        }




        public int PID
        {
            get { return _pid; }
        }




        public Dictionary<string, object> Params
        {
            get { return _params; }
        }




        public List<string[]> Results
        {
            get { return _results; }
            set { _results = value; }
        }




        public virtual void Search()
        {
        }

        protected void CallSearchFinished()
        {
            if (SearchFinished != null)
                SearchFinished();
        }

        protected void CallSearchProgressChanged(string progress)
        {
            if (SearchProgressChanged != null)
                SearchProgressChanged(progress);
        }

        protected void CallSearchError(string message)
        {
            if (SearchError != null)
                SearchError(message);
        }
    }
}
