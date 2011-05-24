

using System;
using System.Collections.Generic;
using System.Text;

namespace ProcessHacker
{



    public enum SearchType
    {
        Literal, Regex, String, Heap, Struct
    }




    public class SearchOptions
    {
        private int _pid;
        private SearchType _type = SearchType.Literal;
        private Searcher _searcher;






        public SearchOptions(int PID, SearchType type)
        {
            _pid = PID;

            _searcher = new Searcher(_pid);


            _searcher.Params.Add("text", new byte[0]);
            _searcher.Params.Add("regex", "");
            _searcher.Params.Add("s_ms", "10");
            _searcher.Params.Add("unicode", true);
            _searcher.Params.Add("h_ms", "1024");
            _searcher.Params.Add("nooverlap", true);
            _searcher.Params.Add("ignorecase", false);
            _searcher.Params.Add("private", true);
            _searcher.Params.Add("image", false);
            _searcher.Params.Add("mapped", false);
            _searcher.Params.Add("struct", "");
            _searcher.Params.Add("struct_align", "4");

            Type = type;
        }




        public int PID
        {
            get { return _pid; }
        }




        public SearchType Type
        {
            get { return _type; }
            set
            {
                _type = value;

                Dictionary<string, object> oldparams = _searcher.Params;
                List<string[]> oldresults = _searcher.Results;

                switch (_type)
                {
                    case SearchType.Literal:
                        _searcher = new LiteralSearcher(PID);
                        break;

                    case SearchType.Regex:
                        _searcher = new RegexSearcher(PID);
                        break;

                    case SearchType.String:
                        _searcher = new StringSearcher(PID);
                        break;

                    case SearchType.Heap:
                        _searcher = new HeapSearcher(PID);
                        break;

                    case SearchType.Struct:
                        _searcher = new StructSearcher(PID);
                        break;

                    default:
                        _searcher = new Searcher(PID);
                        break;
                }

                foreach (string s in oldparams.Keys)
                {
                    _searcher.Params.Add(s, oldparams[s]);
                }

                _searcher.Results = oldresults;
            }
        }




        public Searcher Searcher
        {
            get { return _searcher; }
        }
    }
}
