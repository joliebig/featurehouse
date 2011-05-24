

using System;
using System.Collections.Generic;

namespace ProcessHacker.Common
{



    public class IdGenerator
    {
        private int _step = 1;
        private bool _sort = false;
        private List<int> _ids = new List<int>();
        private int _id;




        public IdGenerator()
            : this(0)
        { }





        public IdGenerator(int start)
            : this(start, 1)
        { }






        public IdGenerator(int start, int step)
        {
            if (step == 0)
                throw new ArgumentException("step cannot be zero.");

            _id = start;
            _step = step;
        }

        public bool Sort
        {
            get { return _sort; }
            set { _sort = value; }
        }





        public int Pop()
        {
            int id;

            lock (_ids)
            {
                if (_ids.Count > 0)
                {
                    id = _ids[0];

                    _ids.Remove(_ids[0]);

                    return id;
                }
                else
                {
                    id = _id;
                    _id += _step;
                }
            }

            return id;
        }





        public void Push(int id)
        {
            lock (_ids)
            {
                _ids.Add(id);

                if (_sort)
                    _ids.Sort();
            }
        }
    }
}
