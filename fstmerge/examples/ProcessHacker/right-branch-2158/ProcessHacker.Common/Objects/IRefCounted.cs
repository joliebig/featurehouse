using System;

namespace ProcessHacker.Common.Objects
{
    public interface IRefCounted : IDisposable
    {




        int Dereference();






        int Dereference(bool managed);






        int Dereference(int count);







        int Dereference(int count, bool managed);






        void Dispose(bool managed);





        int Reference();






        int Reference(int count);
    }
}
