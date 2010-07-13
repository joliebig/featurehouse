

using System;
using System.Collections.Generic;
using System.Text;
using ProcessHacker.Native.Api;
using ProcessHacker.Native.Objects;
using ProcessHacker.Native.Security;

namespace ProcessHacker.Native.Threading
{



    public sealed class Mutant : NativeObject<MutantHandle>
    {



        public Mutant()
            : this(null)
        { }
        public Mutant(bool owned)
            : this(null, owned)
        { }
        public Mutant(string name)
            : this(name, false)
        { }
        public Mutant(string name, bool owned)
        {
            this.Handle = MutantHandle.Create(
                MutantAccess.All,
                name,
                ObjectFlags.OpenIf,
                null,
                owned
                );
        }
        public bool Owned
        {
            get { return this.Handle.GetBasicInformation().CurrentCount <= 0; }
        }
        public void Release()
        {
            this.Handle.Release();
        }
    }
}
