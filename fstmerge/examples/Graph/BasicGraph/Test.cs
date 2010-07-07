      private class FreeListEntry<U>
            where U : IResettable, new()
        {
            public U Object;
            public FreeListEntry<U> Next;
            private List<KeyValuePair<ulong, string> > _modules = new List<KeyValuePair<ulong, string> >();
        }