      private class FreeListEntry<U>
            where U : IResettable2, new()
        {
            public U Object;
            public FreeListEntry<U> Next;
            private List<KeyValuePair<ulong, string> > _modules = new List<KeyValuePair<ulong, string> >();
        }