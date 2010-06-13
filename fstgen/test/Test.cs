class X {
 private void addSendToWorkerMenuItems()
        {
            jobQueue.AddDynamicSubMenu("Send to worker", null,
                new MultiJobMenuGenerator(delegate
            {
                Pair<string, MultiJobHandler>[] list = new Pair<string, MultiJobHandler>[workers.Count];
                int i = 0;
                foreach (JobWorker w in workers.Values)
                {
                    list[i] = new Pair<string, MultiJobHandler>(w.Name,
                        new MultiJobHandler((new SendToWorkerThunk(w, this)).handleEvent));
                    i++;
                }
                return list;
            }));
        }
}

struct Y {

}

interface Z {}

delegate void C(int k);

enum P {}