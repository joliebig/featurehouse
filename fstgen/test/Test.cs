class CleanupJobRunner : IJobProcessor
    {
        public static JobProcessorFactory Factory = new JobProcessorFactory(new ProcessorFactory(
            delegate(MainForm f, Job j)
            {
                if (j is CleanupJob)
                    return new CleanupJobRunner(f);
                return null;
            }), "cleanup");
}
class X {

}

struct Y {

}

interface Z {}

delegate void C(int k);

enum P {}