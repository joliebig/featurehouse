

using System;

namespace ProcessHackerRestartRecovery
{







    public class RecoverySettings
    {
        private RecoveryData recoveryData;
        private uint pingInterval;
        public RecoverySettings(RecoveryData data, uint interval)
        {
            this.recoveryData = data;
            this.pingInterval = interval;
        }
        public RecoveryData RecoveryData
        {
            get { return recoveryData; }
        }
        public uint PingInterval
        {
            get { return pingInterval; }
        }
        public override string ToString()
        {
            return String.Format("delegate: {0}, state: {1}, ping: {2}",
                this.recoveryData.Callback.Method.ToString(),
                this.recoveryData.State.ToString(),
                this.PingInterval);
        }
    }
}
