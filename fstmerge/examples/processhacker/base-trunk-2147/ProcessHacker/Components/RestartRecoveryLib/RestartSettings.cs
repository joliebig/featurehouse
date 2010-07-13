

using System;

namespace ProcessHackerRestartRecovery
{





    public class RestartSettings
    {
        private string command;
        private RestartRestrictions restrictions;
        public RestartSettings(string commandLine, RestartRestrictions restrict)
        {
            command = commandLine;
            restrictions = restrict;
        }
        public string Command
        {
            get { return command; }
        }
        public RestartRestrictions Restrictions
        {
            get { return restrictions; }
        }
        public override string ToString()
        {
            return String.Format("Command: {0} Restrictions: {1}", command, restrictions.ToString());
        }
    }
}
