// ****************************************************************************
// 
// Copyright (C) 2005-2009  Doom9 & al
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// 
// ****************************************************************************

using System;
using System.Collections.Generic;
using System.Text;

namespace MeGUI
{
    public class CommandlineParser
    {
        public Dictionary<string, string> upgradeData = new Dictionary<string, string>();
        public List<string> filesToInstall = new List<string>();
        public List<string> failedUpgrades = new List<string>();
        public bool start = true;

        public bool Parse(string[] commandline)
        {
            for (int i = 0; i < commandline.Length; i++)
            {
                if (commandline[i] == "--upgraded")
                {
                    if (commandline.Length > i + 2)
                    {
                        upgradeData.Add(commandline[i + 1], commandline[i + 2]);
                        i += 2;
                    }
                    else
                        return false;
                }
                else if (commandline[i] == "--install")
                {
                    if (commandline.Length > i + 1)
                    {
                        filesToInstall.Add(commandline[i + 1]);
                        i++;
                    }
                    else return false;
                }
                else if (commandline[i] == "--upgrade-failed")
                {
                    if (commandline.Length > i + 1)
                    {
                        failedUpgrades.Add(commandline[i + 1]);
                        i++;
                    }
                    else
                        return false;
                }
                else if (commandline[i] == "--dont-start")
                {
                    start = false;
                }
                else
                    return false;
            }
            return true;
        }
    }
}
